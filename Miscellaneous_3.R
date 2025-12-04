############################################################
# Dominant-UOM Price Homogeneity & Rauch Classification
# Production Version (v5)
#
# Pipeline:
#   1. Compute price dispersion metrics by HS6-year
#   2. Test for divergence between equal-weight and value-weight measures
#   3. Classify goods as homogeneous/reference/differentiated
#
# Author: [Your Name]
# Last Updated: [Date]
############################################################

suppressPackageStartupMessages({
  library(data.table)
  library(mclust)
})

# =============================================================================
# USER CONFIGURATION
# =============================================================================

config <- list(
  
  # --- Homogeneity pipeline parameters ---
  min_exporters = 10,        # Minimum exporters required for reliable estimates

  min_coverage  = 0.85,      # Minimum value share in dominant UOM
  trim_tail     = 0.01,      # Trim extreme prices (0.01 = drop top/bottom 1%)
  min_Q_exporter = 0,        # Minimum quantity per exporter (0 = no filter)
  min_V_exporter = 0,        # Minimum value per exporter (0 = no filter)
  

  # --- Divergence testing parameters ---
  R_boot = 500,              # Bootstrap replications for CI
  R_perm = 500,              # Permutation replications for p-value
  fdr_level = 0.10,          # False discovery rate for flagging
  min_years_for_hs6 = 3,     # Minimum years for HS6-level inference
  
  # --- Rauch classification parameters ---
  n_components = 3,          # Max GMM components (2 or 3)
  min_years_classify = 3,    # Minimum years for classification
  instability_override_quantile = 0.90,  # CV threshold for stability override
  use_2d = TRUE              # Use 2D GMM (dispersion + stability) if possible
)

# =============================================================================
# DATA INPUT SPECIFICATION
# =============================================================================

#' Required columns in your input data:
#' 
#' | Column   | Type      | Description                                    |
#' |----------|-----------|------------------------------------------------|
#' | year     | integer   | Year of observation                            |
#' | exporter | character | Exporter country code (e.g., "USA", "CHN")     |
#' | importer | character | Importer country code                          |
#' | hs6      | character | 6-digit HS code (as string to preserve zeros)  |
#' | value    | numeric   | Trade value (e.g., USD)                        |
#' | quantity | numeric   | Trade quantity                                 |
#' | uom      | character | Unit of measure (e.g., "kg", "item", "litre")  |
#'
#' Example:
#'   year exporter importer    hs6      value  quantity   uom
#'   2022      CHN      USA 847130 1250000.00    5000.0    kg
#'   2022      DEU      USA 847130  890000.00    3200.0    kg
#'   2022      JPN      USA 300490  450000.00     120.5  item


# =============================================================================
# DATA LOADING FUNCTION (CUSTOMIZE THIS)
# =============================================================================

load_trade_data <- function(paths = NULL, id_col = NULL) {
#
# Load and combine trade data from one or more files
#
# Arguments:
#   paths:   Character vector of file paths (CSV, RDS, Parquet, or Stata)
#            Can also be a single path or a directory path
#   id_col:  Optional name for source identifier column (e.g., "source_file")
#            If provided, adds a column indicating which file each row came from
#
# Returns:
#   Combined data.table with all observations
#
# Examples:
#   # Single file
#   dt <- load_trade_data("data/trade_2023.csv")
#
#   # Multiple files explicitly
#   dt <- load_trade_data(c("data/trade_2022.csv", 
#                           "data/trade_2023.csv", 
#                           "data/trade_2024.csv"))
#
#   # All CSVs in a directory
#   dt <- load_trade_data("data/annual/")
#
#   # With source tracking
#   dt <- load_trade_data(c("comtrade.csv", "census.csv"), id_col = "source")
#
  
  if (is.null(paths)) {
    stop("Please provide file path(s) or modify load_trade_data() to load your data")
  }
  
  # If a directory is provided, find all supported files
 if (length(paths) == 1 && dir.exists(paths)) {
    dir_path <- paths
    paths <- list.files(
      dir_path, 
      pattern = "\\.(csv|rds|parquet|dta)$", 
      full.names = TRUE, 
      ignore.case = TRUE
    )
    if (length(paths) == 0) {
      stop("No supported files found in directory: ", dir_path)
    }
    cat("Found", length(paths), "files in", dir_path, "\n")
  }
  
  # Function to load a single file
  load_single <- function(path) {
    
    if (!file.exists(path)) {
      stop("File not found: ", path)
    }
    
    ext <- tolower(tools::file_ext(path))
    
    dt <- switch(ext,
      "csv" = fread(path, colClasses = c(hs6 = "character")),
      "rds" = as.data.table(readRDS(path)),
      "parquet" = {
        if (!requireNamespace("arrow", quietly = TRUE)) {
          stop("Package 'arrow' required to read parquet files")
        }
        as.data.table(arrow::read_parquet(path))
      },
      "dta" = {
        if (!requireNamespace("haven", quietly = TRUE)) {
          stop("Package 'haven' required to read Stata files")
        }
        as.data.table(haven::read_dta(path))
      },
      stop("Unsupported file format: ", ext, " (file: ", path, ")")
    )
    
    # Ensure hs6 is character
    if ("hs6" %in% names(dt)) {
      dt[, hs6 := as.character(hs6)]
    }
    
    return(dt)
  }
  
  # Load all files
  cat("Loading", length(paths), "file(s)...\n")
  
  dt_list <- lapply(seq_along(paths), function(i) {
    path <- paths[i]
    cat("  [", i, "/", length(paths), "] ", basename(path), sep = "")
    
    dt <- load_single(path)
    
    cat(" -", format(nrow(dt), big.mark = ","), "rows\n")
    
    # Add source identifier if requested
    if (!is.null(id_col)) {
      dt[, (id_col) := basename(path)]
    }
    
    return(dt)
  })
  
  # Combine all datasets
  if (length(dt_list) == 1) {
    dt <- dt_list[[1]]
  } else {
    # Check for column consistency
    all_cols <- lapply(dt_list, names)
    common_cols <- Reduce(intersect, all_cols)
    all_unique_cols <- unique(unlist(all_cols))
    
    if (length(common_cols) < length(all_unique_cols)) {
      missing_info <- sapply(seq_along(dt_list), function(i) {
        missing <- setdiff(all_unique_cols, names(dt_list[[i]]))
        if (length(missing) > 0) {
          paste0(basename(paths[i]), ": missing ", paste(missing, collapse = ", "))
        } else {
          NULL
        }
      })
      missing_info <- missing_info[!sapply(missing_info, is.null)]
      
      if (length(missing_info) > 0) {
        warning("Column mismatch across files (using rbindlist with fill=TRUE):\n  ",
                paste(missing_info, collapse = "\n  "))
      }
    }
    
    dt <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
  }
  
  cat("\nTotal rows loaded:", format(nrow(dt), big.mark = ","), "\n")
  
  return(dt)
}

# =============================================================================
# DATA VALIDATION
# =============================================================================

validate_trade_data <- function(dt) {
#
# Validates input data and reports issues
# Returns cleaned data.table or stops with error
#
  
  required_cols <- c("year", "exporter", "importer", "hs6", "value", "quantity", "uom")
  
  # Check required columns
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  dt <- as.data.table(copy(dt))
  
  # Type coercion
  dt[, hs6 := as.character(hs6)]
  dt[, year := as.integer(year)]
  dt[, exporter := as.character(exporter)]
  dt[, importer := as.character(importer)]
  dt[, uom := as.character(uom)]
  dt[, value := as.numeric(value)]
  dt[, quantity := as.numeric(quantity)]
  
  # Report data summary
  cat("\n=== Data Validation Summary ===\n")
  cat("Rows:           ", format(nrow(dt), big.mark = ","), "\n")
  cat("Years:          ", paste(range(dt$year, na.rm = TRUE), collapse = " - "), "\n")
  cat("HS6 codes:      ", format(uniqueN(dt$hs6), big.mark = ","), "\n")
  cat("Exporters:      ", format(uniqueN(dt$exporter), big.mark = ","), "\n")
  cat("Importers:      ", format(uniqueN(dt$importer), big.mark = ","), "\n")
  cat("UOMs:           ", paste(unique(dt$uom), collapse = ", "), "\n")
  
  # Check for issues
  issues <- list()
  
  n_na_value <- sum(is.na(dt$value))
  n_na_qty <- sum(is.na(dt$quantity))
  n_zero_value <- sum(dt$value == 0, na.rm = TRUE)
  n_zero_qty <- sum(dt$quantity == 0, na.rm = TRUE)
  n_neg_value <- sum(dt$value < 0, na.rm = TRUE)
  n_neg_qty <- sum(dt$quantity < 0, na.rm = TRUE)
  
  if (n_na_value > 0) issues <- c(issues, sprintf("NA values: %d rows", n_na_value))
  if (n_na_qty > 0) issues <- c(issues, sprintf("NA quantities: %d rows", n_na_qty))
  if (n_zero_value > 0) issues <- c(issues, sprintf("Zero values: %d rows", n_zero_value))
  if (n_zero_qty > 0) issues <- c(issues, sprintf("Zero quantities: %d rows", n_zero_qty))
  if (n_neg_value > 0) issues <- c(issues, sprintf("Negative values: %d rows", n_neg_value))
  if (n_neg_qty > 0) issues <- c(issues, sprintf("Negative quantities: %d rows", n_neg_qty))
  
  if (length(issues) > 0) {
    cat("\nData issues (will be filtered):\n")
    for (issue in issues) cat("  - ", issue, "\n")
  } else {
    cat("\nNo data issues detected.\n")
  }
  
  # Filter to valid rows
  n_before <- nrow(dt)
  dt <- dt[!is.na(value) & !is.na(quantity) & value > 0 & quantity > 0]
  n_after <- nrow(dt)
  
  if (n_before > n_after) {
    cat("\nFiltered:", format(n_before - n_after, big.mark = ","), 
        "rows removed (", round(100 * (n_before - n_after) / n_before, 1), "%)\n")
  }
  
  cat("Final rows:     ", format(nrow(dt), big.mark = ","), "\n")
  
  return(dt)
}

# =============================================================================
# CORE FUNCTIONS (DO NOT MODIFY)
# =============================================================================

# --- Weighted quantile helpers ---
wtd_quantile <- function(x, w, probs) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) == 0L) return(rep(NA_real_, length(probs)))
  o <- order(x)
  x <- x[o]; w <- w[o]
  cw <- cumsum(w)
  sw <- cw[length(cw)]
  sapply(probs, function(p) x[which(cw >= p * sw)[1L]])
}

wtd_median <- function(x, w) as.numeric(wtd_quantile(x, w, 0.5))

# --- Permutation-based divergence test ---
test_divergence_perm <- function(P, V, R_boot = 500, R_perm = 500) {
  n <- length(P)
  if (n < 5) {
    return(list(
      delta_obs = NA_real_, delta_boot_se = NA_real_,
      delta_boot_lo = NA_real_, delta_boot_hi = NA_real_,
      p_value = NA_real_
    ))
  }
  
  compute_delta <- function(p, v) {
    gap_eq <- log(quantile(p, 0.90, type = 7)) - log(quantile(p, 0.10, type = 7))
    q_wt <- wtd_quantile(p, v, c(0.10, 0.90))
    gap_wt <- log(q_wt[2]) - log(q_wt[1])
    gap_wt - gap_eq
  }
  
  delta_obs <- compute_delta(P, V)
  
  delta_boot <- replicate(R_boot, {
    idx <- sample.int(n, replace = TRUE)
    compute_delta(P[idx], V[idx])
  })
  
  delta_null <- replicate(R_perm, {
    V_perm <- V[sample.int(n)]
    compute_delta(P, V_perm)
  })
  
  p_value <- mean(abs(delta_null) >= abs(delta_obs))
  boot_ci <- quantile(delta_boot, c(0.025, 0.975), na.rm = TRUE)
  
  list(
    delta_obs = delta_obs,
    delta_boot_se = sd(delta_boot, na.rm = TRUE),
    delta_boot_lo = as.numeric(boot_ci[1]),
    delta_boot_hi = as.numeric(boot_ci[2]),
    p_value = p_value
  )
}

# --- Dominant-UOM homogeneity pipeline ---
dominant_uom_metrics <- function(dt, config) {
  dt <- as.data.table(copy(dt))
  
  # 1) Collapse to exporter-year-hs6-uom
  ex <- dt[
    , .(V = sum(value, na.rm = TRUE), Q = sum(quantity, na.rm = TRUE)),
    by = .(hs6, year, uom, exporter)
  ][
    Q > config$min_Q_exporter & V > config$min_V_exporter
  ][, P := V / Q][is.finite(P) & P > 0]
  
  # 2) Dominant UOM per hs6-year
  uom_tot <- ex[, .(V_uom = sum(V), n_exporters_uom = uniqueN(exporter)), by = .(hs6, year, uom)]
  uom_tot[, V_total := sum(V_uom), by = .(hs6, year)]
  uom_tot[, coverage := V_uom / V_total]
  
  setorder(uom_tot, hs6, year, -V_uom)
  dom <- uom_tot[, .SD[1L], by = .(hs6, year)]
  setnames(dom, c("uom", "V_uom"), c("uom_dom", "V_dom"))
  
  # 3) Restrict to dominant UOM
  ex_dom <- ex[dom, on = .(hs6, year, uom = uom_dom), nomatch = 0L]
  
  # 4) Optional trimming
  if (config$trim_tail > 0) {
    ex_dom[, `:=`(
      lo = as.numeric(quantile(P, config$trim_tail, na.rm = TRUE, type = 7)),
      hi = as.numeric(quantile(P, 1 - config$trim_tail, na.rm = TRUE, type = 7))
    ), by = .(hs6, year)]
    ex_dom <- ex_dom[P >= lo & P <= hi]
  }
  
  # 5) Compute quantiles
  eq <- ex_dom[, .(
    n_exporters_dom = uniqueN(exporter),
    p10_eq = as.numeric(quantile(P, 0.10, type = 7)),
    p90_eq = as.numeric(quantile(P, 0.90, type = 7))
  ), by = .(hs6, year)]
  
  wt <- ex_dom[, {
    q <- wtd_quantile(P, V, c(0.10, 0.90))
    .(p10_wt = as.numeric(q[1L]), p90_wt = as.numeric(q[2L]))
  }, by = .(hs6, year)]
  
  out <- wt[eq, on = .(hs6, year)]
  out <- dom[out, on = .(hs6, year)]
  
  out[, `:=`(
    ratio_eq = p90_eq / p10_eq, log_gap_eq = log(p90_eq / p10_eq), H_eq = p10_eq / p90_eq,
    ratio_wt = p90_wt / p10_wt, log_gap_wt = log(p90_wt / p10_wt), H_wt = p10_wt / p90_wt
  )]
  
  out[, ok_exporters := n_exporters_dom >= config$min_exporters]
  out[, ok_coverage := coverage >= config$min_coverage]
  
  out[ok_exporters == FALSE, `:=`(
    p10_eq = NA_real_, p90_eq = NA_real_, ratio_eq = NA_real_, log_gap_eq = NA_real_, H_eq = NA_real_,
    p10_wt = NA_real_, p90_wt = NA_real_, ratio_wt = NA_real_, log_gap_wt = NA_real_, H_wt = NA_real_
  )]
  
  # 6) Aggregate to HS6-level
  out[, year_w := fifelse(is.finite(V_dom) & is.finite(coverage), V_dom * coverage, NA_real_)]
  
  hs6_level <- out[
    is.finite(log_gap_eq) & is.finite(year_w) & year_w > 0,
    {
      medD_eq <- wtd_median(log_gap_eq, year_w)
      medD_wt <- wtd_median(log_gap_wt, year_w)
      .(
        years_used = .N,
        med_log_gap_eq = medD_eq, med_H_eq = exp(-medD_eq),
        med_log_gap_wt = medD_wt, med_H_wt = exp(-medD_wt),
        avg_coverage_w = sum(coverage * year_w) / sum(year_w),
        years_ok_coverage_share = mean(ok_coverage)
      )
    },
    by = .(hs6)
  ]
  
  list(year_level = out[], hs6_level = hs6_level[], exporter_level = ex_dom[])
}

# --- Divergence testing pipeline ---
run_divergence_tests <- function(ex_dom, config) {
  cat("Running permutation tests...\n")
  
  divergence_tests <- ex_dom[, {
    res <- test_divergence_perm(P, V, R_boot = config$R_boot, R_perm = config$R_perm)
    .(delta_obs = res$delta_obs, delta_se = res$delta_boot_se,
      delta_lo = res$delta_boot_lo, delta_hi = res$delta_boot_hi,
      p_value = res$p_value, n_exporters = .N)
  }, by = .(hs6, year)]
  
  divergence_tests[, p_adj := p.adjust(p_value, method = "BH")]
  divergence_tests[, flag_year := p_adj < config$fdr_level]
  divergence_tests[, direction := fifelse(delta_obs > 0, "large_in_tails",
                                           fifelse(delta_obs < 0, "large_in_center", "neutral"))]
  
  hs6_divergence <- divergence_tests[
    !is.na(p_value),
    .(n_years = .N, n_flagged = sum(flag_year), pct_flagged = mean(flag_year),
      med_delta = median(delta_obs), mean_delta = mean(delta_obs), sd_delta = sd(delta_obs),
      sign_consistency = abs(mean(sign(delta_obs))), pct_positive = mean(delta_obs > 0),
      fisher_stat = -2 * sum(log(pmax(p_value, 1e-10)))),
    by = hs6
  ][n_years >= config$min_years_for_hs6
  ][, fisher_p := pchisq(fisher_stat, df = 2 * n_years, lower.tail = FALSE)
  ][, fisher_p_adj := p.adjust(fisher_p, method = "BH")]
  
  hs6_divergence[, flag_divergence := fisher_p_adj < config$fdr_level]
  hs6_divergence[, dominant_direction := fifelse(med_delta > 0, "large_in_tails",
                                                  fifelse(med_delta < 0, "large_in_center", "neutral"))]
  
  list(year_level = divergence_tests[], hs6_level = hs6_divergence[])
}

# --- Rauch classification (GMM-based) ---
classify_rauch_gmm <- function(year_level, hs6_level, config) {
  
  # Step 1: Compute temporal stability
  stability <- year_level[
    is.finite(log_gap_eq),
    .(mean_log_gap = mean(log_gap_eq), sd_log_gap = sd(log_gap_eq),
      cv_log_gap = sd(log_gap_eq) / abs(mean(log_gap_eq)),
      iqr_log_gap = IQR(log_gap_eq), n_years_valid = .N),
    by = hs6
  ]
  
  dt <- stability[hs6_level, on = "hs6"]
  dt <- dt[is.finite(med_log_gap_eq) & n_years_valid >= config$min_years_classify]
  dt[!is.finite(cv_log_gap), cv_log_gap := max(dt$cv_log_gap, na.rm = TRUE) * 2]
  
  if (nrow(dt) < config$n_components * 3) {
    warning("Too few HS6 codes for reliable GMM estimation; results may be unstable")
  }
  
  # Step 2: Fit GMM
  fit <- NULL
  fit_2d_success <- FALSE
  scale_center <- NULL
  scale_sd <- NULL
  
  if (config$use_2d && nrow(dt) >= 10) {
    X <- as.matrix(dt[, .(med_log_gap_eq, cv_log_gap)])
    X_scaled <- scale(X)
    scale_center <- attr(X_scaled, "scaled:center")
    scale_sd <- attr(X_scaled, "scaled:scale")
    
    fit <- tryCatch(Mclust(X_scaled, G = 2:config$n_components, verbose = FALSE), error = function(e) NULL)
    
    if (!is.null(fit)) {
      fit_2d_success <- TRUE
      centroids_scaled <- fit$parameters$mean
      homog_score <- 0.7 * centroids_scaled[1, ] + 0.3 * centroids_scaled[2, ]
      ord <- order(homog_score)
      n_detected <- ncol(centroids_scaled)
    }
  }
  
  if (!fit_2d_success) {
    x <- dt$med_log_gap_eq
    max_G <- min(config$n_components, max(2, floor(nrow(dt) / 3)))
    fit <- tryCatch(Mclust(x, G = 2:max_G, modelNames = "V", verbose = FALSE), error = function(e) NULL)
    
    if (is.null(fit)) {
      warning("GMM failed; using quantile-based classification")
      q <- quantile(x, c(0.33, 0.67), na.rm = TRUE)
      dt[, gmm_class := fcase(med_log_gap_eq <= q[1], 1L, med_log_gap_eq <= q[2], 2L, default = 3L)]
      dt[, rauch_category := fcase(gmm_class == 1L, "homogeneous", gmm_class == 2L, "reference", default = "differentiated")]
      dt[, posterior_prob := 0.5]
      
      cv_threshold <- quantile(dt$cv_log_gap, config$instability_override_quantile, na.rm = TRUE)
      dt[, stability_flag := cv_log_gap > cv_threshold]
      dt[, rauch_original := rauch_category]
      n_overridden <- dt[rauch_category == "homogeneous" & stability_flag == TRUE, .N]
      dt[rauch_category == "homogeneous" & stability_flag == TRUE, rauch_category := "reference"]
      
      category_summary <- dt[, .(n_hs6 = .N,
        med_log_gap_range = paste0("[", round(min(med_log_gap_eq), 3), ", ", round(max(med_log_gap_eq), 3), "]"),
        med_H_range = paste0("[", round(min(med_H_eq), 3), ", ", round(max(med_H_eq), 3), "]"),
        mean_cv_log_gap = round(mean(cv_log_gap, na.rm = TRUE), 3),
        mean_posterior = round(mean(posterior_prob), 3)
      ), by = rauch_category][order(factor(rauch_category, levels = c("homogeneous", "reference", "differentiated")))]
      
      return(list(
        classification = dt[, .(hs6, med_log_gap_eq, med_H_eq, cv_log_gap, years_used,
                                 rauch_category, rauch_original, gmm_class, posterior_prob, stability_flag)],
        fit = NULL,
        thresholds = list(type = "quantile_fallback", boundaries = q, n_components = 3),
        category_summary = category_summary,
        override_summary = list(cv_threshold = cv_threshold, n_overridden = n_overridden),
        settings = list(use_2d = FALSE, n_components = 3, min_years = config$min_years_classify,
                        instability_override_quantile = config$instability_override_quantile, method = "quantile_fallback")
      ))
    }
    
    ord <- order(fit$parameters$mean)
    n_detected <- length(fit$parameters$mean)
  }
  
  # Step 3: Assign labels
  labels <- if (n_detected == 2) c("homogeneous", "differentiated") else c("homogeneous", "reference", "differentiated")
  label_map <- setNames(labels, as.character(ord))
  
  dt[, gmm_class := fit$classification]
  dt[, rauch_category := label_map[as.character(gmm_class)]]
  dt[, posterior_prob := apply(fit$z, 1, max)]
  
  # Step 4: Stability override
  cv_threshold <- quantile(dt$cv_log_gap, config$instability_override_quantile, na.rm = TRUE)
  dt[, stability_flag := cv_log_gap > cv_threshold]
  dt[, rauch_original := rauch_category]
  n_overridden <- dt[rauch_category == "homogeneous" & stability_flag == TRUE, .N]
  dt[rauch_category == "homogeneous" & stability_flag == TRUE, rauch_category := "reference"]
  
  # Step 5: Summaries
  category_summary <- dt[, .(n_hs6 = .N,
    med_log_gap_range = paste0("[", round(min(med_log_gap_eq), 3), ", ", round(max(med_log_gap_eq), 3), "]"),
    med_H_range = paste0("[", round(min(med_H_eq), 3), ", ", round(max(med_H_eq), 3), "]"),
    mean_cv_log_gap = round(mean(cv_log_gap, na.rm = TRUE), 3),
    mean_posterior = round(mean(posterior_prob), 3)
  ), by = rauch_category][order(factor(rauch_category, levels = c("homogeneous", "reference", "differentiated")))]
  
  # Step 6: Thresholds
  if (fit_2d_success) {
    centroids_original <- t(t(fit$parameters$mean) * scale_sd + scale_center)
    colnames(centroids_original) <- labels
    rownames(centroids_original) <- c("med_log_gap_eq", "cv_log_gap")
    thresholds <- list(type = "2D_GMM", centroids = centroids_original, n_components = n_detected)
  } else {
    means <- fit$parameters$mean[ord]
    boundaries <- if (n_detected == 2) mean(means) else c(mean(means[1:2]), mean(means[2:3]))
    thresholds <- list(type = "1D_GMM", component_means = setNames(means, labels),
                       decision_boundaries = boundaries, n_components = n_detected)
  }
  
  list(
    classification = dt[, .(hs6, med_log_gap_eq, med_H_eq, cv_log_gap, years_used,
                             rauch_category, rauch_original, gmm_class, posterior_prob, stability_flag)],
    fit = fit,
    thresholds = thresholds,
    category_summary = category_summary,
    override_summary = list(cv_threshold = cv_threshold, n_overridden = n_overridden),
    settings = list(use_2d = fit_2d_success, n_components = config$n_components, min_years = config$min_years_classify,
                    instability_override_quantile = config$instability_override_quantile,
                    method = if (fit_2d_success) "2D_GMM" else "1D_GMM")
  )
}

# --- Year-level Rauch classification ---
classify_years_rauch <- function(year_level, hs6_classification) {
  year_classified <- hs6_classification$classification[, .(hs6, rauch_category, cv_log_gap)][year_level, on = "hs6"]
  thresholds <- hs6_classification$thresholds
  
  if (thresholds$type == "1D_GMM") {
    bounds <- thresholds$decision_boundaries
    if (length(bounds) == 1) {
      year_classified[, rauch_year := fifelse(log_gap_eq <= bounds[1], "homogeneous", "differentiated")]
    } else {
      year_classified[, rauch_year := fcase(log_gap_eq <= bounds[1], "homogeneous",
                                             log_gap_eq <= bounds[2], "reference", default = "differentiated")]
    }
  } else if (thresholds$type == "quantile_fallback") {
    bounds <- thresholds$boundaries
    year_classified[, rauch_year := fcase(log_gap_eq <= bounds[1], "homogeneous",
                                           log_gap_eq <= bounds[2], "reference", default = "differentiated")]
  } else {
    year_classified[, rauch_year := rauch_category]
  }
  
  transitions <- year_classified[
    !is.na(rauch_year) & !is.na(rauch_category),
    .(n_years = .N, n_categories_year = uniqueN(rauch_year),
      modal_category = names(sort(table(rauch_year), decreasing = TRUE))[1],
      matches_hs6_category = mean(rauch_year == rauch_category)),
    by = hs6
  ]
  
  list(year_classified = year_classified[], transitions = transitions[])
}

# =============================================================================
# MAIN PIPELINE FUNCTION
# =============================================================================

run_full_pipeline <- function(dt, config, verbose = TRUE) {
#
# Runs the complete pipeline:
#   1. Homogeneity metrics
#   2. Divergence tests
#   3. Rauch classification
#
# Arguments:
#   dt:      Validated trade data (data.table)
#   config:  Configuration list
#   verbose: Print progress and summaries
#
# Returns:
#   List with all results
#
  
  if (verbose) cat("\n=== Step 1: Computing Homogeneity Metrics ===\n")
  homog <- dominant_uom_metrics(dt, config)
  
  if (verbose) {
    cat("Year-level observations: ", format(nrow(homog$year_level), big.mark = ","), "\n")
    cat("HS6 codes with valid estimates: ", format(nrow(homog$hs6_level), big.mark = ","), "\n")
  }
  
  if (verbose) cat("\n=== Step 2: Running Divergence Tests ===\n")
  div <- run_divergence_tests(homog$exporter_level, config)
  
  if (verbose) {
    n_flagged <- sum(div$hs6_level$flag_divergence, na.rm = TRUE)
    cat("HS6 codes with significant divergence: ", n_flagged, "\n")
  }
  
  if (verbose) cat("\n=== Step 3: Rauch Classification ===\n")
  rauch <- classify_rauch_gmm(homog$year_level, homog$hs6_level, config)
  
  if (verbose) {
    cat("Method used: ", rauch$settings$method, "\n")
    cat("\nCategory distribution:\n")
    print(rauch$category_summary)
  }
  
  if (verbose) cat("\n=== Step 4: Year-Level Classification ===\n")
  year_rauch <- classify_years_rauch(homog$year_level, rauch)
  
  # Combine results
  combined <- rauch$classification[
    div$hs6_level[, .(hs6, med_delta, flag_divergence, dominant_direction)],
    on = "hs6"
  ]
  
  if (verbose) {
    cat("\n=== Pipeline Complete ===\n")
    cat("Results available in output list.\n")
  }
  
  list(
    homogeneity = homog,
    divergence = div,
    rauch = rauch,
    year_rauch = year_rauch,
    combined = combined,
    config = config
  )
}

# =============================================================================
# EXPORT FUNCTIONS
# =============================================================================

export_results <- function(results, output_dir = "output", prefix = "homogeneity") {
#
# Exports results to CSV files
#
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Year-level homogeneity
  fwrite(results$homogeneity$year_level, 
         file.path(output_dir, paste0(prefix, "_year_level.csv")))
  
  # HS6-level homogeneity
  fwrite(results$homogeneity$hs6_level,
         file.path(output_dir, paste0(prefix, "_hs6_level.csv")))
  
  # Divergence results
  fwrite(results$divergence$hs6_level,
         file.path(output_dir, paste0(prefix, "_divergence.csv")))
  
  # Rauch classification
  fwrite(results$rauch$classification,
         file.path(output_dir, paste0(prefix, "_rauch_classification.csv")))
  
  # Combined output
  fwrite(results$combined,
         file.path(output_dir, paste0(prefix, "_combined.csv")))
  
  # Year-level Rauch
  fwrite(results$year_rauch$year_classified,
         file.path(output_dir, paste0(prefix, "_year_rauch.csv")))
  
  cat("Results exported to:", output_dir, "\n")
  cat("Files:\n")
  cat("  - ", prefix, "_year_level.csv\n", sep = "")
  cat("  - ", prefix, "_hs6_level.csv\n", sep = "")
  cat("  - ", prefix, "_divergence.csv\n", sep = "")
  cat("  - ", prefix, "_rauch_classification.csv\n", sep = "")
  cat("  - ", prefix, "_combined.csv\n", sep = "")
  cat("  - ", prefix, "_year_rauch.csv\n", sep = "")
}

# =============================================================================
# USAGE EXAMPLE
# =============================================================================

#' To run the pipeline:
#'
#' 1. Load your data (multiple options):
#'
#'    # Single file
#'    dt <- load_trade_data("data/trade_all_years.csv")
#'
#'    # Multiple files by year
#'    dt <- load_trade_data(c(
#'      "data/trade_2022.csv",
#'      "data/trade_2023.csv", 
#'      "data/trade_2024.csv"
#'    ))
#'
#'    # All files in a directory
#'    dt <- load_trade_data("data/annual/")
#'
#'    # With source tracking (adds column identifying origin file)
#'    dt <- load_trade_data(
#'      c("data/comtrade_2023.csv", "data/census_2023.csv"),
#'      id_col = "data_source"
#'    )
#'
#' 2. Validate:
#'    dt <- validate_trade_data(dt)
#'
#' 3. Run pipeline:
#'    results <- run_full_pipeline(dt, config)
#'
#' 4. Access results:
#'    results$combined                    # Main HS6-level output
#'    results$homogeneity$year_level      # Year-level metrics
#'    results$rauch$classification        # Rauch categories
#'    results$divergence$hs6_level        # Divergence test results
#'
#' 5. Export:
#'    export_results(results, output_dir = "output")


# =============================================================================
# UNCOMMENT BELOW TO RUN
# =============================================================================

# # --- Load and validate data ---
# dt <- load_trade_data(c(
#   "data/trade_2022.csv",
#   "data/trade_2023.csv",
#   "data/trade_2024.csv"
# ))
# dt <- validate_trade_data(dt)
#
# # --- Run pipeline ---
# results <- run_full_pipeline(dt, config, verbose = TRUE)
#
# # --- View key outputs ---
# print(results$combined[order(med_log_gap_eq)])
# print(results$rauch$category_summary)
#
# # --- Export ---
# export_results(results, output_dir = "output", prefix = "trade_homogeneity")
