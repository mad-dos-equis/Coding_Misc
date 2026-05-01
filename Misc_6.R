# ============================================================================
# Company Name Fuzzy Matching — Production Library
# ----------------------------------------------------------------------------
# Source this file, then call run_fuzzy_match_pipeline() on your data frames.
#
# Usage:
#   source("company_name_fuzzy_match.R")
#
#   result <- run_fuzzy_match_pipeline(
#     left_df       = my_small_df,
#     right_df      = my_large_df,
#     left_name_col  = "company_name",
#     right_name_col = "firm_name",
#     left_id_col    = "id",            # optional; auto-generated if NULL
#     right_id_col   = "id"             # optional; auto-generated if NULL
#   )
#
#   result$matches      # final matched rows with tier, method, distance
#   result$silver       # silver-standard pairs
#   result$grid         # grid evaluation results
#   result$audit        # stratified audit sample for manual review
#   result$config       # winning (method, max_dist) configuration
# ============================================================================

required_pkgs <- c("data.table", "fuzzyjoin", "stringdist")
to_install <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)

suppressPackageStartupMessages({
  library(data.table)
  library(fuzzyjoin)
  library(stringdist)
})


# ---- Normalization for company names --------------------------------------
# Standardizes case, accents, ampersands, punctuation, and legal suffixes.
# Preserves entity distinction (e.g., "Apple Inc" vs "Apple Corp" stay
# distinct) while eliminating spurious distance from formatting differences.

normalize_company <- function(x) {
  x <- as.character(x)
  x <- tolower(x)
  # ASCII-fold accents: "nestlé" -> "nestle"
  folded <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- ifelse(is.na(folded), x, folded)
  # Standardize ampersand BEFORE stripping punctuation
  x <- gsub("&", " and ", x)
  x <- gsub("[[:punct:]]", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  # Strip leading "the "
  x <- sub("^the\\s+", "", x)
  # Standardize legal suffixes (canonicalize form, don't delete)
  x <- gsub("\\b(corporation|corp)\\b",                     "corp",  x)
  x <- gsub("\\b(incorporated|inc)\\b",                     "inc",   x)
  x <- gsub("\\b(limited|ltd)\\b",                          "ltd",   x)
  x <- gsub("\\b(company|co)\\b",                           "co",    x)
  x <- gsub("\\b(industries|inds|industry)\\b",             "inds",  x)
  x <- gsub("\\b(holdings|hldgs|hldg)\\b",                  "hldgs", x)
  x <- gsub("\\b(group|grp)\\b",                            "grp",   x)
  x <- gsub("\\bllc\\b",                                    "llc",   x)
  x <- gsub("\\b(llp|lp|plc|gmbh|ag|sa|nv|bv|spa|pty)\\b",  "\\1",   x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}


# ---- Single-config matcher (best match per left_id) -----------------------

match_once <- function(left_dt, right_dt, method, max_dist,
                        match_col = "match_key") {
  m <- tryCatch(
    fuzzyjoin::stringdist_inner_join(
      as.data.frame(left_dt), as.data.frame(right_dt),
      by = setNames(match_col, match_col),
      method = method, max_dist = max_dist,
      distance_col = "match_dist", ignore_case = TRUE
    ),
    error = function(e) { warning(e$message); NULL }
  )
  if (is.null(m) || nrow(m) == 0) return(data.table())
  m <- as.data.table(m)
  setorder(m, left_id, match_dist, right_id)
  m[, .SD[1L], by = left_id]
}


# ---- Silver-standard bootstrap --------------------------------------------
# High-confidence pairs from multi-method agreement at very tight thresholds.

build_silver_standard <- function(left_dt, right_dt,
                                   tight_configs = list(
                                     list(method = "jw",      max_dist = 0.05),
                                     list(method = "cosine",  max_dist = 0.10),
                                     list(method = "jaccard", max_dist = 0.15),
                                     list(method = "osa",     max_dist = 2)
                                   ),
                                   min_methods_agreeing = 2L,
                                   verbose = TRUE) {

  preds <- rbindlist(lapply(tight_configs, function(cfg) {
    p <- match_once(left_dt, right_dt, cfg$method, cfg$max_dist)
    if (nrow(p) == 0) return(data.table())
    p[, .(left_id, right_id, method = cfg$method)]
  }))

  if (nrow(preds) == 0) {
    if (verbose) warning("No silver-standard pairs found. Loosen tight_configs.")
    return(data.table(left_id = integer(), right_id = integer(),
                      n_methods_agreeing = integer()))
  }

  agreement <- preds[, .(n_methods_agreeing = uniqueN(method)),
                     by = .(left_id, right_id)]
  silver <- agreement[n_methods_agreeing >= min_methods_agreeing]
  silver <- silver[, if (.N == 1) .SD else .SD[0L], by = left_id]

  if (verbose) {
    coverage <- nrow(silver) / nrow(left_dt)
    cat(sprintf("  Silver pairs: %d / %d left rows (%.1f%% coverage)\n",
                nrow(silver), nrow(left_dt), 100 * coverage))
    if (coverage < 0.30)
      cat("  WARNING: low silver coverage. Grid ranking may be unreliable.\n",
          "  Consider loosening tight_configs.\n")
  }
  silver[]
}


# ---- Grid evaluation against silver standard ------------------------------

evaluate_vs_silver <- function(left_dt, right_dt, silver, method, max_dist) {
  preds <- match_once(left_dt, right_dt, method, max_dist)

  if (nrow(silver) == 0 || nrow(preds) == 0) {
    return(data.table(
      method = method, max_dist = max_dist,
      n_predicted = nrow(preds),
      silver_recall = if (nrow(preds) == 0) 0 else NA_real_,
      silver_precision = NA_real_, silver_f1 = NA_real_
    ))
  }

  pred_lookup <- preds[, .(left_id, predicted_right = right_id)]
  joined <- merge(silver, pred_lookup, by = "left_id", all.x = TRUE)
  joined[, agrees := !is.na(predicted_right) & predicted_right == right_id]
  silver_recall <- mean(joined$agrees)

  preds_in_silver <- merge(preds[left_id %in% silver$left_id],
                           silver[, .(left_id, silver_right = right_id)],
                           by = "left_id")
  preds_in_silver[, correct := right_id == silver_right]
  silver_precision <- if (nrow(preds_in_silver) > 0)
    mean(preds_in_silver$correct) else NA_real_

  silver_f1 <- if (!is.na(silver_recall) && !is.na(silver_precision) &&
                   (silver_recall + silver_precision) > 0)
    2 * silver_recall * silver_precision /
        (silver_recall + silver_precision) else NA_real_

  data.table(method = method, max_dist = max_dist, n_predicted = nrow(preds),
             silver_recall = silver_recall, silver_precision = silver_precision,
             silver_f1 = silver_f1)
}


# ---- Default parameter grids for company-name matching -------------------

default_grid <- function() {
  methods <- c("jw", "osa", "lv", "cosine", "jaccard")
  max_dists <- list(
    jw      = c(0.05, 0.10, 0.15, 0.20, 0.25),
    osa     = c(1, 2, 3, 4, 5),
    lv      = c(1, 2, 3, 4, 5),
    cosine  = c(0.05, 0.10, 0.20, 0.30, 0.40),
    jaccard = c(0.10, 0.20, 0.30, 0.40, 0.50)
  )
  rbindlist(lapply(methods, function(m)
    data.table(method = m, max_dist = max_dists[[m]])))
}


# ---- Tiered production matcher --------------------------------------------

tiered_match <- function(left_dt, right_dt, tiers, verbose = TRUE) {
  unmatched <- copy(left_dt)
  results <- vector("list", length(tiers))

  for (i in seq_along(tiers)) {
    t <- tiers[[i]]
    if (verbose)
      cat(sprintf("  Tier %d [%s]: method=%s, max_dist=%.3f, candidates=%d ... ",
                  i, t$label, t$method, t$max_dist, nrow(unmatched)))

    matched <- if (nrow(unmatched) > 0)
      match_once(unmatched, right_dt, t$method, t$max_dist) else data.table()

    if (nrow(matched) > 0) {
      matched[, `:=`(tier = t$label, method = t$method)]
      unmatched <- unmatched[!left_id %in% matched$left_id]
    }
    if (verbose) cat(sprintf("matched=%d\n", nrow(matched)))
    results[[i]] <- matched
    if (nrow(unmatched) == 0) break
  }

  matched_all <- rbindlist(results, fill = TRUE)
  if (nrow(unmatched) > 0) {
    unmatched[, `:=`(tier = "unmatched", method = NA_character_,
                     match_dist = NA_real_)]
    final <- rbindlist(list(matched_all, unmatched), fill = TRUE)
  } else {
    final <- matched_all
  }
  setorder(final, left_id)
  final[]
}


# ---- Audit sample at the decision boundary -------------------------------

audit_sample <- function(left_dt, right_dt, method, max_dist,
                          n_per_bucket = 5, n_buckets = 4) {
  all_cands <- tryCatch(
    fuzzyjoin::stringdist_inner_join(
      as.data.frame(left_dt), as.data.frame(right_dt),
      by = c("match_key" = "match_key"),
      method = method, max_dist = max_dist,
      distance_col = "match_dist", ignore_case = TRUE
    ),
    error = function(e) NULL
  )
  if (is.null(all_cands) || nrow(all_cands) == 0) return(data.table())
  all_cands <- as.data.table(all_cands)
  setorder(all_cands, left_id, match_dist)
  best <- all_cands[, .SD[1L], by = left_id]
  best[, bucket := cut(match_dist,
                       breaks = seq(0, max_dist, length.out = n_buckets + 1),
                       include.lowest = TRUE, labels = FALSE)]
  audit <- best[, .SD[sample(.N, min(.N, n_per_bucket))], by = bucket]
  audit[order(bucket, match_dist)]
}


# ---- End-to-end pipeline (the function you'll typically call) ------------

run_fuzzy_match_pipeline <- function(
  left_df, right_df,
  left_name_col, right_name_col,
  left_id_col  = NULL,
  right_id_col = NULL,
  grid = NULL,
  tight_configs = NULL,
  min_methods_agreeing = 2L,
  tier_multipliers = c(strict = 0.5, moderate = 1.0, loose = 1.5),
  grid_sample_size = NULL,   # if non-NULL and < nrow(left), grid uses a sample
  audit_n_per_bucket = 5,
  audit_n_buckets = 4,
  verbose = TRUE
) {

  stopifnot(left_name_col %in% names(left_df),
            right_name_col %in% names(right_df))

  L <- as.data.table(copy(left_df))
  R <- as.data.table(copy(right_df))

  # Standardize ID columns. If user passed names, use them; otherwise generate.
  if (is.null(left_id_col)) {
    L[, left_id := .I]
  } else {
    stopifnot(left_id_col %in% names(L))
    if (uniqueN(L[[left_id_col]]) != nrow(L))
      stop("left_id_col '", left_id_col, "' is not unique in left_df.")
    L[, left_id := get(left_id_col)]
  }
  if (is.null(right_id_col)) {
    R[, right_id := .I]
  } else {
    stopifnot(right_id_col %in% names(R))
    if (uniqueN(R[[right_id_col]]) != nrow(R))
      stop("right_id_col '", right_id_col, "' is not unique in right_df.")
    R[, right_id := get(right_id_col)]
  }

  if (verbose) cat("Step 1: Normalizing company names ...\n")
  L[, match_key := normalize_company(get(left_name_col))]
  R[, match_key := normalize_company(get(right_name_col))]
  empty_left  <- sum(L$match_key == "" | is.na(L$match_key))
  empty_right <- sum(R$match_key == "" | is.na(R$match_key))
  if (empty_left + empty_right > 0 && verbose)
    cat(sprintf("  Dropped %d left and %d right rows with empty normalized keys.\n",
                empty_left, empty_right))
  L <- L[match_key != "" & !is.na(match_key)]
  R <- R[match_key != "" & !is.na(match_key)]

  if (verbose) {
    cat(sprintf("  Left:  %d rows after normalization\n", nrow(L)))
    cat(sprintf("  Right: %d rows after normalization\n", nrow(R)))
  }

  L_for_grid <- if (!is.null(grid_sample_size) && grid_sample_size < nrow(L)) {
    if (verbose) cat(sprintf("  Using %d sampled left rows for grid evaluation.\n",
                             grid_sample_size))
    L[sample(.N, grid_sample_size)]
  } else L

  if (verbose) cat("\nStep 2: Building silver standard ...\n")
  silver_args <- list(left_dt = L_for_grid, right_dt = R,
                      min_methods_agreeing = min_methods_agreeing,
                      verbose = verbose)
  if (!is.null(tight_configs)) silver_args$tight_configs <- tight_configs
  silver <- do.call(build_silver_standard, silver_args)

  if (verbose) cat("\nStep 3: Grid evaluation against silver standard ...\n")
  if (is.null(grid)) grid <- default_grid()
  grid_results <- rbindlist(Map(
    evaluate_vs_silver,
    list(L_for_grid), list(R), list(silver),
    grid$method, grid$max_dist
  ))
  setorder(grid_results, -silver_f1, method, max_dist)

  if (all(is.na(grid_results$silver_f1))) {
    stop("Grid evaluation produced no usable F1 scores. Likely cause: silver ",
         "standard is empty. Inspect intermediate output and adjust ",
         "tight_configs or min_methods_agreeing.")
  }

  top <- grid_results[which.max(silver_f1)]
  if (verbose)
    cat(sprintf("  Winning config: method=%s, max_dist=%g, silver-F1=%.3f\n",
                top$method, top$max_dist, top$silver_f1))

  if (verbose) cat("\nStep 4: Tiered production match on full data ...\n")
  tiers <- lapply(names(tier_multipliers), function(label) {
    list(label = label, method = top$method,
         max_dist = top$max_dist * tier_multipliers[[label]])
  })
  matches <- tiered_match(L, R, tiers, verbose = verbose)

  if (verbose) cat("\nStep 5: Generating audit sample ...\n")
  audit <- audit_sample(L, R, method = top$method,
                        max_dist = top$max_dist * max(tier_multipliers),
                        n_per_bucket = audit_n_per_bucket,
                        n_buckets = audit_n_buckets)

  if (verbose) {
    cat("\nMatch counts by tier:\n")
    print(matches[, .N, by = tier])
  }

  list(
    matches = matches,
    silver  = silver,
    grid    = grid_results,
    audit   = audit,
    config  = top,
    normalized_left  = L,
    normalized_right = R
  )
}
