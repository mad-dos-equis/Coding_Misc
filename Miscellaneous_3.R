# ==============================================================================
# TRANSSHIPMENT DETECTION - STEP 1: FLAG ROUTES BY CRITERION
# ==============================================================================
# Implements Freund (2025) "The China Wash" methodology.
# Produces one row per (origin, commodity, third_country) route, flagged with
# binary indicators for each criterion plus intermediate values.
#
# Freund's four criteria:
#   C1  301 tariff on the product.
#       → Assumed satisfied by construction (input data pre-filtered to
#         tariff-affected products). No flag generated.
#   C2  (a) Origin's share of destination imports declines,
#       (b) origin's share of TC imports increases, AND
#       (c) TC's share of destination imports rises.
#   C3  Origin's share of ROW imports grows faster than TC's share of ROW
#       imports in the product.
#   C4  TC imports from origin ≥ 75% of destination imports from TC.
#       (Raw end-year values, per Freund's specification.)
#
# Transshipment value (for downstream calculation):
#   min(origin exports to TC, TC exports to destination) for qualifying routes.
#
# Output: route-level CSV with flags and all intermediate values.
# ==============================================================================

# ------------------------------------------------------------------------------
# CONFIGURATION SECTION
# ------------------------------------------------------------------------------

# File paths
INPUT_FILE_PATH <- "path/to/your/trade_data.csv"  # Update this path
OUTPUT_DIR <- "path/to/output/directory"          # Update this path

# Analysis parameters
ORIGIN_COUNTRIES <- c("CHN")  # ISO3 codes, e.g., c("CHN", "VNM", "MEX")
DESTINATION_COUNTRY <- "USA"  # ISO3 code
START_YEAR <- 2018            # Start year (pre-tariff baseline)
END_YEAR <- 2023              # End year for analysis

# Origin country handling
# When multiple origins specified, allow them as third countries for each other?
# TRUE:  e.g., VNM is a valid TC when analyzing CHN
# FALSE: origin countries excluded from TC list (default)
ALLOW_ORIGIN_COUNTRIES_AS_TC <- FALSE

# Criterion thresholds
MIN_SHARE_FOR_GROWTH <- 0       # Minimum baseline share for C3 growth calc
VOLUME_RATIO_THRESHOLD <- 0.75  # C4: TC imports from origin ≥ 75% of dest imports from TC

# ------------------------------------------------------------------------------
# PACKAGE LOADING
# ------------------------------------------------------------------------------

library(data.table)

cat("==============================================================================\n")
cat("TRANSSHIPMENT DETECTION - STEP 1: FLAG ROUTES\n")
cat("Methodology: Freund (2025) 'The China Wash'\n")
cat("==============================================================================\n\n")

cat("Configuration:\n")
cat(sprintf("  Input file: %s\n", INPUT_FILE_PATH))
cat(sprintf("  Origin countries: %s\n", paste(ORIGIN_COUNTRIES, collapse = ", ")))
cat(sprintf("  Destination: %s\n", DESTINATION_COUNTRY))
cat(sprintf("  Period: %d to %d\n", START_YEAR, END_YEAR))
cat(sprintf("  Allow origins as third countries: %s\n", ALLOW_ORIGIN_COUNTRIES_AS_TC))
cat(sprintf("  Min share for growth (C3): %s\n", MIN_SHARE_FOR_GROWTH))
cat(sprintf("  Volume ratio threshold (C4): %s\n\n", VOLUME_RATIO_THRESHOLD))

# ------------------------------------------------------------------------------
# DATA LOADING AND PREPARATION
# ------------------------------------------------------------------------------

cat("Loading trade data...\n")
trade_data <- fread(INPUT_FILE_PATH)

# Validate required columns
required_cols <- c("commodity", "year", "imp_iso", "exp_iso",
                   "value_importer", "value_exporter")
missing_cols <- setdiff(required_cols, names(trade_data))
if (length(missing_cols) > 0) {
  stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
}

# Calculate preferred values
trade_data[, value_imp_pref := fifelse(
  value_importer == 0 | is.na(value_importer),
  value_exporter,
  value_importer
)]
trade_data[, value_exp_pref := fifelse(
  value_exporter == 0 | is.na(value_exporter),
  value_importer,
  value_exporter
)]

# Exclude self-trade
trade_data <- trade_data[imp_iso != exp_iso]

cat(sprintf("Loaded %s trade records\n", format(nrow(trade_data), big.mark = ",")))

# Split by year
data_start <- trade_data[year == START_YEAR]
data_end   <- trade_data[year == END_YEAR]

# Calculate import shares: exporter's share of importer's total imports per commodity
calc_shares <- function(dt) {
  dt[, total_value := sum(value_imp_pref, na.rm = TRUE), by = .(imp_iso, commodity)]
  dt[, share := fifelse(total_value > 0, value_imp_pref / total_value, 0)]
  dt
}

data_start <- calc_shares(data_start)
data_end   <- calc_shares(data_end)

# Enumerate analysis dimensions
all_commodities <- union(unique(data_start$commodity), unique(data_end$commodity))

if (ALLOW_ORIGIN_COUNTRIES_AS_TC) {
  all_third_countries <- unique(c(
    data_start$imp_iso[data_start$imp_iso != DESTINATION_COUNTRY],
    data_end$imp_iso[data_end$imp_iso != DESTINATION_COUNTRY]
  ))
} else {
  all_third_countries <- unique(c(
    data_start$imp_iso[!data_start$imp_iso %in% c(DESTINATION_COUNTRY, ORIGIN_COUNTRIES)],
    data_end$imp_iso[!data_end$imp_iso %in% c(DESTINATION_COUNTRY, ORIGIN_COUNTRIES)]
  ))
}

cat(sprintf("Analysis scope:\n"))
cat(sprintf("  Commodities:      %s\n", format(length(all_commodities), big.mark = ",")))
cat(sprintf("  Origin countries: %s\n", length(ORIGIN_COUNTRIES)))
cat(sprintf("  Third countries:  %s\n\n", format(length(all_third_countries), big.mark = ",")))

# ==============================================================================
# MAIN LOOP: Build route-level flags by origin country
# ==============================================================================

all_route_results <- vector("list", length(ORIGIN_COUNTRIES))

for (oc_idx in seq_along(ORIGIN_COUNTRIES)) {

  origin_country <- ORIGIN_COUNTRIES[oc_idx]
  cat(sprintf("==============================================================================\n"))
  cat(sprintf("PROCESSING ORIGIN: %s\n", origin_country))
  cat(sprintf("==============================================================================\n\n"))
  origin_timer <- Sys.time()

  # Determine third countries for this origin
  if (ALLOW_ORIGIN_COUNTRIES_AS_TC) {
    tc_list <- all_third_countries
  } else {
    tc_list <- all_third_countries[all_third_countries != origin_country]
  }

  # --------------------------------------------------------------------------
  # Build route skeleton: one row per (commodity, third_country)
  # --------------------------------------------------------------------------

  routes <- CJ(commodity = all_commodities, third_country = tc_list)
  routes[, origin_country := origin_country]

  cat(sprintf("  Route skeleton: %s rows (%s commodities x %s TCs)\n",
              format(nrow(routes), big.mark = ","),
              format(length(all_commodities), big.mark = ","),
              format(length(tc_list), big.mark = ",")))

  # ==========================================================================
  # CRITERION 2: Three-part share test
  #   (a) Origin's share of destination imports declines
  #   (b) Origin's share of TC imports increases
  #   (c) TC's share of destination imports rises
  # ==========================================================================

  cat("  Computing Criterion 2 (three-part share test)...\n")

  # --- C2a: Origin's share of destination imports (one value per commodity) ---
  origin_dest_start <- data_start[
    imp_iso == DESTINATION_COUNTRY & exp_iso == origin_country,
    .(commodity, origin_dest_share_start = share)
  ]
  origin_dest_end <- data_end[
    imp_iso == DESTINATION_COUNTRY & exp_iso == origin_country,
    .(commodity, origin_dest_share_end = share)
  ]

  c2a <- merge(data.table(commodity = all_commodities),
               origin_dest_start, by = "commodity", all.x = TRUE)
  c2a <- merge(c2a, origin_dest_end, by = "commodity", all.x = TRUE)
  setnafill(c2a, fill = 0, cols = c("origin_dest_share_start", "origin_dest_share_end"))

  routes <- merge(routes, c2a, by = "commodity", all.x = TRUE)

  # --- C2b: Origin's share of TC imports ---
  origin_tc_start <- data_start[
    exp_iso == origin_country & imp_iso %in% tc_list,
    .(commodity, third_country = imp_iso, origin_tc_share_start = share)
  ]
  origin_tc_end <- data_end[
    exp_iso == origin_country & imp_iso %in% tc_list,
    .(commodity, third_country = imp_iso, origin_tc_share_end = share)
  ]

  routes <- merge(routes, origin_tc_start, by = c("commodity", "third_country"), all.x = TRUE)
  routes <- merge(routes, origin_tc_end,   by = c("commodity", "third_country"), all.x = TRUE)

  # --- C2c: TC's share of destination imports ---
  tc_dest_start <- data_start[
    imp_iso == DESTINATION_COUNTRY & exp_iso %in% tc_list,
    .(commodity, third_country = exp_iso, tc_dest_share_start = share)
  ]
  tc_dest_end <- data_end[
    imp_iso == DESTINATION_COUNTRY & exp_iso %in% tc_list,
    .(commodity, third_country = exp_iso, tc_dest_share_end = share)
  ]

  routes <- merge(routes, tc_dest_start, by = c("commodity", "third_country"), all.x = TRUE)
  routes <- merge(routes, tc_dest_end,   by = c("commodity", "third_country"), all.x = TRUE)

  # Fill NAs
  c2_share_cols <- c("origin_tc_share_start", "origin_tc_share_end",
                     "tc_dest_share_start", "tc_dest_share_end")
  setnafill(routes, fill = 0, cols = c2_share_cols)

  # Apply three-part criterion
  routes[, criterion2 := as.integer(
    (origin_dest_share_end < origin_dest_share_start) &  # (a) origin share declined
    (origin_tc_share_end > origin_tc_share_start) &      # (b) origin share of TC increased
    (tc_dest_share_end > tc_dest_share_start)            # (c) TC share of dest increased
  )]

  cat(sprintf("    Routes passing C2: %s / %s\n",
              format(sum(routes$criterion2), big.mark = ","),
              format(nrow(routes), big.mark = ",")))

  # ==========================================================================
  # CRITERION 3: Origin's share of ROW imports grows faster than TC's share
  # ==========================================================================

  cat("  Computing Criterion 3 (ROW growth comparison)...\n")

  # ROW = all importers except destination
  row_countries <- union(
    unique(data_start$imp_iso[data_start$imp_iso != DESTINATION_COUNTRY]),
    unique(data_end$imp_iso[data_end$imp_iso != DESTINATION_COUNTRY])
  )

  # --- Total ROW imports per commodity ---
  total_row_start <- data_start[
    imp_iso %in% row_countries,
    .(total_row_start = sum(value_imp_pref, na.rm = TRUE)),
    by = commodity
  ]
  total_row_end <- data_end[
    imp_iso %in% row_countries,
    .(total_row_end = sum(value_imp_pref, na.rm = TRUE)),
    by = commodity
  ]

  # --- Origin's exports to ROW per commodity (excluding self-imports) ---
  origin_row_start <- data_start[
    exp_iso == origin_country & imp_iso %in% row_countries & imp_iso != origin_country,
    .(origin_row_val_start = sum(value_imp_pref, na.rm = TRUE)),
    by = commodity
  ]
  origin_row_end <- data_end[
    exp_iso == origin_country & imp_iso %in% row_countries & imp_iso != origin_country,
    .(origin_row_val_end = sum(value_imp_pref, na.rm = TRUE)),
    by = commodity
  ]

  # Build commodity-level C3 table
  c3_commodity <- merge(data.table(commodity = all_commodities),
                        total_row_start, by = "commodity", all.x = TRUE)
  c3_commodity <- merge(c3_commodity, total_row_end,    by = "commodity", all.x = TRUE)
  c3_commodity <- merge(c3_commodity, origin_row_start, by = "commodity", all.x = TRUE)
  c3_commodity <- merge(c3_commodity, origin_row_end,   by = "commodity", all.x = TRUE)
  setnafill(c3_commodity, fill = 0,
            cols = c("total_row_start", "total_row_end",
                     "origin_row_val_start", "origin_row_val_end"))

  c3_commodity[, origin_row_share_start := fifelse(
    total_row_start == 0, 0, origin_row_val_start / total_row_start
  )]
  c3_commodity[, origin_row_share_end := fifelse(
    total_row_end == 0, 0, origin_row_val_end / total_row_end
  )]

  routes <- merge(routes,
                  c3_commodity[, .(commodity, total_row_start, total_row_end,
                                  origin_row_share_start, origin_row_share_end)],
                  by = "commodity", all.x = TRUE)

  # --- TC's exports to ROW per (commodity, TC), excluding self-trade ---
  tc_row_start <- data_start[
    exp_iso %in% tc_list & imp_iso %in% row_countries & imp_iso != exp_iso,
    .(tc_row_val_start = sum(value_imp_pref, na.rm = TRUE)),
    by = .(commodity, third_country = exp_iso)
  ]
  tc_row_end <- data_end[
    exp_iso %in% tc_list & imp_iso %in% row_countries & imp_iso != exp_iso,
    .(tc_row_val_end = sum(value_imp_pref, na.rm = TRUE)),
    by = .(commodity, third_country = exp_iso)
  ]

  routes <- merge(routes, tc_row_start, by = c("commodity", "third_country"), all.x = TRUE)
  routes <- merge(routes, tc_row_end,   by = c("commodity", "third_country"), all.x = TRUE)
  setnafill(routes, fill = 0, cols = c("tc_row_val_start", "tc_row_val_end"))

  routes[, tc_row_share_start := fifelse(
    total_row_start == 0, 0, tc_row_val_start / total_row_start
  )]
  routes[, tc_row_share_end := fifelse(
    total_row_end == 0, 0, tc_row_val_end / total_row_end
  )]

  # Growth values
  routes[, `:=`(
    origin_row_growth = origin_row_share_end - origin_row_share_start,
    tc_row_growth     = tc_row_share_end - tc_row_share_start
  )]

  # Apply Criterion 3 with small-share edge cases
  routes[, criterion3 := {
    origin_below <- origin_row_share_start < MIN_SHARE_FOR_GROWTH
    tc_below     <- tc_row_share_start < MIN_SHARE_FOR_GROWTH

    fifelse(
      origin_below & tc_below,
      # Both below minimum: compare end-year shares
      as.integer(origin_row_share_end > tc_row_share_end),
      fifelse(
        origin_below & !tc_below,
        # Only origin below minimum: cannot pass
        0L,
        # Origin has sufficient baseline: compare absolute growth
        as.integer(origin_row_growth > tc_row_growth)
      )
    )
  }]

  cat(sprintf("    Routes passing C3: %s / %s\n",
              format(sum(routes$criterion3), big.mark = ","),
              format(nrow(routes), big.mark = ",")))

  # ==========================================================================
  # CRITERION 4: Volume plausibility (raw end-year values per Freund)
  # TC imports from origin ≥ 75% of destination imports from TC
  # ==========================================================================

  cat("  Computing Criterion 4 (volume plausibility)...\n")

  # --- Origin -> TC trade values ---
  origin_to_tc_start <- data_start[
    exp_iso == origin_country & imp_iso %in% tc_list,
    .(commodity, third_country = imp_iso,
      origin_to_tc_val_start = value_imp_pref)
  ]
  origin_to_tc_end <- data_end[
    exp_iso == origin_country & imp_iso %in% tc_list,
    .(commodity, third_country = imp_iso,
      origin_to_tc_val_end = value_imp_pref)
  ]

  routes <- merge(routes, origin_to_tc_start, by = c("commodity", "third_country"), all.x = TRUE)
  routes <- merge(routes, origin_to_tc_end,   by = c("commodity", "third_country"), all.x = TRUE)

  # --- TC -> Destination trade values ---
  tc_to_dest_start <- data_start[
    imp_iso == DESTINATION_COUNTRY & exp_iso %in% tc_list,
    .(commodity, third_country = exp_iso,
      tc_to_dest_val_start = value_imp_pref)
  ]
  tc_to_dest_end <- data_end[
    imp_iso == DESTINATION_COUNTRY & exp_iso %in% tc_list,
    .(commodity, third_country = exp_iso,
      tc_to_dest_val_end = value_imp_pref)
  ]

  routes <- merge(routes, tc_to_dest_start, by = c("commodity", "third_country"), all.x = TRUE)
  routes <- merge(routes, tc_to_dest_end,   by = c("commodity", "third_country"), all.x = TRUE)

  # Fill NAs
  c4_val_cols <- c("origin_to_tc_val_start", "origin_to_tc_val_end",
                   "tc_to_dest_val_start", "tc_to_dest_val_end")
  setnafill(routes, fill = 0, cols = c4_val_cols)

  # Criterion 4: raw end-year comparison per Freund
  routes[, criterion4 := as.integer(
    origin_to_tc_val_end >= VOLUME_RATIO_THRESHOLD * tc_to_dest_val_end
  )]

  cat(sprintf("    Routes passing C4: %s / %s\n",
              format(sum(routes$criterion4), big.mark = ","),
              format(nrow(routes), big.mark = ",")))

  # --------------------------------------------------------------------------
  # COMPOSITE FLAG
  # --------------------------------------------------------------------------

  routes[, all_criteria := as.integer(
    criterion2 == 1L & criterion3 == 1L & criterion4 == 1L
  )]

  n_pass <- sum(routes$all_criteria, na.rm = TRUE)
  cat(sprintf("\n  Routes passing ALL criteria: %s / %s\n",
              format(n_pass, big.mark = ","),
              format(nrow(routes), big.mark = ",")))

  # --------------------------------------------------------------------------
  # TRANSSHIPMENT VALUE COMPONENTS (for downstream use)
  # Freund: "transshipment is defined as the minimum of China's exports to
  # the third country or third country exports to U.S."
  # --------------------------------------------------------------------------

  routes[, transshipment_value := fifelse(
    all_criteria == 1L,
    pmin(origin_to_tc_val_end, tc_to_dest_val_end),
    NA_real_
  )]

  # --------------------------------------------------------------------------
  # ATTACH COMMODITY DESCRIPTION IF AVAILABLE
  # --------------------------------------------------------------------------

  if ("description" %in% names(trade_data)) {
    desc_lookup <- unique(trade_data[, .(commodity, description)])[
      , .SD[1], by = commodity
    ]
    routes <- merge(routes, desc_lookup, by = "commodity", all.x = TRUE)
  }

  # Store
  all_route_results[[oc_idx]] <- routes

  elapsed <- as.numeric(difftime(Sys.time(), origin_timer, units = "secs"))
  cat(sprintf("  [%s] completed in %.1f seconds\n\n", origin_country, elapsed))
}

# ==============================================================================
# COMBINE AND SAVE
# ==============================================================================

cat("==============================================================================\n")
cat("COMBINING RESULTS\n")
cat("==============================================================================\n\n")

flagged_routes <- rbindlist(all_route_results, use.names = TRUE, fill = TRUE)

# Reorder columns for clarity
id_cols <- c("origin_country", "commodity", "third_country")
if ("description" %in% names(flagged_routes)) id_cols <- c(id_cols, "description")

flag_cols <- c("criterion2", "criterion3", "criterion4", "all_criteria")

c2_cols <- c("origin_dest_share_start", "origin_dest_share_end",
             "origin_tc_share_start", "origin_tc_share_end",
             "tc_dest_share_start", "tc_dest_share_end")

c3_cols <- c("origin_row_share_start", "origin_row_share_end", "origin_row_growth",
             "tc_row_share_start", "tc_row_share_end", "tc_row_growth",
             "tc_row_val_start", "tc_row_val_end",
             "total_row_start", "total_row_end")

c4_cols <- c("origin_to_tc_val_start", "origin_to_tc_val_end",
             "tc_to_dest_val_start", "tc_to_dest_val_end")

value_cols <- c("transshipment_value")

col_order <- c(id_cols, flag_cols, c2_cols, c3_cols, c4_cols, value_cols)
col_order <- intersect(col_order, names(flagged_routes))
remaining <- setdiff(names(flagged_routes), col_order)
setcolorder(flagged_routes, c(col_order, remaining))

# Summary
cat(sprintf("Total routes evaluated: %s\n", format(nrow(flagged_routes), big.mark = ",")))
cat(sprintf("Routes passing all criteria: %s\n",
            format(sum(flagged_routes$all_criteria, na.rm = TRUE), big.mark = ",")))

cat("\nCriterion pass rates:\n")
for (cc in c("criterion2", "criterion3", "criterion4")) {
  n_pass <- sum(flagged_routes[[cc]], na.rm = TRUE)
  pct <- round(100 * n_pass / nrow(flagged_routes), 2)
  cat(sprintf("  %s: %s (%s%%)\n", cc, format(n_pass, big.mark = ","), pct))
}

# Save
output_file <- file.path(OUTPUT_DIR,
                          sprintf("flagged_routes_%s_to_%s_%d_%d.csv",
                                  paste(ORIGIN_COUNTRIES, collapse = "_"),
                                  DESTINATION_COUNTRY,
                                  START_YEAR, END_YEAR))

fwrite(flagged_routes, output_file)
cat(sprintf("\nFlagged route dataset saved to: %s\n", output_file))

cat("\n==============================================================================\n")
cat("STEP 1 COMPLETE\n")
cat("==============================================================================\n")
