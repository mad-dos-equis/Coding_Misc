# ==============================================================================
# TRANSSHIPMENT DETECTION - STEP 2: ESTIMATE TRANSSHIPMENT VALUES
# ==============================================================================
# Uses output from Step 1 (flagged_routes) to estimate transshipment values
# following Freund (2025) "The China Wash" methodology.
#
# Estimation method:
#   For each route passing all criteria (C2, C3, C4):
#     transshipment_value = min(origin→TC value, TC→dest value)
#   This "bottleneck" approach reflects that the transshipped quantity cannot
#   exceed either leg of the route.
#
# Output aggregations:
#   1. Route-level:         origin × commodity × third_country
#   2. By commodity:        origin × commodity  (summed across TCs)
#   3. By third country:    origin × third_country (summed across commodities)
#
# ==============================================================================

# ------------------------------------------------------------------------------
# CONFIGURATION SECTION
# ------------------------------------------------------------------------------

# File paths
INPUT_FILE_PATH <- "path/to/flagged_routes_CHN_to_USA_2018_2023.csv"  # Step 1 output
OUTPUT_DIR      <- "path/to/output/directory"

# Labels (used in output filenames)
ORIGIN_LABEL      <- "CHN"       # For filename construction
DESTINATION_LABEL <- "USA"
START_YEAR        <- 2018
END_YEAR          <- 2023

# ------------------------------------------------------------------------------
# PACKAGE LOADING
# ------------------------------------------------------------------------------

library(data.table)

cat("==============================================================================\n")
cat("TRANSSHIPMENT DETECTION - STEP 2: ESTIMATE TRANSSHIPMENT VALUES\n")
cat("Methodology: Freund (2025) 'The China Wash' — Bottleneck Estimation\n")
cat("==============================================================================\n\n")

cat("Configuration:\n")
cat(sprintf("  Input file: %s\n", INPUT_FILE_PATH))
cat(sprintf("  Output dir: %s\n\n", OUTPUT_DIR))

# ------------------------------------------------------------------------------
# DATA LOADING
# ------------------------------------------------------------------------------

cat("Loading flagged route data...\n")
flagged_routes <- fread(INPUT_FILE_PATH)
setDT(flagged_routes)

cat(sprintf("  Total routes loaded: %s\n", format(nrow(flagged_routes), big.mark = ",")))
cat(sprintf("  Routes passing all criteria: %s\n\n",
            format(sum(flagged_routes$all_criteria == 1L, na.rm = TRUE), big.mark = ",")))

# Validate required columns
required_cols <- c("origin_country", "commodity", "third_country",
                   "all_criteria", "origin_to_tc_val_end", "tc_to_dest_val_end")
missing_cols <- setdiff(required_cols, names(flagged_routes))
if (length(missing_cols) > 0) {
  stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
}

# ------------------------------------------------------------------------------
# ESTIMATE TRANSSHIPMENT VALUES (ROUTE LEVEL)
# ------------------------------------------------------------------------------

cat("Computing transshipment estimates...\n\n")

# Filter to routes passing all criteria
ts_routes <- flagged_routes[all_criteria == 1L]

# Bottleneck estimation: min of the two legs (end-year values)
ts_routes[, transshipment_value := pmin(origin_to_tc_val_end, tc_to_dest_val_end)]

# Identify the binding leg for reference
ts_routes[, binding_leg := fifelse(
  origin_to_tc_val_end <= tc_to_dest_val_end,
  "origin_to_tc",
  "tc_to_dest"
)]

cat(sprintf("  Flagged routes with estimates: %s\n",
            format(nrow(ts_routes), big.mark = ",")))
cat(sprintf("  Total estimated transshipment: $%s\n",
            format(round(sum(ts_routes$transshipment_value, na.rm = TRUE)),
                   big.mark = ",")))
cat(sprintf("  Binding leg breakdown:\n"))
cat(sprintf("    origin→TC binding:  %s routes\n",
            format(sum(ts_routes$binding_leg == "origin_to_tc"), big.mark = ",")))
cat(sprintf("    TC→dest binding:    %s routes\n\n",
            format(sum(ts_routes$binding_leg == "tc_to_dest"), big.mark = ",")))

# ==============================================================================
# AGGREGATION 1: ROUTE LEVEL (origin × commodity × third_country)
# ==============================================================================

cat("--- Aggregation 1: Route Level ---\n")

# Select output columns
route_id_cols <- c("origin_country", "commodity", "third_country")
if ("description" %in% names(ts_routes)) route_id_cols <- c(route_id_cols, "description")

route_value_cols <- c("transshipment_value", "binding_leg",
                      "origin_to_tc_val_end", "tc_to_dest_val_end",
                      "origin_to_tc_val_start", "tc_to_dest_val_start")
route_value_cols <- intersect(route_value_cols, names(ts_routes))

ts_by_route <- ts_routes[, .SD, .SDcols = c(route_id_cols, route_value_cols)]

# Sort by estimated value descending
setorderv(ts_by_route, "transshipment_value", order = -1L)

cat(sprintf("  Routes: %s\n", format(nrow(ts_by_route), big.mark = ",")))
cat(sprintf("  Total:  $%s\n\n",
            format(round(sum(ts_by_route$transshipment_value)), big.mark = ",")))

# ==============================================================================
# AGGREGATION 2: BY COMMODITY (origin × commodity, summed across TCs)
# ==============================================================================

cat("--- Aggregation 2: By Commodity ---\n")

# Group by columns
commodity_group <- c("origin_country", "commodity")
if ("description" %in% names(ts_routes)) commodity_group <- c(commodity_group, "description")

ts_by_commodity <- ts_routes[, .(
  transshipment_value   = sum(transshipment_value, na.rm = TRUE),
  n_third_countries     = uniqueN(third_country),
  third_countries       = paste(sort(unique(third_country)), collapse = "; "),
  sum_origin_to_tc_end  = sum(origin_to_tc_val_end, na.rm = TRUE),
  sum_tc_to_dest_end    = sum(tc_to_dest_val_end, na.rm = TRUE)
), by = commodity_group]

setorderv(ts_by_commodity, "transshipment_value", order = -1L)

cat(sprintf("  Commodities: %s\n", format(nrow(ts_by_commodity), big.mark = ",")))
cat(sprintf("  Total:       $%s\n\n",
            format(round(sum(ts_by_commodity$transshipment_value)), big.mark = ",")))

# ==============================================================================
# AGGREGATION 3: BY THIRD COUNTRY (origin × third_country, summed across commodities)
# ==============================================================================

cat("--- Aggregation 3: By Third Country ---\n")

ts_by_tc <- ts_routes[, .(
  transshipment_value    = sum(transshipment_value, na.rm = TRUE),
  n_commodities          = uniqueN(commodity),
  sum_origin_to_tc_end   = sum(origin_to_tc_val_end, na.rm = TRUE),
  sum_tc_to_dest_end     = sum(tc_to_dest_val_end, na.rm = TRUE)
), by = .(origin_country, third_country)]

setorderv(ts_by_tc, "transshipment_value", order = -1L)

cat(sprintf("  Third countries: %s\n", format(nrow(ts_by_tc), big.mark = ",")))
cat(sprintf("  Total:           $%s\n\n",
            format(round(sum(ts_by_tc$transshipment_value)), big.mark = ",")))

# Print top third countries
cat("  Top third countries by estimated transshipment:\n")
top_n <- min(10, nrow(ts_by_tc))
for (i in seq_len(top_n)) {
  cat(sprintf("    %2d. %s: $%s (%d commodities)\n",
              i,
              ts_by_tc$third_country[i],
              format(round(ts_by_tc$transshipment_value[i]), big.mark = ","),
              ts_by_tc$n_commodities[i]))
}
cat("\n")

# ==============================================================================
# SAVE OUTPUTS
# ==============================================================================

cat("==============================================================================\n")
cat("SAVING OUTPUTS\n")
cat("==============================================================================\n\n")

file_suffix <- sprintf("%s_to_%s_%d_%d",
                       ORIGIN_LABEL, DESTINATION_LABEL, START_YEAR, END_YEAR)

# Route-level
output_route <- file.path(OUTPUT_DIR, sprintf("transshipment_by_route_%s.csv", file_suffix))
fwrite(ts_by_route, output_route)
cat(sprintf("  Route-level:      %s\n", output_route))

# By commodity
output_commodity <- file.path(OUTPUT_DIR, sprintf("transshipment_by_commodity_%s.csv", file_suffix))
fwrite(ts_by_commodity, output_commodity)
cat(sprintf("  By commodity:     %s\n", output_commodity))

# By third country
output_tc <- file.path(OUTPUT_DIR, sprintf("transshipment_by_third_country_%s.csv", file_suffix))
fwrite(ts_by_tc, output_tc)
cat(sprintf("  By third country: %s\n", output_tc))

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n==============================================================================\n")
cat("STEP 2 SUMMARY\n")
cat("==============================================================================\n\n")

origins <- unique(ts_routes$origin_country)
for (oc in origins) {
  oc_data <- ts_routes[origin_country == oc]
  cat(sprintf("Origin: %s\n", oc))
  cat(sprintf("  Total estimated transshipment: $%s\n",
              format(round(sum(oc_data$transshipment_value)), big.mark = ",")))
  cat(sprintf("  Flagged routes:     %s\n", format(nrow(oc_data), big.mark = ",")))
  cat(sprintf("  Unique commodities: %s\n", format(uniqueN(oc_data$commodity), big.mark = ",")))
  cat(sprintf("  Unique TCs:         %s\n\n", format(uniqueN(oc_data$third_country), big.mark = ",")))
}

cat("==============================================================================\n")
cat("STEP 2 COMPLETE\n")
cat("==============================================================================\n")
