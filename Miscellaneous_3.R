# ==============================================================================
# TRANSSHIPMENT DETECTION ANALYSIS - PARAMETERIZED VERSION
# ==============================================================================
# Based on Freund et al. methodology with configurable parameters
# Detects trade transshipment through third countries using four criteria
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
START_YEAR <- 2018            # Start year for analysis
END_YEAR <- 2023              # End year for analysis

# Origin country handling
# When multiple origin countries are specified, should they be allowed as third countries for each other?
# TRUE: Germany → France → USA would be analyzed (detects intra-bloc transshipment)
# FALSE: France excluded when analyzing Germany (only external third countries)
ALLOW_ORIGIN_COUNTRIES_AS_TC <- FALSE  # Default: exclude origin countries from TC list

# Processing parameters
CHUNK_SIZE <- 50              # Number of commodities to process at once
MIN_SHARE_FOR_GROWTH <- 0     # Minimum share threshold for Criterion 3 growth calculation

# Criterion thresholds
VOLUME_RATIO_THRESHOLD <- 0.75  # Criterion 4: Origin→TC must be ≥ 75% of TC→Dest

# ------------------------------------------------------------------------------
# PACKAGE LOADING
# ------------------------------------------------------------------------------

library(data.table)
library(dplyr)

cat("==============================================================================\n")
cat("TRANSSHIPMENT DETECTION ANALYSIS\n")
cat("==============================================================================\n\n")

cat("Configuration:\n")
cat(sprintf("  Input file: %s\n", INPUT_FILE_PATH))
cat(sprintf("  Origin countries: %s\n", paste(ORIGIN_COUNTRIES, collapse=", ")))
cat(sprintf("  Destination: %s\n", DESTINATION_COUNTRY))
cat(sprintf("  Period: %d to %d\n", START_YEAR, END_YEAR))
cat(sprintf("  Allow origins as third countries: %s\n", ALLOW_ORIGIN_COUNTRIES_AS_TC))
cat(sprintf("  Chunk size: %d commodities\n\n", CHUNK_SIZE))

# ------------------------------------------------------------------------------
# DATA LOADING AND PREPARATION
# ------------------------------------------------------------------------------

cat("Loading trade data...\n")
trade_data <- fread(INPUT_FILE_PATH)

# Ensure required columns exist
required_cols <- c("commodity", "year", "imp_iso", "exp_iso", 
                   "value_importer", "value_exporter")
missing_cols <- setdiff(required_cols, names(trade_data))
if (length(missing_cols) > 0) {
  stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse=", ")))
}

# Calculate value_imp_pref (importer-preferred values)
trade_data[, value_avg := rowSums(cbind(value_importer, value_exporter), na.rm = TRUE) / 2]
trade_data[, value_imp_pref := ifelse(value_importer == 0 | is.na(value_importer), 
                                       value_exporter, 
                                       value_importer)]
trade_data[, value_exp_pref := ifelse(value_exporter == 0 | is.na(value_exporter), 
                                       value_importer, 
                                       value_exporter)]

# Convert to data.table and filter
trade_data <- as.data.table(trade_data)
trade_data <- trade_data[imp_iso != exp_iso]  # Exclude re-imports/exports

cat(sprintf("Loaded %s trade records\n", format(nrow(trade_data), big.mark=",")))

# Filter for analysis years only
data_filtered <- trade_data[year %in% c(START_YEAR, END_YEAR)]

# Calculate total imports and shares
data_filtered[, total_value := sum(value_imp_pref, na.rm = TRUE), 
              by = .(imp_iso, commodity, year)]
data_filtered[, share := value_imp_pref / total_value]

# Get lists
all_commodities <- unique(data_filtered$commodity)

# Define third countries based on configuration
if (ALLOW_ORIGIN_COUNTRIES_AS_TC) {
  # Allow origin countries to serve as third countries for each other
  third_countries <- unique(data_filtered$imp_iso[!(data_filtered$imp_iso %in% c(DESTINATION_COUNTRY))])
  tc_description <- "all countries except destination"
} else {
  # Exclude all origin countries from third country list
  third_countries <- unique(data_filtered$imp_iso[!(data_filtered$imp_iso %in% c(DESTINATION_COUNTRY, ORIGIN_COUNTRIES))])
  tc_description <- "all countries except destination and origins"
}

cat(sprintf("Analysis scope:\n"))
cat(sprintf("  Commodities: %s\n", format(length(all_commodities), big.mark=",")))
cat(sprintf("  Third countries (%s): %s\n\n", tc_description, format(length(third_countries), big.mark=",")))

# ------------------------------------------------------------------------------
# CHUNKED PROCESSING SETUP
# ------------------------------------------------------------------------------

n_chunks <- ceiling(length(all_commodities) / CHUNK_SIZE)

cat("==============================================================================\n")
cat("STEP 2: CHUNKED PROCESSING\n")
cat("==============================================================================\n\n")
cat(sprintf("Processing %s commodities in %d chunks of up to %d\n\n", 
            format(length(all_commodities), big.mark=","), 
            n_chunks, 
            CHUNK_SIZE))

# Initialize results storage
all_results <- list()

# ------------------------------------------------------------------------------
# MAIN PROCESSING LOOP - BY ORIGIN COUNTRY
# ------------------------------------------------------------------------------

for (origin_country in ORIGIN_COUNTRIES) {
  
  cat(sprintf("\n==============================================================================\n"))
  cat(sprintf("PROCESSING ORIGIN COUNTRY: %s\n", origin_country))
  cat(sprintf("==============================================================================\n\n"))
  
  origin_start_time <- Sys.time()
  
  # Process in chunks
  for (chunk_num in 1:n_chunks) {
    
    # Get commodities for this chunk
    start_idx <- (chunk_num - 1) * CHUNK_SIZE + 1
    end_idx <- min(chunk_num * CHUNK_SIZE, length(all_commodities))
    chunk_commodities <- all_commodities[start_idx:end_idx]
    
    cat(sprintf("Processing chunk %d/%d - Commodities %d to %d... ", 
                chunk_num, n_chunks, start_idx, end_idx))
    
    # Filter data for this chunk
    chunk_data <- data_filtered[commodity %in% chunk_commodities]
    
    # Initialize results storage for this chunk
    chunk_results <- list()
    chunk_routes_count <- 0
    
    # ------------------------------------------------------------------------------
    # COMMODITY LOOP
    # ------------------------------------------------------------------------------
    
    for (comm in chunk_commodities) {
      
      # Get commodity-specific data
      comm_data <- chunk_data[commodity == comm]
      
      # Pre-calculate shares for this commodity
      shares_yr_start <- comm_data[year == START_YEAR, .(imp_iso, exp_iso, share)]
      shares_yr_end <- comm_data[year == END_YEAR, .(imp_iso, exp_iso, share)]
      
      # Process each third country for this commodity
      tc_results <- data.table(
        commodity = comm,
        third_country = third_countries
      )
      
      # ------------------------------------------------------------------------------
      # CRITERION 2: Calculate share changes vectorially
      # ------------------------------------------------------------------------------
      
      # Origin's share of destination imports
      origin_dest_yr_start <- shares_yr_start[imp_iso == DESTINATION_COUNTRY & exp_iso == origin_country, share]
      origin_dest_yr_start <- ifelse(length(origin_dest_yr_start) == 0, 0, origin_dest_yr_start)
      
      origin_dest_yr_end <- shares_yr_end[imp_iso == DESTINATION_COUNTRY & exp_iso == origin_country, share]
      origin_dest_yr_end <- ifelse(length(origin_dest_yr_end) == 0, 0, origin_dest_yr_end)
      
      tc_results[, `:=`(
        origin_dest_yr_start = origin_dest_yr_start,
        origin_dest_yr_end = origin_dest_yr_end
      )]
      
      # Third countries' shares of destination imports
      tc_dest_shares_yr_start <- shares_yr_start[imp_iso == DESTINATION_COUNTRY & exp_iso %in% third_countries]
      tc_dest_shares_yr_end <- shares_yr_end[imp_iso == DESTINATION_COUNTRY & exp_iso %in% third_countries]
      
      tc_results <- merge(tc_results,
                          tc_dest_shares_yr_start[, .(third_country = exp_iso, tc_dest_yr_start = share)],
                          by = "third_country", all.x = TRUE)
      tc_results <- merge(tc_results,
                          tc_dest_shares_yr_end[, .(third_country = exp_iso, tc_dest_yr_end = share)],
                          by = "third_country", all.x = TRUE)
      
      # Origin's shares of third country imports
      origin_tc_shares_yr_start <- shares_yr_start[exp_iso == origin_country & imp_iso %in% third_countries]
      origin_tc_shares_yr_end <- shares_yr_end[exp_iso == origin_country & imp_iso %in% third_countries]
      
      tc_results <- merge(tc_results,
                          origin_tc_shares_yr_start[, .(third_country = imp_iso, origin_tc_yr_start = share)],
                          by = "third_country", all.x = TRUE)
      tc_results <- merge(tc_results,
                          origin_tc_shares_yr_end[, .(third_country = imp_iso, origin_tc_yr_end = share)],
                          by = "third_country", all.x = TRUE)
      
      # Replace NAs with 0
      cols_to_fill <- c("tc_dest_yr_start", "tc_dest_yr_end", "origin_tc_yr_start", "origin_tc_yr_end")
      tc_results[, (cols_to_fill) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), 
                 .SDcols = cols_to_fill]
      
      # Check criterion 2
      tc_results[, criterion2 := (origin_dest_yr_end < origin_dest_yr_start) &
                                  (origin_tc_yr_end > origin_tc_yr_start) &
                                  (tc_dest_yr_end > tc_dest_yr_start)]
      
      # Filter to only those passing criterion 2
      tc_results <- tc_results[criterion2 == TRUE]
      
      if (nrow(tc_results) == 0) next
      
      # ------------------------------------------------------------------------------
      # CRITERION 3: ROW analysis (vectorized)
      # ------------------------------------------------------------------------------
      
      # Define ROW (Rest of World) countries
      if (ALLOW_ORIGIN_COUNTRIES_AS_TC) {
        row_countries <- unique(comm_data$imp_iso[!comm_data$imp_iso %in% c(DESTINATION_COUNTRY)])
      } else {
        row_countries <- unique(comm_data$imp_iso[!comm_data$imp_iso %in% c(DESTINATION_COUNTRY, ORIGIN_COUNTRIES)])
      }
      
      # Total ROW imports (scalar per year)
      total_row_yr_start <- comm_data[year == START_YEAR & imp_iso %in% row_countries, 
                                      sum(value_imp_pref, na.rm = TRUE)]
      total_row_yr_end <- comm_data[year == END_YEAR & imp_iso %in% row_countries, 
                                    sum(value_imp_pref, na.rm = TRUE)]
      
      # Origin to ROW (scalar per year - origin exports to all ROW countries)
      origin_row_yr_start <- comm_data[year == START_YEAR & exp_iso == origin_country & 
                                        imp_iso != origin_country & imp_iso %in% row_countries, 
                                       sum(value_imp_pref, na.rm = TRUE)]
      origin_row_yr_end <- comm_data[year == END_YEAR & exp_iso == origin_country & 
                                      imp_iso != origin_country & imp_iso %in% row_countries, 
                                     sum(value_imp_pref, na.rm = TRUE)]
      
      # Origin's share of ROW imports
      origin_share_row_yr_start <- ifelse(total_row_yr_start == 0, 0, origin_row_yr_start / total_row_yr_start)
      origin_share_row_yr_end <- ifelse(total_row_yr_end == 0, 0, origin_row_yr_end / total_row_yr_end)
      origin_row_growth <- origin_share_row_yr_end - origin_share_row_yr_start
      
      # Third country exports to ROW (vectorized - one value per TC)
      tc_row_yr_start <- comm_data[year == START_YEAR & exp_iso %in% tc_results$third_country & 
                                    imp_iso %in% row_countries,
                                   .(tc_row_val_yr_start = sum(value_imp_pref, na.rm = TRUE)),
                                   by = .(third_country = exp_iso)]
      
      tc_row_yr_end <- comm_data[year == END_YEAR & exp_iso %in% tc_results$third_country & 
                                  imp_iso %in% row_countries,
                                 .(tc_row_val_yr_end = sum(value_imp_pref, na.rm = TRUE)),
                                 by = .(third_country = exp_iso)]
      
      # Merge TC ROW values
      tc_results <- merge(tc_results, tc_row_yr_start, by = "third_country", all.x = TRUE)
      tc_results <- merge(tc_results, tc_row_yr_end, by = "third_country", all.x = TRUE)
      
      # Fill NAs with 0
      tc_results[is.na(tc_row_val_yr_start), tc_row_val_yr_start := 0]
      tc_results[is.na(tc_row_val_yr_end), tc_row_val_yr_end := 0]
      
      # Calculate TC shares of ROW
      tc_results[, tc_share_row_yr_start := ifelse(total_row_yr_start == 0, 0, tc_row_val_yr_start / total_row_yr_start)]
      tc_results[, tc_share_row_yr_end := ifelse(total_row_yr_end == 0, 0, tc_row_val_yr_end / total_row_yr_end)]
      tc_results[, tc_row_growth := tc_share_row_yr_end - tc_share_row_yr_start]
      
      # Apply Criterion 3 logic vectorized
      tc_results[, criterion3 := fcase(
        # Both are new/tiny in base year - compare end year shares
        origin_share_row_yr_start < MIN_SHARE_FOR_GROWTH & tc_share_row_yr_start < MIN_SHARE_FOR_GROWTH,
          origin_share_row_yr_end > tc_share_row_yr_end,
        # Origin is new entrant, TC is established - fail
        origin_share_row_yr_start < MIN_SHARE_FOR_GROWTH,
          FALSE,
        # Both have sufficient baseline - use normal growth rates
        default = origin_row_growth > tc_row_growth
      )]
      
      # Filter to only those passing criterion 3
      tc_results <- tc_results[criterion3 == TRUE]
      
      if (nrow(tc_results) == 0) next
      
      # ------------------------------------------------------------------------------
      # CRITERION 4: Volume check
      # ------------------------------------------------------------------------------
      
      # Get trade values for year_end
      values_yr_start <- comm_data[year == START_YEAR, .(imp_iso, exp_iso, share, value_imp_pref)]
      values_yr_end <- comm_data[year == END_YEAR, .(imp_iso, exp_iso, share, value_imp_pref)]
      
      # Origin to third countries
      origin_to_tc_yr_start <- values_yr_start[exp_iso == origin_country & imp_iso %in% tc_results$third_country,
                                                .(third_country = imp_iso, origin_to_tc_share_yr_start = share, 
                                                  origin_to_tc_val_yr_start = value_imp_pref)]
      
      origin_to_tc_yr_end <- values_yr_end[exp_iso == origin_country & imp_iso %in% tc_results$third_country,
                                            .(third_country = imp_iso, origin_to_tc_share_yr_end = share, 
                                              origin_to_tc_val_yr_end = value_imp_pref)]
      
      origin_to_tc <- merge(origin_to_tc_yr_start, origin_to_tc_yr_end, by = "third_country", all.x = TRUE)
      
      # World to third countries (total imports by each TC)
      world_to_tc_yr_start <- values_yr_start[imp_iso %in% tc_results$third_country,
                                               .(world_to_tc_val_yr_start = sum(value_imp_pref, na.rm = TRUE)),
                                               by = .(third_country = imp_iso)]
      
      world_to_tc_yr_end <- values_yr_end[imp_iso %in% tc_results$third_country,
                                           .(world_to_tc_val_yr_end = sum(value_imp_pref, na.rm = TRUE)),
                                           by = .(third_country = imp_iso)]
      
      world_to_tc <- merge(world_to_tc_yr_start, world_to_tc_yr_end, by = "third_country", all.x = TRUE)
      
      # Third countries to destination
      tc_to_dest_yr_start <- values_yr_start[imp_iso == DESTINATION_COUNTRY & exp_iso %in% tc_results$third_country,
                                              .(third_country = exp_iso, tc_to_dest_share_yr_start = share, 
                                                tc_to_dest_val_yr_start = value_imp_pref)]
      
      tc_to_dest_yr_end <- values_yr_end[imp_iso == DESTINATION_COUNTRY & exp_iso %in% tc_results$third_country,
                                          .(third_country = exp_iso, tc_to_dest_share_yr_end = share, 
                                            tc_to_dest_val_yr_end = value_imp_pref)]
      
      tc_to_dest <- merge(tc_to_dest_yr_start, tc_to_dest_yr_end, by = "third_country", all.x = TRUE)
      
      # World to destination (scalar totals for the commodity)
      world_to_dest_val_yr_start <- values_yr_start[imp_iso == DESTINATION_COUNTRY,
                                                     sum(value_imp_pref, na.rm = TRUE)]
      world_to_dest_val_yr_end <- values_yr_end[imp_iso == DESTINATION_COUNTRY,
                                                 sum(value_imp_pref, na.rm = TRUE)]
      
      # Join the different data frames of trade flows using data.table merges
      tc_results <- merge(tc_results, origin_to_tc, by = "third_country", all.x = TRUE)
      tc_results <- merge(tc_results, tc_to_dest, by = "third_country", all.x = TRUE)
      tc_results <- merge(tc_results, world_to_tc, by = "third_country", all.x = TRUE)
      
      # Add world_to_dest scalars as columns
      tc_results[, world_to_dest_val_yr_start := world_to_dest_val_yr_start]
      tc_results[, world_to_dest_val_yr_end := world_to_dest_val_yr_end]
      
      # Calculate excess flows
      tc_results[, origin_to_tc := origin_to_tc_val_yr_end - ((origin_to_tc_val_yr_start/world_to_tc_val_yr_start) * world_to_tc_val_yr_end)]
      tc_results[, tc_to_dest := tc_to_dest_val_yr_end - ((tc_to_dest_val_yr_start/world_to_dest_val_yr_start) * world_to_dest_val_yr_end)]
      
      # Replace NAs with 0
      tc_results[is.na(origin_to_tc), origin_to_tc := 0]
      tc_results[is.na(tc_to_dest), tc_to_dest := 0]
      
      # Check criterion 4
      tc_results[, criterion4 := origin_to_tc_val_yr_end >= VOLUME_RATIO_THRESHOLD * tc_to_dest_val_yr_end]
      
      # Filter to qualifying routes
      tc_results <- tc_results[criterion4 == TRUE]
      
      if (nrow(tc_results) > 0) {
        # Calculate transshipment value
        tc_results[, transshipment_value := pmin(pmax(origin_to_tc, 0), pmax(tc_to_dest, 0))]
        
        # Get description if available
        commodity_desc <- unique(comm_data[commodity == comm, description])
        if (length(commodity_desc) > 0) {
          tc_results[, description := commodity_desc[1]]
        } else {
          tc_results[, description := NA_character_]
        }
        
        # Track number of routes in this chunk
        chunk_routes_count <- chunk_routes_count + nrow(tc_results)
        
        # Add to chunk results
        chunk_results[[length(chunk_results) + 1]] <- tc_results[, .(
          origin_country = origin_country,
          commodity, description, third_country,
          origin_to_tc_val_yr_end, origin_to_tc_val_yr_start, world_to_tc_val_yr_start, world_to_tc_val_yr_end,
          origin_to_tc,
          tc_to_dest_val_yr_end, tc_to_dest_val_yr_start, world_to_dest_val_yr_start, world_to_dest_val_yr_end,
          tc_to_dest,
          transshipment_value
        )]
      }
    }
    
    # Report chunk completion
    if (chunk_routes_count > 0) {
      cat(sprintf("Found %s transshipment routes\n", format(chunk_routes_count, big.mark=",")))
    } else {
      cat("No transshipment detected\n")
    }
    
    # Combine chunk results
    if (length(chunk_results) > 0) {
      chunk_results_dt <- rbindlist(chunk_results)
      all_results[[length(all_results) + 1]] <- chunk_results_dt
    }
    
    # Clean up chunk objects
    rm(chunk_data, chunk_results)
  }
  
  origin_end_time <- Sys.time()
  origin_elapsed <- as.numeric(difftime(origin_end_time, origin_start_time, units = "secs"))
  
  cat(sprintf("\n[%s] Processing completed in %.1f seconds\n",
              origin_country, origin_elapsed))
}

# ------------------------------------------------------------------------------
# COMBINE AND OUTPUT RESULTS
# ------------------------------------------------------------------------------

cat("\n==============================================================================\n")
cat("FINALIZING RESULTS\n")
cat("==============================================================================\n\n")

if (length(all_results) > 0) {
  final_results <- rbindlist(all_results)
  
  # Calculate summary statistics by origin
  summary_by_origin <- final_results[, .(
    total_transshipment = sum(transshipment_value, na.rm = TRUE),
    n_routes = .N,
    n_third_countries = length(unique(third_country)),
    n_commodities = length(unique(commodity))
  ), by = origin_country]
  
  cat("=== RESULTS BY ORIGIN COUNTRY ===\n")
  for (i in 1:nrow(summary_by_origin)) {
    cat(sprintf("  %s: $%s (%s routes, %s commodities, %s third countries)\n",
                summary_by_origin$origin_country[i],
                format(round(summary_by_origin$total_transshipment[i]), big.mark=","),
                format(summary_by_origin$n_routes[i], big.mark=","),
                format(summary_by_origin$n_commodities[i], big.mark=","),
                format(summary_by_origin$n_third_countries[i], big.mark=",")))
  }
  cat("\n")
  
  # Calculate summary statistics by third country
  summary_by_tc <- final_results[, .(
    total_transshipment = sum(transshipment_value, na.rm = TRUE),
    n_routes = .N,
    n_origins = length(unique(origin_country)),
    n_commodities = length(unique(commodity))
  ), by = third_country]
  summary_by_tc <- summary_by_tc[order(-total_transshipment)]
  
  cat("=== TOP 20 THIRD COUNTRIES BY TRANSSHIPMENT VALUE ===\n")
  top_n <- min(20, nrow(summary_by_tc))
  for (i in 1:top_n) {
    cat(sprintf("  %2d. %s: $%s (%s routes, %s commodities)\n",
                i,
                summary_by_tc$third_country[i],
                format(round(summary_by_tc$total_transshipment[i]), big.mark=","),
                format(summary_by_tc$n_routes[i], big.mark=","),
                format(summary_by_tc$n_commodities[i], big.mark=",")))
  }
  cat("\n")
  
  # Calculate summary statistics by commodity
  summary_by_commodity <- final_results[, .(
    total_transshipment = sum(transshipment_value, na.rm = TRUE),
    n_routes = .N,
    n_third_countries = length(unique(third_country)),
    n_origins = length(unique(origin_country)),
    description = first(description)
  ), by = commodity]
  summary_by_commodity <- summary_by_commodity[order(-total_transshipment)]
  
  cat("=== TOP 20 COMMODITIES BY TRANSSHIPMENT VALUE ===\n")
  top_n <- min(20, nrow(summary_by_commodity))
  for (i in 1:top_n) {
    desc_text <- if (!is.na(summary_by_commodity$description[i])) {
      paste0(" (", substr(summary_by_commodity$description[i], 1, 50), ")")
    } else {
      ""
    }
    cat(sprintf("  %2d. %s%s: $%s (%s routes)\n",
                i,
                summary_by_commodity$commodity[i],
                desc_text,
                format(round(summary_by_commodity$total_transshipment[i]), big.mark=","),
                format(summary_by_commodity$n_routes[i], big.mark=",")))
  }
  cat("\n")
  
  # Calculate summary statistics by individual route
  summary_by_route <- final_results[, .(
    total_transshipment = sum(transshipment_value, na.rm = TRUE),
    description = first(description),
    origin_to_tc_excess = first(origin_to_tc),
    tc_to_dest_excess = first(tc_to_dest)
  ), by = .(origin_country, commodity, third_country)]
  summary_by_route <- summary_by_route[order(-total_transshipment)]
  
  cat("=== TOP 20 INDIVIDUAL ROUTES BY TRANSSHIPMENT VALUE ===\n")
  top_n <- min(20, nrow(summary_by_route))
  for (i in 1:top_n) {
    desc_text <- if (!is.na(summary_by_route$description[i])) {
      paste0(" - ", substr(summary_by_route$description[i], 1, 40))
    } else {
      ""
    }
    cat(sprintf("  %2d. %s → %s → %s: $%s\n",
                i,
                summary_by_route$origin_country[i],
                summary_by_route$third_country[i],
                DESTINATION_COUNTRY,
                format(round(summary_by_route$total_transshipment[i]), big.mark=",")))
    cat(sprintf("      Commodity: %s%s\n",
                summary_by_route$commodity[i],
                desc_text))
  }
  cat("\n")
  
  # Calculate summary statistics
  total_transshipment <- sum(final_results$transshipment_value, na.rm = TRUE)
  n_routes <- nrow(final_results)
  n_origins <- length(unique(final_results$origin_country))
  n_third_countries <- length(unique(final_results$third_country))
  
  cat("=== OVERALL STATISTICS ===\n")
  cat(sprintf("Total estimated transshipment value: $%s\n", 
              format(round(total_transshipment), big.mark=",")))
  cat(sprintf("Number of origin countries with transshipment: %d\n", n_origins))
  cat(sprintf("Number of third countries involved: %d\n", n_third_countries))
  cat(sprintf("Number of qualifying routes: %s\n\n", 
              format(n_routes, big.mark=",")))
  
  # Save detailed results
  output_file_detailed <- file.path(OUTPUT_DIR, 
                           sprintf("transshipment_results_%s_to_%s_%d_%d.csv",
                                   paste(ORIGIN_COUNTRIES, collapse="_"),
                                   DESTINATION_COUNTRY,
                                   START_YEAR,
                                   END_YEAR))
  
  fwrite(final_results, output_file_detailed)
  cat(sprintf("Detailed results saved to: %s\n", output_file_detailed))
  
  # Save summary by origin country
  output_file_by_origin <- file.path(OUTPUT_DIR, 
                                     sprintf("summary_by_origin_%s_to_%s_%d_%d.csv",
                                             paste(ORIGIN_COUNTRIES, collapse="_"),
                                             DESTINATION_COUNTRY,
                                             START_YEAR,
                                             END_YEAR))
  fwrite(summary_by_origin, output_file_by_origin)
  cat(sprintf("Summary by origin saved to: %s\n", output_file_by_origin))
  
  # Save summary by third country
  output_file_by_tc <- file.path(OUTPUT_DIR, 
                                 sprintf("summary_by_third_country_%s_to_%s_%d_%d.csv",
                                         paste(ORIGIN_COUNTRIES, collapse="_"),
                                         DESTINATION_COUNTRY,
                                         START_YEAR,
                                         END_YEAR))
  fwrite(summary_by_tc, output_file_by_tc)
  cat(sprintf("Summary by third country saved to: %s\n", output_file_by_tc))
  
  # Save summary by commodity
  output_file_by_commodity <- file.path(OUTPUT_DIR, 
                                        sprintf("summary_by_commodity_%s_to_%s_%d_%d.csv",
                                                paste(ORIGIN_COUNTRIES, collapse="_"),
                                                DESTINATION_COUNTRY,
                                                START_YEAR,
                                                END_YEAR))
  fwrite(summary_by_commodity, output_file_by_commodity)
  cat(sprintf("Summary by commodity saved to: %s\n", output_file_by_commodity))
  
  # Save summary by route
  output_file_by_route <- file.path(OUTPUT_DIR, 
                                    sprintf("summary_by_route_%s_to_%s_%d_%d.csv",
                                            paste(ORIGIN_COUNTRIES, collapse="_"),
                                            DESTINATION_COUNTRY,
                                            START_YEAR,
                                            END_YEAR))
  fwrite(summary_by_route, output_file_by_route)
  cat(sprintf("Summary by route saved to: %s\n", output_file_by_route))
  
  cat("\nAll summary files successfully saved!\n")
  
} else {
  cat("No transshipment routes detected across any origin countries.\n")
}

cat("\n==============================================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("==============================================================================\n")
