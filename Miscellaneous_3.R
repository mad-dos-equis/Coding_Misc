################################################################################
# TRANSSHIPMENT ANALYSIS - MULTIPLE ORIGIN COUNTRIES
# 
# Identifies potential trade rerouting from multiple origin countries through 
# third countries to a destination (USA)
#
# Methodology based on Freund et al. (2025):
# https://china.ucsd.edu/_files/02072025-brief-identify-tariff-evasion-web.pdf
#
# Detection Criteria (Freund's ii, iii, iv):
# ii.  Three-way pattern: Origin→Dest decreased, Origin→TC increased, TC→Dest increased
# iii. Origin share growth in rest-of-world > TC share growth in rest-of-world
# iv.  TC imports from origin ≥ 75% of TC exports to destination
#
# Transshipment Value: Excess growth above market expectations (deviation from Freund)
#
# Author: Updated for multi-origin analysis
# Date: December 2025
################################################################################

# Load required libraries
library(data.table)
library(dplyr)

# Clear environment
rm(list = ls())
gc()

################################################################################
# CONFIGURATION PARAMETERS
################################################################################

# Define origin countries to analyze
origin_countries <- c("China", "Vietnam", "Mexico")

# Define destination country
destination_country <- "USA"

# Allow origin countries to serve as third countries for each other?
# FALSE = more conservative (recommended), excludes origin countries from third country analysis
# TRUE = allows origin countries to be potential transshipment hubs for each other
allow_origin_crossover <- FALSE

# Years to compare
start_year <- 2018  # Baseline/starting year
end_year <- 2023    # Comparison/ending year

# Chunk size for processing (adjust based on available memory)
chunk_size <- 50

# Output file path
output_file <- "transshipment_results_multi_origin.rds"

################################################################################
# LOAD AND PREPARE DATA
################################################################################

cat("Loading data...\n")

# Load your trade data
# Replace this with your actual data loading code
# trade_data <- fread("your_trade_data.csv")
# 
# Required columns:
# - importer: importing country
# - exporter: exporting country
# - commodity: commodity code/identifier
# - year: year of observation
# - value: trade value

# For this example, assuming trade_data is already loaded
# Ensure it's a data.table
trade_data <- as.data.table(trade_data)

cat(sprintf("Loaded %s rows of trade data\n", format(nrow(trade_data), big.mark = ",")))

# Keep only the two years we're comparing
trade_data <- trade_data[year %in% c(start_year, end_year)]

cat(sprintf("Filtered to years %d and %d: %s rows\n", 
            start_year, end_year, format(nrow(trade_data), big.mark = ",")))

################################################################################
# IDENTIFY THIRD COUNTRIES
################################################################################

cat("\nIdentifying third countries...\n")

# Get all unique exporters in the data
all_exporters <- unique(trade_data$exporter)

# Define third countries based on toggle parameter
if (allow_origin_crossover) {
  # Third countries = all countries except destination
  third_countries <- setdiff(all_exporters, destination_country)
  cat(sprintf("Origin crossover ALLOWED: %d third countries identified\n", 
              length(third_countries)))
} else {
  # Third countries = all countries except destination and origin countries
  third_countries <- setdiff(all_exporters, c(destination_country, origin_countries))
  cat(sprintf("Origin crossover EXCLUDED: %d third countries identified\n", 
              length(third_countries)))
}

################################################################################
# CALCULATE IMPORT SHARES
################################################################################

cat("\nCalculating import shares...\n")

# Calculate total imports by importer-commodity-year
total_imports <- trade_data[, .(total_value = sum(value, na.rm = TRUE)), 
                            by = .(importer, commodity, year)]

# Merge and calculate shares
trade_data <- merge(trade_data, total_imports, 
                    by = c("importer", "commodity", "year"),
                    all.x = TRUE)

trade_data[, share := value / total_value]
trade_data[is.na(share), share := 0]

################################################################################
# PREPARE DATA SUBSETS
################################################################################

cat("Preparing data subsets by year...\n")

# Calculate total imports by commodity-importer-year (for excess growth calculations)
# This represents "world" imports to each country for each commodity
total_imports_by_country <- trade_data[, .(
  total_imports = sum(value, na.rm = TRUE)
), by = .(commodity, importer, year)]

setkey(total_imports_by_country, commodity, importer, year)

# Calculate rest-of-world (ROW) total imports by commodity-year
# ROW = all importers except the destination country (for Freund criterion iii)
row_total_imports <- trade_data[importer != destination_country, .(
  row_total_imports = sum(value, na.rm = TRUE)
), by = .(commodity, year)]

setkey(row_total_imports, commodity, year)

# OPTIMIZATION: Pre-calculate ROW exports by exporter-commodity-year
# This avoids repeated aggregations in the innermost loop
cat("Pre-calculating ROW exports for criterion iii...\n")
row_exports <- trade_data[importer != destination_country, .(
  row_exports = sum(value, na.rm = TRUE)
), by = .(exporter, commodity, year)]

row_exports_start <- row_exports[year == start_year]
row_exports_end <- row_exports[year == end_year]
setkey(row_exports_start, exporter, commodity)
setkey(row_exports_end, exporter, commodity)

# Create separate datasets for each year
shares_start <- trade_data[year == start_year, 
                           .(importer, exporter, commodity, value, share)]
setkey(shares_start, commodity, importer, exporter)  # OPTIMIZATION: compound key

shares_end <- trade_data[year == end_year, 
                         .(importer, exporter, commodity, value, share)]
setkey(shares_end, commodity, importer, exporter)  # OPTIMIZATION: compound key

# Get unique commodities
all_commodities <- unique(trade_data$commodity)
n_commodities <- length(all_commodities)

cat(sprintf("Total commodities to process: %s\n", format(n_commodities, big.mark = ",")))

################################################################################
# MAIN TRANSSHIPMENT ANALYSIS - BY ORIGIN COUNTRY
################################################################################

cat("\n=== BEGINNING TRANSSHIPMENT ANALYSIS ===\n\n")

# Initialize list to store results for each origin country
all_results <- list()

# Process each origin country separately
for (origin_idx in seq_along(origin_countries)) {
  
  origin_country <- origin_countries[origin_idx]
  
  cat(sprintf("\n--- ANALYZING ORIGIN COUNTRY %d/%d: %s ---\n", 
              origin_idx, length(origin_countries), origin_country))
  
  # Initialize results list for this origin
  origin_results <- list()
  
  # Create chunks of commodities
  n_chunks <- ceiling(n_commodities / chunk_size)
  
  # Process commodities in chunks
  for (chunk_idx in 1:n_chunks) {
    
    # Get commodity indices for this chunk
    start_idx <- (chunk_idx - 1) * chunk_size + 1
    end_idx <- min(chunk_idx * chunk_size, n_commodities)
    chunk_commodities <- all_commodities[start_idx:end_idx]
    
    cat(sprintf("  [%s] Processing chunk %d/%d (commodities %d-%d)...",
                origin_country, chunk_idx, n_chunks, start_idx, end_idx))
    
    # Track results before this chunk
    results_before_chunk <- length(origin_results)
    
    # Filter data for this chunk
    comm_data_start <- shares_start[commodity %in% chunk_commodities]
    comm_data_end <- shares_end[commodity %in% chunk_commodities]
    
    # Process each commodity in the chunk
    for (comm in chunk_commodities) {
      
      # Get data for this commodity
      comm_start <- comm_data_start[commodity == comm]
      comm_end <- comm_data_end[commodity == comm]
      
      # Check each third country as potential transshipment hub
      for (tc in third_countries) {
        
        # ===================================================================
        # GET TRADE VALUES AND SHARES (OPTIMIZED WITH KEYED LOOKUPS)
        # ===================================================================
        
        # Origin→Third Country (TC)
        origin_to_tc_val_start <- shares_start[.(comm, tc, origin_country), value]
        origin_to_tc_val_end <- shares_end[.(comm, tc, origin_country), value]
        
        if (length(origin_to_tc_val_start) == 0 || is.na(origin_to_tc_val_start)) origin_to_tc_val_start <- 0
        if (length(origin_to_tc_val_end) == 0 || is.na(origin_to_tc_val_end)) origin_to_tc_val_end <- 0
        
        # OPTIMIZATION: Early exit if no trade on origin→TC leg in end year
        if (origin_to_tc_val_end == 0) next
        
        # Third Country→Destination
        tc_to_dest_val_start <- shares_start[.(comm, destination_country, tc), value]
        tc_to_dest_val_end <- shares_end[.(comm, destination_country, tc), value]
        
        if (length(tc_to_dest_val_start) == 0 || is.na(tc_to_dest_val_start)) tc_to_dest_val_start <- 0
        if (length(tc_to_dest_val_end) == 0 || is.na(tc_to_dest_val_end)) tc_to_dest_val_end <- 0
        
        # OPTIMIZATION: Early exit if no trade on TC→dest leg in end year
        if (tc_to_dest_val_end == 0) next
        
        # Origin→Destination (direct)
        origin_to_dest_val_start <- shares_start[.(comm, destination_country, origin_country), value]
        origin_to_dest_val_end <- shares_end[.(comm, destination_country, origin_country), value]
        
        if (length(origin_to_dest_val_start) == 0 || is.na(origin_to_dest_val_start)) origin_to_dest_val_start <- 0
        if (length(origin_to_dest_val_end) == 0 || is.na(origin_to_dest_val_end)) origin_to_dest_val_end <- 0
        
        # Total ("world") imports by commodity-importer-year (for excess growth)
        world_to_tc_val_start <- total_imports_by_country[
          .(comm, tc, start_year), total_imports]
        world_to_tc_val_end <- total_imports_by_country[
          .(comm, tc, end_year), total_imports]
        world_to_dest_val_start <- total_imports_by_country[
          .(comm, destination_country, start_year), total_imports]
        world_to_dest_val_end <- total_imports_by_country[
          .(comm, destination_country, end_year), total_imports]
        
        if (length(world_to_tc_val_start) == 0 || is.na(world_to_tc_val_start)) world_to_tc_val_start <- 0
        if (length(world_to_tc_val_end) == 0 || is.na(world_to_tc_val_end)) world_to_tc_val_end <- 0
        if (length(world_to_dest_val_start) == 0 || is.na(world_to_dest_val_start)) world_to_dest_val_start <- 0
        if (length(world_to_dest_val_end) == 0 || is.na(world_to_dest_val_end)) world_to_dest_val_end <- 0
        
        # ===================================================================
        # FREUND CRITERION ii: THREE-WAY PATTERN
        # Origin→Dest decreased AND Origin→TC increased AND TC→Dest increased
        # ===================================================================
        
        criterion_ii <- (origin_to_dest_val_end < origin_to_dest_val_start) &
                        (origin_to_tc_val_end > origin_to_tc_val_start) &
                        (tc_to_dest_val_end > tc_to_dest_val_start)
        
        # OPTIMIZATION: Early exit if criterion ii fails (skip expensive ROW calculations)
        if (!criterion_ii) next
        
        # ===================================================================
        # FREUND CRITERION iii: REST-OF-WORLD COMPETITIVENESS
        # Origin's ROW share growth > TC's ROW share growth
        # (Excludes cases where TC is becoming globally competitive)
        # ===================================================================
        
        # Rest-of-world (ROW) total imports for this commodity
        row_total_start <- row_total_imports[.(comm, start_year), row_total_imports]
        row_total_end <- row_total_imports[.(comm, end_year), row_total_imports]
        
        if (length(row_total_start) == 0 || is.na(row_total_start)) row_total_start <- 0
        if (length(row_total_end) == 0 || is.na(row_total_end)) row_total_end <- 0
        
        # OPTIMIZATION: Use pre-computed ROW exports (instead of aggregating in loop)
        origin_to_row_start <- row_exports_start[.(origin_country, comm), row_exports]
        origin_to_row_end <- row_exports_end[.(origin_country, comm), row_exports]
        tc_to_row_start <- row_exports_start[.(tc, comm), row_exports]
        tc_to_row_end <- row_exports_end[.(tc, comm), row_exports]
        
        if (length(origin_to_row_start) == 0 || is.na(origin_to_row_start)) origin_to_row_start <- 0
        if (length(origin_to_row_end) == 0 || is.na(origin_to_row_end)) origin_to_row_end <- 0
        if (length(tc_to_row_start) == 0 || is.na(tc_to_row_start)) tc_to_row_start <- 0
        if (length(tc_to_row_end) == 0 || is.na(tc_to_row_end)) tc_to_row_end <- 0
        
        # Calculate shares of rest-of-world imports
        if (row_total_start > 0) {
          origin_row_share_start <- origin_to_row_start / row_total_start
          tc_row_share_start <- tc_to_row_start / row_total_start
        } else {
          origin_row_share_start <- 0
          tc_row_share_start <- 0
        }
        
        if (row_total_end > 0) {
          origin_row_share_end <- origin_to_row_end / row_total_end
          tc_row_share_end <- tc_to_row_end / row_total_end
        } else {
          origin_row_share_end <- 0
          tc_row_share_end <- 0
        }
        
        # Calculate share growth
        origin_row_growth <- origin_row_share_end - origin_row_share_start
        tc_row_growth <- tc_row_share_end - tc_row_share_start
        
        criterion_iii <- origin_row_growth > tc_row_growth
        
        # OPTIMIZATION: Early exit if criterion iii fails
        if (!criterion_iii) next
        
        # ===================================================================
        # FREUND CRITERION iv: VOLUME THRESHOLD
        # TC imports from origin ≥ 75% of TC exports to destination
        # (Ensures TC is importing significant volumes from origin)
        # ===================================================================
        
        if (tc_to_dest_val_end > 0) {
          volume_ratio <- origin_to_tc_val_end / tc_to_dest_val_end
          criterion_iv <- volume_ratio >= 0.75
        } else {
          criterion_iv <- FALSE
        }
        
        # ===================================================================
        # ALL CRITERIA MET - CALCULATE TRANSSHIPMENT VALUE
        # ===================================================================
        
        # =================================================================
        # CALCULATE TRANSSHIPMENT VALUE (EXCESS GROWTH METHOD)
        # =================================================================
          
          # Calculate excess growth for Origin→TC
          # (growth beyond what would be expected from TC's overall import growth)
          if (world_to_tc_val_start > 0) {
            expected_origin_to_tc <- (origin_to_tc_val_start / world_to_tc_val_start) * 
                                     world_to_tc_val_end
            origin_to_tc_excess <- origin_to_tc_val_end - expected_origin_to_tc
          } else {
            # If no baseline, all end-year trade is excess
            origin_to_tc_excess <- origin_to_tc_val_end
          }
          
          # Calculate excess growth for TC→Destination
          # (growth beyond what would be expected from destination's overall import growth)
          if (world_to_dest_val_start > 0) {
            expected_tc_to_dest <- (tc_to_dest_val_start / world_to_dest_val_start) * 
                                   world_to_dest_val_end
            tc_to_dest_excess <- tc_to_dest_val_end - expected_tc_to_dest
          } else {
            # If no baseline, all end-year trade is excess
            tc_to_dest_excess <- tc_to_dest_val_end
          }
          
          # Transshipment value = min of positive excesses
          # (can't transship more than the excess on either leg)
          transshipment_value <- min(
            max(origin_to_tc_excess, 0),
            max(tc_to_dest_excess, 0)
          )
          
          # Only store if transshipment value is positive
          if (transshipment_value > 0) {
            # Store result
            origin_results[[length(origin_results) + 1]] <- data.table(
              origin_country = origin_country,
              commodity = comm,
              third_country = tc,
              origin_to_tc_val_end = origin_to_tc_val_end,
              tc_to_dest_val_end = tc_to_dest_val_end,
              volume_ratio = volume_ratio,
              origin_row_share_growth = origin_row_growth,
              tc_row_share_growth = tc_row_growth,
              origin_to_tc_excess = max(origin_to_tc_excess, 0),
              tc_to_dest_excess = max(tc_to_dest_excess, 0),
              transshipment_value = transshipment_value
            )
          }
      }
    }
    
    # Report if transshipment was detected in this chunk
    results_after_chunk <- length(origin_results)
    new_routes <- results_after_chunk - results_before_chunk
    
    if (new_routes > 0) {
      cat(sprintf(" Transshipment detected (%d routes)\n", new_routes))
    } else {
      cat(" No transshipment detected\n")
    }
    
    # Clean up memory after each chunk
    gc(verbose = FALSE)
  }
  
  # Combine results for this origin country
  if (length(origin_results) > 0) {
    origin_results_dt <- rbindlist(origin_results)
    all_results[[origin_country]] <- origin_results_dt
    
    cat(sprintf("  [%s] COMPLETE: Found %s qualifying transshipment routes\n",
                origin_country, format(nrow(origin_results_dt), big.mark = ",")))
  } else {
    cat(sprintf("  [%s] COMPLETE: No qualifying transshipment routes found\n",
                origin_country))
  }
}

################################################################################
# COMBINE AND SUMMARIZE RESULTS
################################################################################

cat("\n=== GENERATING SUMMARY STATISTICS ===\n\n")

# Combine all origin results into single data.table
if (length(all_results) > 0) {
  results <- rbindlist(all_results)
  
  cat(sprintf("Total transshipment routes identified: %s\n", 
              format(nrow(results), big.mark = ",")))
  
  # SUMMARY 1: By Origin Country
  cat("\nSummary by Origin Country:\n")
  origin_summary <- results[, .(
    total_transshipment_value = sum(transshipment_value),
    n_routes = .N,
    n_commodities = uniqueN(commodity),
    n_third_countries = uniqueN(third_country),
    top_third_country = third_country[which.max(transshipment_value)],
    top_third_country_value = max(transshipment_value)
  ), by = origin_country][order(-total_transshipment_value)]
  
  print(origin_summary)
  
  # SUMMARY 2: By Commodity (across all origins)
  cat("\n\nTop 20 Commodities by Total Transshipment Value:\n")
  commodity_summary <- results[, .(
    total_transshipment_value = sum(transshipment_value),
    n_origins = uniqueN(origin_country),
    n_third_countries = uniqueN(third_country),
    n_routes = .N,
    main_origin = origin_country[which.max(transshipment_value)],
    main_third_country = third_country[which.max(transshipment_value)]
  ), by = commodity][order(-total_transshipment_value)][1:20]
  
  print(commodity_summary)
  
  # SUMMARY 3: By Third Country (across all origins)
  cat("\n\nTop 20 Third Countries by Total Transshipment Value:\n")
  third_country_summary <- results[, .(
    total_transshipment_value = sum(transshipment_value),
    n_origins = uniqueN(origin_country),
    n_commodities = uniqueN(commodity),
    n_routes = .N,
    main_origin = origin_country[which.max(transshipment_value)],
    top_commodity = commodity[which.max(transshipment_value)]
  ), by = third_country][order(-total_transshipment_value)][1:20]
  
  print(third_country_summary)
  
  # SUMMARY 4: By Origin-Third Country Pair
  cat("\n\nTop 20 Origin-Third Country Pairs by Transshipment Value:\n")
  origin_tc_summary <- results[, .(
    total_transshipment_value = sum(transshipment_value),
    n_commodities = uniqueN(commodity),
    top_commodity = commodity[which.max(transshipment_value)],
    top_commodity_value = max(transshipment_value)
  ), by = .(origin_country, third_country)][order(-total_transshipment_value)][1:20]
  
  print(origin_tc_summary)
  
  # Overall statistics
  cat("\n\n=== OVERALL STATISTICS ===\n")
  cat(sprintf("Total estimated transshipment value: $%s\n", 
              format(sum(results$transshipment_value), big.mark = ",", scientific = FALSE)))
  cat(sprintf("Number of origin countries with transshipment: %d\n", 
              uniqueN(results$origin_country)))
  cat(sprintf("Number of third countries involved: %d\n", 
              uniqueN(results$third_country)))
  cat(sprintf("Number of commodities transshipped: %d\n", 
              uniqueN(results$commodity)))
  cat(sprintf("Total number of routes: %d\n", nrow(results)))
  
  ################################################################################
  # SAVE RESULTS
  ################################################################################
  
  cat("\n=== SAVING RESULTS ===\n")
  
  # Create output object with all results and summaries
  output <- list(
    detailed_routes = results,
    origin_summary = origin_summary,
    commodity_summary = commodity_summary,
    third_country_summary = third_country_summary,
    origin_tc_summary = origin_tc_summary,
    parameters = list(
      origin_countries = origin_countries,
      destination_country = destination_country,
      allow_origin_crossover = allow_origin_crossover,
      start_year = start_year,
      end_year = end_year,
      n_third_countries = length(third_countries)
    )
  )
  
  # Save to file
  saveRDS(output, output_file)
  cat(sprintf("Results saved to: %s\n", output_file))
  
} else {
  cat("No transshipment routes identified for any origin country.\n")
}

cat("\n=== ANALYSIS COMPLETE ===\n")
