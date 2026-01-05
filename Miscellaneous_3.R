# ==============================================================================
# TRANSSHIPMENT DETECTION ANALYSIS - PARAMETERIZED VERSION (OPTIMIZED V3)
# ==============================================================================
# Based on Freund et al. methodology with configurable parameters
# Detects trade transshipment through third countries using four criteria
# Logic identical to original script, optimized for efficiency
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

# Diagnostic options
RUN_DIAGNOSTICS <- TRUE         # Set to TRUE to export zero-bottleneck routes for inspection

# ------------------------------------------------------------------------------
# PACKAGE LOADING
# ------------------------------------------------------------------------------

library(data.table)

cat("==============================================================================\n")
cat("TRANSSHIPMENT DETECTION ANALYSIS (OPTIMIZED V3)\n")
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
trade_data[, value_imp_pref := fifelse(value_importer == 0 | is.na(value_importer), 
                                        value_exporter, 
                                        value_importer)]
trade_data[, value_exp_pref := fifelse(value_exporter == 0 | is.na(value_exporter), 
                                        value_importer, 
                                        value_exporter)]

# Filter and prepare
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
  third_countries <- unique(data_filtered$imp_iso[!(data_filtered$imp_iso %in% c(DESTINATION_COUNTRY))])
  tc_description <- "all countries except destination"
} else {
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
all_results <- vector("list", length(ORIGIN_COUNTRIES) * n_chunks)
result_idx <- 0L

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
    start_idx <- (chunk_num - 1L) * CHUNK_SIZE + 1L
    end_idx <- min(chunk_num * CHUNK_SIZE, length(all_commodities))
    chunk_commodities <- all_commodities[start_idx:end_idx]
    
    cat(sprintf("Processing chunk %d/%d - Commodities %d to %d... ", 
                chunk_num, n_chunks, start_idx, end_idx))
    
    # Filter data for this chunk
    chunk_data <- data_filtered[commodity %in% chunk_commodities]
    
    # Initialize results storage for this chunk
    chunk_results <- vector("list", length(chunk_commodities))
    chunk_result_idx <- 0L
    chunk_routes_count <- 0L
    
    # ------------------------------------------------------------------------------
    # COMMODITY LOOP
    # ------------------------------------------------------------------------------
    
    for (comm in chunk_commodities) {
      
      # Get commodity-specific data
      comm_data <- chunk_data[commodity == comm]
      
      # Skip if no data
      if (nrow(comm_data) == 0) next
      
      # Split by year for efficiency
      comm_data_start <- comm_data[year == START_YEAR]
      comm_data_end <- comm_data[year == END_YEAR]
      
      # Pre-calculate shares for this commodity
      shares_yr_start <- comm_data_start[, .(imp_iso, exp_iso, share)]
      shares_yr_end <- comm_data_end[, .(imp_iso, exp_iso, share)]
      
      # Initialize tc_results
      tc_results <- data.table(
        commodity = comm,
        third_country = third_countries
      )
      
      # ------------------------------------------------------------------------------
      # CRITERION 2: Calculate share changes vectorially
      # ------------------------------------------------------------------------------
      
      # Origin's share of destination imports (scalar)
      origin_dest_yr_start <- shares_yr_start[imp_iso == DESTINATION_COUNTRY & exp_iso == origin_country, share]
      origin_dest_yr_start <- if (length(origin_dest_yr_start) == 0) 0 else origin_dest_yr_start[1]
      if (is.na(origin_dest_yr_start)) origin_dest_yr_start <- 0
      
      origin_dest_yr_end <- shares_yr_end[imp_iso == DESTINATION_COUNTRY & exp_iso == origin_country, share]
      origin_dest_yr_end <- if (length(origin_dest_yr_end) == 0) 0 else origin_dest_yr_end[1]
      if (is.na(origin_dest_yr_end)) origin_dest_yr_end <- 0
      
      # Quick check: if origin's share didn't decline, no routes can pass criterion 2
      if (origin_dest_yr_end >= origin_dest_yr_start) next
      
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
      
      # Replace NAs with 0 using setnafill for numeric columns
      cols_to_fill <- c("tc_dest_yr_start", "tc_dest_yr_end", "origin_tc_yr_start", "origin_tc_yr_end")
      for (col in cols_to_fill) {
        set(tc_results, which(is.na(tc_results[[col]])), col, 0)
      }
      
      # Check criterion 2: origin share declined (already checked above), 
      # origin's share of TC increased, TC's share of destination increased
      tc_results[, criterion2 := (origin_tc_yr_end > origin_tc_yr_start) &
                                  (tc_dest_yr_end > tc_dest_yr_start)]
      
      # Filter to only those passing criterion 2
      tc_results <- tc_results[criterion2 == TRUE]
      
      if (nrow(tc_results) == 0) next
      
      # ------------------------------------------------------------------------------
      # CRITERION 3: ROW analysis (optimized but logic-preserving)
      # ------------------------------------------------------------------------------
      
      # Define ROW once per commodity (excludes only destination)
      row_countries <- unique(comm_data$imp_iso[!comm_data$imp_iso %in% c(DESTINATION_COUNTRY)])
      
      # Pre-compute origin's ROW values (constant across all TCs)
      origin_row_yr_start <- comm_data_start[exp_iso == origin_country & 
                                              imp_iso != origin_country & 
                                              imp_iso %in% row_countries, 
                                             sum(value_imp_pref, na.rm = TRUE)]
      origin_row_yr_end <- comm_data_end[exp_iso == origin_country & 
                                          imp_iso != origin_country & 
                                          imp_iso %in% row_countries, 
                                         sum(value_imp_pref, na.rm = TRUE)]
      
      # Pre-compute total ROW imports (constant across all TCs)
      total_row_yr_start <- comm_data_start[imp_iso %in% row_countries, 
                                            sum(value_imp_pref, na.rm = TRUE)]
      total_row_yr_end <- comm_data_end[imp_iso %in% row_countries, 
                                        sum(value_imp_pref, na.rm = TRUE)]
      
      # Origin's share of ROW (constant across all TCs)
      origin_share_yr_start <- if (total_row_yr_start == 0) 0 else origin_row_yr_start / total_row_yr_start
      origin_share_yr_end <- if (total_row_yr_end == 0) 0 else origin_row_yr_end / total_row_yr_end
      
      # Pre-compute TC-specific ROW exports for all TCs at once (excluding self-exports)
      tc_row_start_all <- comm_data_start[exp_iso %in% tc_results$third_country & 
                                           imp_iso %in% row_countries & 
                                           imp_iso != exp_iso,
                                          .(tc_row_val_yr_start = sum(value_imp_pref, na.rm = TRUE)),
                                          by = .(third_country = exp_iso)]
      
      tc_row_end_all <- comm_data_end[exp_iso %in% tc_results$third_country & 
                                       imp_iso %in% row_countries & 
                                       imp_iso != exp_iso,
                                      .(tc_row_val_yr_end = sum(value_imp_pref, na.rm = TRUE)),
                                      by = .(third_country = exp_iso)]
      
      # Merge TC ROW values
      tc_results <- merge(tc_results, tc_row_start_all, by = "third_country", all.x = TRUE)
      tc_results <- merge(tc_results, tc_row_end_all, by = "third_country", all.x = TRUE)
      
      # Fill NAs with 0
      set(tc_results, which(is.na(tc_results$tc_row_val_yr_start)), "tc_row_val_yr_start", 0)
      set(tc_results, which(is.na(tc_results$tc_row_val_yr_end)), "tc_row_val_yr_end", 0)
      
      # Calculate TC shares of ROW
      tc_results[, tc_share_yr_start := if (total_row_yr_start == 0) 0 else tc_row_val_yr_start / total_row_yr_start]
      tc_results[, tc_share_yr_end := if (total_row_yr_end == 0) 0 else tc_row_val_yr_end / total_row_yr_end]
      
      # Apply Criterion 3 logic vectorized but preserving exact original branching
      # Original: if (origin_share < MIN | tc_share < MIN) { ... } else { normal growth }
      # Note: origin_share_yr_start/end are scalars, tc_share_yr_start/end are vectors
      
      tc_results[, criterion3 := {
        # Origin values are scalar (same for all TCs in this commodity)
        origin_below_min <- origin_share_yr_start < MIN_SHARE_FOR_GROWTH
        origin_growth <- origin_share_yr_end - origin_share_yr_start
        
        # TC values are vectors (one per TC)
        tc_below_min <- tc_share_yr_start < MIN_SHARE_FOR_GROWTH
        tc_growth <- tc_share_yr_end - tc_share_yr_start
        
        # Apply logic matching original's if/else structure:
        # if (either below min) {
        #   if (both below min) -> compare end shares
        #   else if (origin below min) -> FALSE
        #   else (tc below min) -> compare absolute changes
        # } else {
        #   normal growth comparison
        # }
        
        if (origin_below_min) {
          # Origin is below min for all rows
          # If TC also below min -> compare end shares; else -> FALSE
          fifelse(tc_below_min,
                  origin_share_yr_end > tc_share_yr_end,
                  FALSE)
        } else {
          # Origin has sufficient baseline
          # If TC below min -> compare absolute changes; else -> normal growth
          fifelse(tc_below_min,
                  origin_growth > tc_growth,
                  origin_growth > tc_growth)
        }
      }]
      
      # Filter to only those passing criterion 3
      tc_results <- tc_results[criterion3 == TRUE]
      
      if (nrow(tc_results) == 0) next
      
      # ------------------------------------------------------------------------------
      # CRITERION 4: Volume check (optimized)
      # ------------------------------------------------------------------------------
      
      # Get trade values for both years
      values_yr_start <- comm_data_start[, .(imp_iso, exp_iso, share, value_imp_pref)]
      values_yr_end <- comm_data_end[, .(imp_iso, exp_iso, share, value_imp_pref)]
      
      # Get list of TCs still in consideration
      tc_list <- tc_results$third_country
      
      # Origin to third countries
      origin_to_tc_yr_start <- values_yr_start[exp_iso == origin_country & imp_iso %in% tc_list,
                                                .(third_country = imp_iso, 
                                                  origin_to_tc_share_yr_start = share, 
                                                  origin_to_tc_val_yr_start = value_imp_pref)]
      
      origin_to_tc_yr_end <- values_yr_end[exp_iso == origin_country & imp_iso %in% tc_list,
                                            .(third_country = imp_iso, 
                                              origin_to_tc_share_yr_end = share, 
                                              origin_to_tc_val_yr_end = value_imp_pref)]
      
      origin_to_tc <- merge(origin_to_tc_yr_start, origin_to_tc_yr_end, 
                            by = "third_country", all = TRUE)
      
      # World to third countries
      world_to_tc_yr_start <- values_yr_start[imp_iso %in% tc_list,
                                               .(world_to_tc_val_yr_start = sum(value_imp_pref, na.rm = TRUE)),
                                               by = .(third_country = imp_iso)]
      
      world_to_tc_yr_end <- values_yr_end[imp_iso %in% tc_list,
                                           .(world_to_tc_val_yr_end = sum(value_imp_pref, na.rm = TRUE)),
                                           by = .(third_country = imp_iso)]
      
      world_to_tc <- merge(world_to_tc_yr_start, world_to_tc_yr_end, 
                           by = "third_country", all = TRUE)
      
      # Third countries to destination
      tc_to_dest_yr_start <- values_yr_start[imp_iso == DESTINATION_COUNTRY & exp_iso %in% tc_list,
                                              .(third_country = exp_iso, 
                                                tc_to_dest_share_yr_start = share, 
                                                tc_to_dest_val_yr_start = value_imp_pref)]
      
      tc_to_dest_yr_end <- values_yr_end[imp_iso == DESTINATION_COUNTRY & exp_iso %in% tc_list,
                                          .(third_country = exp_iso, 
                                            tc_to_dest_share_yr_end = share, 
                                            tc_to_dest_val_yr_end = value_imp_pref)]
      
      tc_to_dest <- merge(tc_to_dest_yr_start, tc_to_dest_yr_end, 
                          by = "third_country", all = TRUE)
      
      # World to destination (scalar totals)
      world_to_dest_val_yr_start <- values_yr_start[imp_iso == DESTINATION_COUNTRY,
                                                     sum(value_imp_pref, na.rm = TRUE)]
      world_to_dest_val_yr_end <- values_yr_end[imp_iso == DESTINATION_COUNTRY,
                                                 sum(value_imp_pref, na.rm = TRUE)]
      
      # Join all trade flow data
      tc_results <- merge(tc_results, origin_to_tc, by = "third_country", all.x = TRUE)
      tc_results <- merge(tc_results, tc_to_dest, by = "third_country", all.x = TRUE)
      tc_results <- merge(tc_results, world_to_tc, by = "third_country", all.x = TRUE)
      
      # Add world_to_dest scalars
      tc_results[, `:=`(
        world_to_dest_val_yr_start = world_to_dest_val_yr_start,
        world_to_dest_val_yr_end = world_to_dest_val_yr_end
      )]
      
      # Calculate excess flows (matching original formula)
      tc_results[, origin_to_tc := origin_to_tc_val_yr_end - 
                   ((origin_to_tc_val_yr_start / world_to_tc_val_yr_start) * world_to_tc_val_yr_end)]
      tc_results[, tc_to_dest := tc_to_dest_val_yr_end - 
                   ((tc_to_dest_val_yr_start / world_to_dest_val_yr_start) * world_to_dest_val_yr_end)]
      
      # Replace NAs with 0
      set(tc_results, which(is.na(tc_results$origin_to_tc)), "origin_to_tc", 0)
      set(tc_results, which(is.na(tc_results$tc_to_dest)), "tc_to_dest", 0)
      
      # Check criterion 4
      tc_results[, criterion4 := origin_to_tc_val_yr_end >= VOLUME_RATIO_THRESHOLD * tc_to_dest_val_yr_end]
      
      # Filter to qualifying routes
      tc_results <- tc_results[criterion4 == TRUE]
      
      if (nrow(tc_results) > 0) {
        # Calculate transshipment value
        tc_results[, transshipment_value := pmin(pmax(origin_to_tc, 0), pmax(tc_to_dest, 0))]
        
        # Add diagnostic columns
        tc_results[, `:=`(
          origin_to_tc_clamped = pmax(origin_to_tc, 0),
          tc_to_dest_clamped = pmax(tc_to_dest, 0)
        )]
        tc_results[, `:=`(
          bottleneck = fifelse(origin_to_tc_clamped <= tc_to_dest_clamped, 
                               "origin_to_tc", "tc_to_dest"),
          origin_to_tc_is_zero = origin_to_tc_clamped == 0,
          tc_to_dest_is_zero = tc_to_dest_clamped == 0,
          value_if_pmax = pmax(origin_to_tc_clamped, tc_to_dest_clamped),
          value_lost = abs(origin_to_tc_clamped - tc_to_dest_clamped)
        )]
        
        # Get description if available
        if ("description" %in% names(comm_data)) {
          commodity_desc <- comm_data[1, description]
          tc_results[, description := commodity_desc]
        } else {
          tc_results[, description := NA_character_]
        }
        
        # Track number of routes
        chunk_routes_count <- chunk_routes_count + nrow(tc_results)
        
        # Add to chunk results
        chunk_result_idx <- chunk_result_idx + 1L
        chunk_results[[chunk_result_idx]] <- tc_results[, .(
          origin_country = origin_country,
          commodity, description, third_country,
          origin_to_tc_val_yr_end, origin_to_tc_val_yr_start, 
          world_to_tc_val_yr_start, world_to_tc_val_yr_end,
          origin_to_tc,
          tc_to_dest_val_yr_end, tc_to_dest_val_yr_start, 
          world_to_dest_val_yr_start, world_to_dest_val_yr_end,
          tc_to_dest,
          transshipment_value,
          # Diagnostic columns
          origin_to_tc_clamped, tc_to_dest_clamped,
          bottleneck, origin_to_tc_is_zero, tc_to_dest_is_zero,
          value_if_pmax, value_lost
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
    if (chunk_result_idx > 0) {
      result_idx <- result_idx + 1L
      all_results[[result_idx]] <- rbindlist(chunk_results[1:chunk_result_idx])
    }
    
    # Clean up
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

if (result_idx > 0) {
  final_results <- rbindlist(all_results[1:result_idx])
  
  # Calculate summary statistics by origin
  summary_by_origin <- final_results[, .(
    total_transshipment = sum(transshipment_value, na.rm = TRUE),
    n_routes = .N,
    n_third_countries = uniqueN(third_country),
    n_commodities = uniqueN(commodity)
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
    n_origins = uniqueN(origin_country),
    n_commodities = uniqueN(commodity)
  ), by = third_country][order(-total_transshipment)]
  
  cat("=== TOP 20 THIRD COUNTRIES BY TRANSSHIPMENT VALUE ===\n")
  top_n <- min(20L, nrow(summary_by_tc))
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
    n_third_countries = uniqueN(third_country),
    n_origins = uniqueN(origin_country),
    description = first(description)
  ), by = commodity][order(-total_transshipment)]
  
  cat("=== TOP 20 COMMODITIES BY TRANSSHIPMENT VALUE ===\n")
  top_n <- min(20L, nrow(summary_by_commodity))
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
  ), by = .(origin_country, commodity, third_country)][order(-total_transshipment)]
  
  cat("=== TOP 20 INDIVIDUAL ROUTES BY TRANSSHIPMENT VALUE ===\n")
  top_n <- min(20L, nrow(summary_by_route))
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
  
  # Overall statistics
  total_transshipment <- sum(final_results$transshipment_value, na.rm = TRUE)
  n_routes <- nrow(final_results)
  n_origins <- uniqueN(final_results$origin_country)
  n_third_countries <- uniqueN(final_results$third_country)
  
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
  
  # ------------------------------------------------------------------------------
  # DIAGNOSTIC: Zero-bottleneck analysis
  # ------------------------------------------------------------------------------
  
  if (RUN_DIAGNOSTICS) {
    cat("\n==============================================================================\n")
    cat("DIAGNOSTIC: EXCESS FLOW BOTTLENECK ANALYSIS\n")
    cat("==============================================================================\n\n")
    
    # Aggregate statistics
    n_both_positive <- final_results[origin_to_tc_clamped > 0 & tc_to_dest_clamped > 0, .N]
    n_origin_zero_only <- final_results[origin_to_tc_is_zero & !tc_to_dest_is_zero, .N]
    n_dest_zero_only <- final_results[!origin_to_tc_is_zero & tc_to_dest_is_zero, .N]
    n_both_zero <- final_results[origin_to_tc_is_zero & tc_to_dest_is_zero, .N]
    
    total_value_pmin <- sum(final_results$transshipment_value, na.rm = TRUE)
    total_value_pmax <- sum(final_results$value_if_pmax, na.rm = TRUE)
    total_value_lost <- sum(final_results$value_lost, na.rm = TRUE)
    
    cat("=== ROUTE COUNTS BY EXCESS FLOW PATTERN ===\n")
    cat(sprintf("  Both excess flows > 0:           %s routes (%s%%)\n", 
                format(n_both_positive, big.mark=","),
                round(100 * n_both_positive / n_routes, 1)))
    cat(sprintf("  Only origin→TC excess = 0:       %s routes (%s%%)\n", 
                format(n_origin_zero_only, big.mark=","),
                round(100 * n_origin_zero_only / n_routes, 1)))
    cat(sprintf("  Only TC→dest excess = 0:         %s routes (%s%%)\n", 
                format(n_dest_zero_only, big.mark=","),
                round(100 * n_dest_zero_only / n_routes, 1)))
    cat(sprintf("  Both excess flows = 0:           %s routes (%s%%)\n", 
                format(n_both_zero, big.mark=","),
                round(100 * n_both_zero / n_routes, 1)))
    cat("\n")
    
    cat("=== VALUE COMPARISON: pmin vs pmax ===\n")
    cat(sprintf("  Total transshipment (pmin):      $%s\n", 
                format(round(total_value_pmin), big.mark=",")))
    cat(sprintf("  Total transshipment (pmax):      $%s\n", 
                format(round(total_value_pmax), big.mark=",")))
    cat(sprintf("  Difference ('left on table'):    $%s (%s%% of pmin)\n",
                format(round(total_value_lost), big.mark=","),
                round(100 * total_value_lost / total_value_pmin, 1)))
    cat("\n")
    
    # Bottleneck distribution (excluding both-zero routes)
    bottleneck_summary <- final_results[!(origin_to_tc_is_zero & tc_to_dest_is_zero), 
                                        .(n_routes = .N,
                                          total_value = sum(transshipment_value, na.rm = TRUE),
                                          total_lost = sum(value_lost, na.rm = TRUE)),
                                        by = bottleneck]
    
    cat("=== BOTTLENECK DISTRIBUTION (excluding both-zero routes) ===\n")
    for (i in 1:nrow(bottleneck_summary)) {
      cat(sprintf("  %s is bottleneck: %s routes, $%s value, $%s 'lost'\n",
                  bottleneck_summary$bottleneck[i],
                  format(bottleneck_summary$n_routes[i], big.mark=","),
                  format(round(bottleneck_summary$total_value[i]), big.mark=","),
                  format(round(bottleneck_summary$total_lost[i]), big.mark=",")))
    }
    cat("\n")
    
    # Export zero-bottleneck routes for inspection
    zero_bottleneck_routes <- final_results[
      (origin_to_tc_is_zero & !tc_to_dest_is_zero) |
      (!origin_to_tc_is_zero & tc_to_dest_is_zero),
      .(origin_country, commodity, description, third_country,
        # Raw values
        origin_to_tc_val_yr_start, origin_to_tc_val_yr_end,
        tc_to_dest_val_yr_start, tc_to_dest_val_yr_end,
        # World totals for context
        world_to_tc_val_yr_start, world_to_tc_val_yr_end,
        world_to_dest_val_yr_start, world_to_dest_val_yr_end,
        # Excess flows
        origin_to_tc, tc_to_dest,
        # Diagnostic info
        bottleneck, 
        transshipment_value,
        value_if_pmax,
        value_lost)
    ][order(-value_lost)]
    
    output_file_diagnostic <- file.path(OUTPUT_DIR, 
                                        sprintf("diagnostic_zero_bottleneck_%s_to_%s_%d_%d.csv",
                                                paste(ORIGIN_COUNTRIES, collapse="_"),
                                                DESTINATION_COUNTRY,
                                                START_YEAR,
                                                END_YEAR))
    fwrite(zero_bottleneck_routes, output_file_diagnostic)
    cat(sprintf("Zero-bottleneck routes exported to: %s\n", output_file_diagnostic))
    cat(sprintf("  (%s routes for inspection)\n", format(nrow(zero_bottleneck_routes), big.mark=",")))
    
    # Top 10 routes by value lost
    cat("\n=== TOP 20 ROUTES BY VALUE 'LOST' (pmax - pmin) ===\n")
    top_lost <- zero_bottleneck_routes[1:min(20, nrow(zero_bottleneck_routes))]
    for (i in 1:nrow(top_lost)) {
      cat(sprintf("  %2d. %s → %s → %s: %s [bottleneck: %s]\n",
                  i,
                  top_lost$origin_country[i],
                  top_lost$third_country[i],
                  DESTINATION_COUNTRY,
                  top_lost$commodity[i],
                  top_lost$bottleneck[i]))
      cat(sprintf("      pmin=$%s, pmax=$%s, lost=$%s\n",
                  format(round(top_lost$transshipment_value[i]), big.mark=","),
                  format(round(top_lost$value_if_pmax[i]), big.mark=","),
                  format(round(top_lost$value_lost[i]), big.mark=",")))
      cat(sprintf("      Raw: origin→TC yr_end=$%s, TC→dest yr_end=$%s\n",
                  format(round(top_lost$origin_to_tc_val_yr_end[i]), big.mark=","),
                  format(round(top_lost$tc_to_dest_val_yr_end[i]), big.mark=",")))
      cat(sprintf("      Excess: origin→TC=$%s, TC→dest=$%s\n",
                  format(round(top_lost$origin_to_tc[i]), big.mark=","),
                  format(round(top_lost$tc_to_dest[i]), big.mark=",")))
    }
  }
  
} else {
  cat("No transshipment routes detected across any origin countries.\n")
}

cat("\n==============================================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("==============================================================================\n")
