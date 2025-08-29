# Load required libraries
library(dplyr)
library(data.table)

# Convert to data.table for speed
trade_data <- as.data.table(trade_data)

# ========================================
# STEP 1: DATA PREPARATION
# ========================================

# Get year bounds
years <- range(trade_data$year)
year1 <- years[1]
year5 <- years[2]

# Filter for year 1 and year 5 only
data_filtered <- trade_data[year %in% c(year1, year5)]

# Calculate total imports and shares
data_filtered[, total_value := sum(value, na.rm = TRUE), 
              by = .(importer, commodity, year)]
data_filtered[, share := value / total_value]

# Get lists
third_countries <- unique(data_filtered$importer[!data_filtered$importer %in% c("USA", "China")])
all_commodities <- unique(data_filtered$commodity)

print(paste("Processing", length(all_commodities), "commodities and", 
            length(third_countries), "third countries"))
print(paste("Total combinations to check:", length(all_commodities) * length(third_countries)))

# ========================================
# STEP 2: CHUNKED PROCESSING
# ========================================

# Process in chunks to avoid memory issues
chunk_size <- 50  # Process 50 commodities at a time
n_chunks <- ceiling(length(all_commodities) / chunk_size)

# Initialize results storage
all_results <- list()

print(paste("Processing in", n_chunks, "chunks of up to", chunk_size, "commodities each"))

for(chunk_num in 1:n_chunks) {
  
  # Get commodities for this chunk
  start_idx <- (chunk_num - 1) * chunk_size + 1
  end_idx <- min(chunk_num * chunk_size, length(all_commodities))
  chunk_commodities <- all_commodities[start_idx:end_idx]
  
  print(paste("Processing chunk", chunk_num, "of", n_chunks, 
              "- Commodities", start_idx, "to", end_idx))
  
  # Filter data for this chunk
  chunk_data <- data_filtered[commodity %in% chunk_commodities]
  
  # Process each commodity in the chunk
  chunk_results <- list()
  
  for(comm in chunk_commodities) {
    
    # Get commodity-specific data
    comm_data <- chunk_data[commodity == comm]
    
    # Pre-calculate shares for this commodity
    shares_y1 <- comm_data[year == year1, .(importer, exporter, share)]
    shares_y5 <- comm_data[year == year5, .(importer, exporter, share)]
    
    # Process each third country for this commodity
    tc_results <- data.table(
      commodity = comm,
      third_country = third_countries
    )
    
    # CRITERION 1: Calculate share changes vectorially
    # China's share of USA imports
    china_usa_y1 <- shares_y1[importer == "USA" & exporter == "China", share]
    china_usa_y1 <- ifelse(length(china_usa_y1) == 0, 0, china_usa_y1)
    
    china_usa_y5 <- shares_y5[importer == "USA" & exporter == "China", share]
    china_usa_y5 <- ifelse(length(china_usa_y5) == 0, 0, china_usa_y5)
    
    tc_results[, `:=`(
      china_usa_y1 = china_usa_y1,
      china_usa_y5 = china_usa_y5
    )]
    
    # Third countries' shares of USA imports
    tc_usa_shares_y1 <- shares_y1[importer == "USA" & exporter %in% third_countries]
    tc_usa_shares_y5 <- shares_y5[importer == "USA" & exporter %in% third_countries]
    
    tc_results <- merge(tc_results, 
                        tc_usa_shares_y1[, .(third_country = exporter, tc_usa_y1 = share)],
                        by = "third_country", all.x = TRUE)
    tc_results <- merge(tc_results,
                        tc_usa_shares_y5[, .(third_country = exporter, tc_usa_y5 = share)],
                        by = "third_country", all.x = TRUE)
    
    # China's shares of third country imports
    china_tc_shares_y1 <- shares_y1[exporter == "China" & importer %in% third_countries]
    china_tc_shares_y5 <- shares_y5[exporter == "China" & importer %in% third_countries]
    
    tc_results <- merge(tc_results,
                        china_tc_shares_y1[, .(third_country = importer, china_tc_y1 = share)],
                        by = "third_country", all.x = TRUE)
    tc_results <- merge(tc_results,
                        china_tc_shares_y5[, .(third_country = importer, china_tc_y5 = share)],
                        by = "third_country", all.x = TRUE)
    
    # Replace NAs with 0
    cols_to_fill <- c("tc_usa_y1", "tc_usa_y5", "china_tc_y1", "china_tc_y5")
    tc_results[, (cols_to_fill) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), 
               .SDcols = cols_to_fill]
    
    # Check criterion 1
    tc_results[, criterion1 := (china_usa_y5 < china_usa_y1) & 
                               (china_tc_y5 > china_tc_y1) & 
                               (tc_usa_y5 > tc_usa_y1)]
    
    # Filter to only those passing criterion 1
    tc_results <- tc_results[criterion1 == TRUE]
    
    if(nrow(tc_results) == 0) next
    
    # CRITERION 2: ROW analysis - IMPROVED VERSION (handles new entrants intelligently)
    # Define minimum share threshold for growth calculation
    MIN_SHARE_FOR_GROWTH <- 0.005  # 0.5% minimum share
    
    for(i in 1:nrow(tc_results)) {
      tc <- tc_results$third_country[i]
      
      # Define ROW (exclude USA, China, and current third country)
      row_countries <- unique(comm_data$importer[!comm_data$importer %in% c("USA", "China", tc)])
      
      # China to ROW
      china_row_y1 <- comm_data[year == year1 & exporter == "China" & importer %in% row_countries, sum(value, na.rm = TRUE)]
      china_row_y5 <- comm_data[year == year5 & exporter == "China" & importer %in% row_countries, sum(value, na.rm = TRUE)]
      
      # Total ROW imports
      total_row_y1 <- comm_data[year == year1 & importer %in% row_countries, sum(value, na.rm = TRUE)]
      total_row_y5 <- comm_data[year == year5 & importer %in% row_countries, sum(value, na.rm = TRUE)]
      
      # Third country to ROW
      tc_row_y1 <- comm_data[year == year1 & exporter == tc & importer %in% row_countries, sum(value, na.rm = TRUE)]
      tc_row_y5 <- comm_data[year == year5 & exporter == tc & importer %in% row_countries, sum(value, na.rm = TRUE)]
      
      # Calculate shares
      china_share_y1 <- ifelse(total_row_y1 == 0, 0, china_row_y1 / total_row_y1)
      china_share_y5 <- ifelse(total_row_y5 == 0, 0, china_row_y5 / total_row_y5)
      tc_share_y1 <- ifelse(total_row_y1 == 0, 0, tc_row_y1 / total_row_y1)
      tc_share_y5 <- ifelse(total_row_y5 == 0, 0, tc_row_y5 / total_row_y5)
      
      # IMPROVED LOGIC: Smart handling of growth rates based on baseline shares
      if(china_share_y1 < MIN_SHARE_FOR_GROWTH | tc_share_y1 < MIN_SHARE_FOR_GROWTH) {
        # Can't calculate meaningful growth rates - use alternative logic
        
        if(china_share_y1 < MIN_SHARE_FOR_GROWTH & tc_share_y1 < MIN_SHARE_FOR_GROWTH) {
          # Both are new/tiny in base year
          # Check if China's entry is larger than TC's entry (suspicious if TC suddenly bigger)
          tc_results[i, criterion2 := china_share_y5 > tc_share_y5]
          
        } else if(china_share_y1 < MIN_SHARE_FOR_GROWTH) {
          # China is new entrant, TC is established
          # Less likely to be transshipment (China entering new market)
          tc_results[i, criterion2 := FALSE]
          
        } else {
          # TC is new entrant, China is established
          # This could be transshipment (TC suddenly entering where China was)
          # Compare absolute changes instead of growth rates
          china_change <- china_share_y5 - china_share_y1
          tc_change <- tc_share_y5 - tc_share_y1
          tc_results[i, criterion2 := china_change > tc_change]
        }
        
      } else {
        # Both have sufficient baseline - use normal growth rates
        china_growth <- (china_share_y5 - china_share_y1) / china_share_y1
        tc_growth <- (tc_share_y5 - tc_share_y1) / tc_share_y1
        tc_results[i, criterion2 := china_growth > tc_growth]
      }
    }
    
    # Filter to only those passing criterion 2
    tc_results <- tc_results[criterion2 == TRUE]
    
    if(nrow(tc_results) == 0) next
    
    # CRITERION 3: Volume check
    # Get trade values for year 5
    values_y5 <- comm_data[year == year5, .(importer, exporter, value)]
    
    # China to third countries
    china_to_tc <- values_y5[exporter == "China" & importer %in% tc_results$third_country,
                             .(third_country = importer, china_to_tc = value)]
    
    # Third countries to USA
    tc_to_usa <- values_y5[importer == "USA" & exporter %in% tc_results$third_country,
                          .(third_country = exporter, tc_to_usa = value)]
    
    # Merge volume data
    tc_results <- merge(tc_results, china_to_tc, by = "third_country", all.x = TRUE)
    tc_results <- merge(tc_results, tc_to_usa, by = "third_country", all.x = TRUE)
    
    # Replace NAs with 0
    tc_results[is.na(china_to_tc), china_to_tc := 0]
    tc_results[is.na(tc_to_usa), tc_to_usa := 0]
    
    # Check criterion 3
    tc_results[, criterion3 := china_to_tc >= 0.75 * tc_to_usa]
    
    # Filter to qualifying routes
    tc_results <- tc_results[criterion3 == TRUE]
    
    if(nrow(tc_results) > 0) {
      # Calculate transshipment value (using full tc_to_usa as discussed)
      tc_results[, transshipment_value := tc_to_usa]  # Changed from pmin
      
      # Get description if available
      commodity_desc <- unique(comm_data[commodity == comm, description])
      if(length(commodity_desc) > 0) {
        tc_results[, description := commodity_desc[1]]
      } else {
        tc_results[, description := NA_character_]
      }
      
      # Add to chunk results
      chunk_results[[length(chunk_results) + 1]] <- tc_results[, .(
        commodity, description, third_country, china_to_tc, tc_to_usa, transshipment_value
      )]
    }
  }
  
  # Combine chunk results
  if(length(chunk_results) > 0) {
    all_results[[chunk_num]] <- rbindlist(chunk_results)
    print(paste("  Found", nrow(all_results[[chunk_num]]), "qualifying routes in this chunk"))
  } else {
    print("  No qualifying routes in this chunk")
  }
  
  # Clean up memory after each chunk
  rm(chunk_data, chunk_results)
  gc()
}

# ========================================
# STEP 3: COMPILE RESULTS
# ========================================

print("Compiling final results...")

# Combine all results
if(length(all_results) > 0) {
  results <- rbindlist(all_results)
  
  # Free memory
  rm(all_results)
  gc()
  
  # Commodity summary
  commodity_transshipment <- results[, .(
    description = first(description),
    total_transshipment_value = sum(transshipment_value),
    n_third_countries = .N,
    main_third_country = third_country[which.max(transshipment_value)],
    max_single_route_value = max(transshipment_value)
  ), by = commodity][order(-total_transshipment_value)]
  
  # Country summary
  country_transshipment <- results[, .(
    total_transshipment_value = sum(transshipment_value),
    n_transshipped_commodities = uniqueN(commodity),
    top_commodity = commodity[which.max(transshipment_value)],
    top_commodity_desc = description[which.max(transshipment_value)]
  ), by = third_country][order(-total_transshipment_value)]
  
  # ========================================
  # STEP 4: OUTPUT RESULTS
  # ========================================
  
  cat("\n=== TRANSSHIPMENT ANALYSIS RESULTS ===\n")
  cat(paste("Number of qualifying routes:", nrow(results), "\n"))
  cat(paste("Number of unique commodities:", uniqueN(results$commodity), "\n"))
  cat(paste("\nTotal estimated transshipment value: $", 
            format(sum(results$transshipment_value), big.mark = ","), "\n"))
  
  cat("\n=== TOP 10 COMMODITIES BY TRANSSHIPMENT VALUE ===\n")
  print(head(commodity_transshipment, 10))
  
  cat("\n=== TOP 10 THIRD COUNTRIES BY TRANSSHIPMENT VALUE ===\n")
  print(head(country_transshipment, 10))
  
  # Save results
  final_results <- list(
    qualifying_routes = results,
    commodity_summary = commodity_transshipment,
    country_summary = country_transshipment
  )
  
  # Optional: Save to file to avoid losing results
  saveRDS(final_results, "transshipment_results.rds")
  print("\nResults saved to transshipment_results.rds")
  
} else {
  cat("No qualifying transshipment routes found.\n")
  final_results <- list(
    qualifying_routes = data.table(),
    commodity_summary = data.table(),
    country_summary = data.table()
  )
}
