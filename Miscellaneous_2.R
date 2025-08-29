# Load required library
library(dplyr)

# Assuming your data frame is called 'trade_data' with columns:
# importer, exporter, commodity, year, value
# Data should already be filtered to only include goods with 301 tariffs

# ========================================
# STEP 1: DATA PREPARATION
# ========================================

# Filter for year 1 and year 5 only for comparison
# This aligns with comparing initial and final periods
data_filtered <- trade_data %>%
  filter(year %in% c(min(year), max(year)))

# Calculate total imports by importer-commodity-year
# This will be used to calculate market shares
total_imports <- data_filtered %>%
  group_by(importer, commodity, year) %>%
  summarise(total_value = sum(value, na.rm = TRUE), .groups = 'drop')

# Calculate shares for each exporter in each market
# Share = exporter's value / total imports for that importer-commodity-year
trade_shares <- data_filtered %>%
  left_join(total_imports, by = c("importer", "commodity", "year")) %>%
  mutate(share = value / total_value)

# Separate year 1 and year 5 data for easier comparison
shares_y1 <- trade_shares %>% filter(year == min(year))
shares_y5 <- trade_shares %>% filter(year == max(year))

# Get list of third countries (all countries except USA and China)
# These are potential transshipment hubs
third_countries <- unique(trade_data$importer[!trade_data$importer %in% c("USA", "China")])

# ========================================
# STEP 2: IDENTIFY QUALIFYING COMMODITIES
# ========================================

# Initialize results vectors
qualifying_commodities <- c()
qualifying_details <- list()  # Store details for transshipment calculation

# Loop through each commodity and third country combination
for (comm in unique(trade_data$commodity)) {
  for (tc in third_countries) {
    
    # ----------------------------------------
    # CRITERION 1: Check share changes in USA and third country markets
    # China should lose USA market share, gain third country market share,
    # and third country should gain USA market share
    # ----------------------------------------
    
    # China's share of USA imports - should decline
    china_usa_y1 <- shares_y1 %>% 
      filter(importer == "USA", exporter == "China", commodity == comm) %>% 
      pull(share) %>% {if(length(.) == 0) 0 else .}
    
    china_usa_y5 <- shares_y5 %>% 
      filter(importer == "USA", exporter == "China", commodity == comm) %>% 
      pull(share) %>% {if(length(.) == 0) 0 else .}
    
    # Third country's share of USA imports - should increase
    tc_usa_y1 <- shares_y1 %>% 
      filter(importer == "USA", exporter == tc, commodity == comm) %>% 
      pull(share) %>% {if(length(.) == 0) 0 else .}
    
    tc_usa_y5 <- shares_y5 %>% 
      filter(importer == "USA", exporter == tc, commodity == comm) %>% 
      pull(share) %>% {if(length(.) == 0) 0 else .}
    
    # China's share of third country imports - should increase
    china_tc_y1 <- shares_y1 %>% 
      filter(importer == tc, exporter == "China", commodity == comm) %>% 
      pull(share) %>% {if(length(.) == 0) 0 else .}
    
    china_tc_y5 <- shares_y5 %>% 
      filter(importer == tc, exporter == "China", commodity == comm) %>% 
      pull(share) %>% {if(length(.) == 0) 0 else .}
    
    # Check all three conditions for criterion 1
    crit1 <- (china_usa_y5 < china_usa_y1) &     # China loses USA market share
             (china_tc_y5 > china_tc_y1) &       # China gains third country market share
             (tc_usa_y5 > tc_usa_y1)             # Third country gains USA market share
    
    # If criterion 1 fails, skip to next combination
    if (!crit1) next
    
    # ----------------------------------------
    # CRITERION 2: Rest of world (ROW) imports - using AGGREGATE measures
    # China's share growth in ROW should exceed third country's share growth
    # This ensures third country isn't becoming globally competitive
    # ----------------------------------------
    
    # Define ROW as all countries except USA, China, and the third country
    row_countries <- unique(trade_data$importer[!trade_data$importer %in% c("USA", "China", tc)])
    
    # Calculate AGGREGATE China exports to ROW for year 1
    china_row_exports_y1 <- data_filtered %>%
      filter(year == min(year), 
             importer %in% row_countries, 
             exporter == "China", 
             commodity == comm) %>%
      summarise(total = sum(value, na.rm = TRUE)) %>%
      pull(total)
    
    # Calculate AGGREGATE China exports to ROW for year 5
    china_row_exports_y5 <- data_filtered %>%
      filter(year == max(year), 
             importer %in% row_countries, 
             exporter == "China", 
             commodity == comm) %>%
      summarise(total = sum(value, na.rm = TRUE)) %>%
      pull(total)
    
    # Calculate total ROW imports for year 1
    row_total_imports_y1 <- data_filtered %>%
      filter(year == min(year), 
             importer %in% row_countries, 
             commodity == comm) %>%
      summarise(total = sum(value, na.rm = TRUE)) %>%
      pull(total)
    
    # Calculate total ROW imports for year 5
    row_total_imports_y5 <- data_filtered %>%
      filter(year == max(year), 
             importer %in% row_countries, 
             commodity == comm) %>%
      summarise(total = sum(value, na.rm = TRUE)) %>%
      pull(total)
    
    # Calculate China's aggregate share of ROW imports
    china_row_share_y1 <- ifelse(row_total_imports_y1 == 0, 0, 
                                  china_row_exports_y1 / row_total_imports_y1)
    china_row_share_y5 <- ifelse(row_total_imports_y5 == 0, 0, 
                                  china_row_exports_y5 / row_total_imports_y5)
    
    # Calculate AGGREGATE third country exports to ROW
    tc_row_exports_y1 <- data_filtered %>%
      filter(year == min(year), 
             importer %in% row_countries, 
             exporter == tc, 
             commodity == comm) %>%
      summarise(total = sum(value, na.rm = TRUE)) %>%
      pull(total)
    
    tc_row_exports_y5 <- data_filtered %>%
      filter(year == max(year), 
             importer %in% row_countries, 
             exporter == tc, 
             commodity == comm) %>%
      summarise(total = sum(value, na.rm = TRUE)) %>%
      pull(total)
    
    # Calculate third country's aggregate share of ROW imports
    tc_row_share_y1 <- ifelse(row_total_imports_y1 == 0, 0, 
                               tc_row_exports_y1 / row_total_imports_y1)
    tc_row_share_y5 <- ifelse(row_total_imports_y5 == 0, 0, 
                               tc_row_exports_y5 / row_total_imports_y5)
    
    # Calculate growth rates for comparison
    # Handle division by zero cases
    china_row_growth <- ifelse(china_row_share_y1 == 0, 
                                ifelse(china_row_share_y5 > 0, Inf, 0),
                                (china_row_share_y5 - china_row_share_y1) / china_row_share_y1)
    
    tc_row_growth <- ifelse(tc_row_share_y1 == 0, 
                             ifelse(tc_row_share_y5 > 0, Inf, 0),
                             (tc_row_share_y5 - tc_row_share_y1) / tc_row_share_y1)
    
    # Check criterion 2: China's ROW growth should exceed third country's
    crit2 <- china_row_growth > tc_row_growth
    
    # If criterion 2 fails, skip to next combination
    if (!crit2) next
    
    # ----------------------------------------
    # CRITERION 3: Volume check
    # Third country imports from China should be ≥ 75% of USA imports from third country
    # This ensures significant volumes are flowing through the third country
    # ----------------------------------------
    
    # Get third country's imports from China in year 5
    tc_from_china_y5 <- data_filtered %>% 
      filter(year == max(year), 
             importer == tc, 
             exporter == "China", 
             commodity == comm) %>% 
      pull(value) %>% 
      {if(length(.) == 0) 0 else sum(., na.rm = TRUE)}
    
    # Get USA's imports from third country in year 5
    usa_from_tc_y5 <- data_filtered %>% 
      filter(year == max(year), 
             importer == "USA", 
             exporter == tc, 
             commodity == comm) %>% 
      pull(value) %>% 
      {if(length(.) == 0) 0 else sum(., na.rm = TRUE)}
    
    # Check criterion 3
    crit3 <- tc_from_china_y5 >= 0.75 * usa_from_tc_y5
    
    # If all three criteria are met, add to qualifying list
    if (crit3) {
      qualifying_commodities <- c(qualifying_commodities, comm)
      
      # Store details for transshipment value calculation
      # Following the paper: transshipment = min(China→TC, TC→USA)
      qualifying_details[[length(qualifying_details) + 1]] <- list(
        commodity = comm,
        third_country = tc,
        china_to_tc = tc_from_china_y5,
        tc_to_usa = usa_from_tc_y5,
        transshipment_value = min(tc_from_china_y5, usa_from_tc_y5)
      )
    }
  }
}

# ========================================
# STEP 3: COMPILE RESULTS
# ========================================

# Get unique list of qualifying commodities
final_commodity_list <- unique(qualifying_commodities)

# Convert qualifying details to data frame for analysis
if (length(qualifying_details) > 0) {
  transshipment_df <- bind_rows(qualifying_details)
  
  # Summarize transshipment by commodity (may have multiple third countries)
  commodity_transshipment <- transshipment_df %>%
    group_by(commodity) %>%
    summarise(
      total_transshipment_value = sum(transshipment_value),
      n_third_countries = n(),
      main_third_country = third_country[which.max(transshipment_value)],
      max_single_route_value = max(transshipment_value),
      .groups = 'drop'
    ) %>%
    arrange(desc(total_transshipment_value))
  
  # Summarize transshipment by third country
  country_transshipment <- transshipment_df %>%
    group_by(third_country) %>%
    summarise(
      total_transshipment_value = sum(transshipment_value),
      n_commodities = n(),
      top_commodity = commodity[which.max(transshipment_value)],
      .groups = 'drop'
    ) %>%
    arrange(desc(total_transshipment_value))
  
} else {
  transshipment_df <- data.frame()
  commodity_transshipment <- data.frame()
  country_transshipment <- data.frame()
}

# ========================================
# STEP 4: OUTPUT RESULTS
# ========================================

# Print summary statistics
cat("=== TRANSSHIPMENT ANALYSIS RESULTS ===\n")
cat(paste("Number of qualifying commodities:", length(final_commodity_list), "\n"))
cat(paste("Total transshipment routes identified:", nrow(transshipment_df), "\n"))

if (nrow(transshipment_df) > 0) {
  cat(paste("\nTotal estimated transshipment value: $", 
            format(sum(transshipment_df$transshipment_value), big.mark = ","), "\n"))
  
  cat("\n=== TOP 10 COMMODITIES BY TRANSSHIPMENT VALUE ===\n")
  print(head(commodity_transshipment, 10))
  
  cat("\n=== TOP THIRD COUNTRIES BY TRANSSHIPMENT VALUE ===\n")
  print(country_transshipment)
}

# Store all results in a list for further analysis
results <- list(
  qualifying_commodities = final_commodity_list,
  transshipment_details = transshipment_df,
  commodity_summary = commodity_transshipment,
  country_summary = country_transshipment
)
