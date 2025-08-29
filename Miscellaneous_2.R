# Load required libraries
library(dplyr)
library(tidyr)
library(data.table)  # Much faster for large data operations

# Convert to data.table for speed (if not already)
# This alone can provide 5-10x speedup
trade_data <- as.data.table(trade_data)

# ========================================
# STEP 1: DATA PREPARATION (VECTORIZED)
# ========================================

# Get year bounds
years <- range(trade_data$year)
year1 <- years[1]
year5 <- years[2]

# Filter for year 1 and year 5 only
data_filtered <- trade_data[year %in% c(year1, year5)]

# Calculate total imports and shares in one go
data_filtered[, total_value := sum(value, na.rm = TRUE), 
              by = .(importer, commodity, year)]
data_filtered[, share := value / total_value]

# Get list of third countries
third_countries <- unique(data_filtered$importer[!data_filtered$importer %in% c("USA", "China")])

# ========================================
# STEP 2: VECTORIZED CRITERIA CHECKING
# ========================================

print(paste("Starting vectorized analysis at", Sys.time()))
print(paste("Analyzing", length(unique(data_filtered$commodity)), "commodities and", 
            length(third_countries), "third countries"))

# Create all share combinations efficiently
shares_wide <- data_filtered %>%
  select(importer, exporter, commodity, year, share) %>%
  pivot_wider(names_from = c(importer, exporter, year),
              values_from = share,
              values_fill = 0,
              names_sep = "_")

# Pre-calculate ROW aggregates (this is the expensive part, do it once)
print("Calculating ROW aggregates...")

row_aggregates <- data_filtered %>%
  group_by(commodity, year) %>%
  summarise(
    # For each third country, calculate ROW totals excluding USA, China, and that country
    row_data = list(tibble(
      third_country = third_countries,
      china_row_exports = sapply(third_countries, function(tc) {
        sum(value[exporter == "China" & 
                 importer != "USA" & 
                 importer != "China" & 
                 importer != tc], na.rm = TRUE)
      }),
      tc_row_exports = sapply(third_countries, function(tc) {
        sum(value[exporter == tc & 
                 importer != "USA" & 
                 importer != "China" & 
                 importer != tc], na.rm = TRUE)
      }),
      row_total = sapply(third_countries, function(tc) {
        sum(value[importer != "USA" & 
                 importer != "China" & 
                 importer != tc], na.rm = TRUE)
      })
    )),
    .groups = 'drop'
  ) %>%
  unnest(row_data) %>%
  mutate(
    china_row_share = ifelse(row_total == 0, 0, china_row_exports / row_total),
    tc_row_share = ifelse(row_total == 0, 0, tc_row_exports / row_total)
  )

# Separate year 1 and year 5 ROW data
row_y1 <- row_aggregates %>% filter(year == year1)
row_y5 <- row_aggregates %>% filter(year == year5)

# Join and calculate growth rates
row_growth <- row_y1 %>%
  select(commodity, third_country, 
         china_row_share_y1 = china_row_share,
         tc_row_share_y1 = tc_row_share) %>%
  inner_join(
    row_y5 %>% 
      select(commodity, third_country,
             china_row_share_y5 = china_row_share,
             tc_row_share_y5 = tc_row_share),
    by = c("commodity", "third_country")
  ) %>%
  mutate(
    china_row_growth = case_when(
      china_row_share_y1 == 0 & china_row_share_y5 > 0 ~ 999,
      china_row_share_y1 == 0 ~ 0,
      TRUE ~ (china_row_share_y5 - china_row_share_y1) / china_row_share_y1
    ),
    tc_row_growth = case_when(
      tc_row_share_y1 == 0 & tc_row_share_y5 > 0 ~ 999,
      tc_row_share_y1 == 0 ~ 0,
      TRUE ~ (tc_row_share_y5 - tc_row_share_y1) / tc_row_share_y1
    )
  )

print("Checking criteria for all combinations...")

# Get all bilateral trade values for criteria checking
bilateral_values <- data_filtered %>%
  filter(year == year5) %>%
  select(importer, exporter, commodity, value_y5 = value)

# Build the analysis dataset with all combinations
results <- expand.grid(
  commodity = unique(data_filtered$commodity),
  third_country = third_countries,
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  
  # Add share data for criterion 1
  left_join(shares_wide, by = "commodity") %>%
  
  # Add ROW growth data for criterion 2
  left_join(row_growth, by = c("commodity", "third_country")) %>%
  
  # Add volume data for criterion 3
  left_join(
    bilateral_values %>%
      filter(importer != "USA", exporter == "China") %>%
      rename(third_country = importer, china_to_tc = value_y5) %>%
      select(commodity, third_country, china_to_tc),
    by = c("commodity", "third_country")
  ) %>%
  left_join(
    bilateral_values %>%
      filter(importer == "USA") %>%
      rename(third_country = exporter, tc_to_usa = value_y5) %>%
      select(commodity, third_country, tc_to_usa),
    by = c("commodity", "third_country")
  ) %>%
  
  # Replace NAs with 0s for share columns
  mutate(across(starts_with("USA_") | starts_with("China_") | contains("_tc_"), 
                ~replace_na(., 0))) %>%
  
  # Apply all three criteria
  mutate(
    # Extract relevant shares (column names depend on your data structure)
    china_usa_y1 = get(paste0("USA_China_", year1)),
    china_usa_y5 = get(paste0("USA_China_", year5)),
    tc_usa_y1 = sapply(1:n(), function(i) {
      col_name <- paste0("USA_", third_country[i], "_", year1)
      if(col_name %in% names(.)) get(col_name)[i] else 0
    }),
    tc_usa_y5 = sapply(1:n(), function(i) {
      col_name <- paste0("USA_", third_country[i], "_", year5)
      if(col_name %in% names(.)) get(col_name)[i] else 0
    }),
    china_tc_y1 = sapply(1:n(), function(i) {
      col_name <- paste0(third_country[i], "_China_", year1)
      if(col_name %in% names(.)) get(col_name)[i] else 0
    }),
    china_tc_y5 = sapply(1:n(), function(i) {
      col_name <- paste0(third_country[i], "_China_", year5)
      if(col_name %in% names(.)) get(col_name)[i] else 0
    }),
    
    # Check criteria
    criterion1 = (china_usa_y5 < china_usa_y1) & 
                 (china_tc_y5 > china_tc_y1) & 
                 (tc_usa_y5 > tc_usa_y1),
    
    criterion2 = china_row_growth > tc_row_growth,
    
    criterion3 = china_to_tc >= 0.75 * tc_to_usa,
    
    # All criteria must be met
    qualifies = criterion1 & criterion2 & criterion3 & !is.na(criterion3),
    
    # Calculate transshipment value
    transshipment_value = ifelse(qualifies, pmin(china_to_tc, tc_to_usa, na.rm = TRUE), NA)
  ) %>%
  filter(qualifies)

print(paste("Analysis complete at", Sys.time()))

# ========================================
# STEP 3: COMPILE RESULTS
# ========================================

if(nrow(results) > 0) {
  # Commodity summary
  commodity_transshipment <- results %>%
    group_by(commodity) %>%
    summarise(
      total_transshipment_value = sum(transshipment_value, na.rm = TRUE),
      n_third_countries = n(),
      main_third_country = third_country[which.max(transshipment_value)],
      max_single_route_value = max(transshipment_value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(desc(total_transshipment_value))
  
  # Country summary
  country_transshipment <- results %>%
    group_by(third_country) %>%
    summarise(
      total_transshipment_value = sum(transshipment_value, na.rm = TRUE),
      n_commodities = n(),
      top_commodity = commodity[which.max(transshipment_value)],
      .groups = 'drop'
    ) %>%
    arrange(desc(total_transshipment_value))
  
  # ========================================
  # STEP 4: OUTPUT RESULTS
  # ========================================
  
  cat("=== TRANSSHIPMENT ANALYSIS RESULTS ===\n")
  cat(paste("Number of qualifying routes:", nrow(results), "\n"))
  cat(paste("Number of unique commodities:", length(unique(results$commodity)), "\n"))
  cat(paste("\nTotal estimated transshipment value: $", 
            format(sum(results$transshipment_value, na.rm = TRUE), big.mark = ","), "\n"))
  
  cat("\n=== TOP 10 COMMODITIES BY TRANSSHIPMENT VALUE ===\n")
  print(head(commodity_transshipment, 10))
  
  cat("\n=== TOP THIRD COUNTRIES BY TRANSSHIPMENT VALUE ===\n")
  print(country_transshipment)
  
} else {
  cat("No qualifying transshipment routes found.\n")
  commodity_transshipment <- data.frame()
  country_transshipment <- data.frame()
}

# Store final results
final_results <- list(
  qualifying_routes = results,
  commodity_summary = commodity_transshipment,
  country_summary = country_transshipment
)
