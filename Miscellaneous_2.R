# Load required library
library(dplyr)

# Assuming your data frame is called 'trade_data' with columns:
# importer, exporter, commodity, year, value

# Filter for year 1 and year 5 only
data_filtered <- trade_data %>%
  filter(year %in% c(min(year), max(year)))

# Calculate total imports by importer-commodity-year
total_imports <- data_filtered %>%
  group_by(importer, commodity, year) %>%
  summarise(total_value = sum(value, na.rm = TRUE), .groups = 'drop')

# Calculate shares for each exporter
trade_shares <- data_filtered %>%
  left_join(total_imports, by = c("importer", "commodity", "year")) %>%
  mutate(share = value / total_value)

# Separate year 1 and year 5 data
shares_y1 <- trade_shares %>% filter(year == min(year))
shares_y5 <- trade_shares %>% filter(year == max(year))

# Get list of third countries (all countries except USA and China)
third_countries <- unique(trade_data$importer[!trade_data$importer %in% c("USA", "China")])

# Initialize results
qualifying_commodities <- c()

# Loop through each commodity and third country combination
for (comm in unique(trade_data$commodity)) {
  for (tc in third_countries) {
    
    # Criterion 1: Check shares for USA imports
    china_usa_y1 <- shares_y1 %>% 
      filter(importer == "USA", exporter == "China", commodity == comm) %>% 
      pull(share) %>% {if(length(.) == 0) 0 else .}
    
    china_usa_y5 <- shares_y5 %>% 
      filter(importer == "USA", exporter == "China", commodity == comm) %>% 
      pull(share) %>% {if(length(.) == 0) 0 else .}
    
    tc_usa_y1 <- shares_y1 %>% 
      filter(importer == "USA", exporter == tc, commodity == comm) %>% 
      pull(share) %>% {if(length(.) == 0) 0 else .}
    
    tc_usa_y5 <- shares_y5 %>% 
      filter(importer == "USA", exporter == tc, commodity == comm) %>% 
      pull(share) %>% {if(length(.) == 0) 0 else .}
    
    china_tc_y1 <- shares_y1 %>% 
      filter(importer == tc, exporter == "China", commodity == comm) %>% 
      pull(share) %>% {if(length(.) == 0) 0 else .}
    
    china_tc_y5 <- shares_y5 %>% 
      filter(importer == tc, exporter == "China", commodity == comm) %>% 
      pull(share) %>% {if(length(.) == 0) 0 else .}
    
    # Check criterion 1
    crit1 <- (china_usa_y5 < china_usa_y1) & 
             (china_tc_y5 > china_tc_y1) & 
             (tc_usa_y5 > tc_usa_y1)
    
    if (!crit1) next
    
    # Criterion 2: Rest of world imports
    row_countries <- unique(trade_data$importer[!trade_data$importer %in% c("USA", "China", tc)])
    
    # Calculate China's share of ROW imports
    china_row_y1 <- shares_y1 %>% 
      filter(importer %in% row_countries, exporter == "China", commodity == comm) %>%
      group_by(commodity) %>%
      summarise(avg_share = mean(share, na.rm = TRUE)) %>%
      pull(avg_share) %>% {if(length(.) == 0) 0 else .}
    
    china_row_y5 <- shares_y5 %>% 
      filter(importer %in% row_countries, exporter == "China", commodity == comm) %>%
      group_by(commodity) %>%
      summarise(avg_share = mean(share, na.rm = TRUE)) %>%
      pull(avg_share) %>% {if(length(.) == 0) 0 else .}
    
    # Calculate third country's share of ROW imports
    tc_row_y1 <- shares_y1 %>% 
      filter(importer %in% row_countries, exporter == tc, commodity == comm) %>%
      group_by(commodity) %>%
      summarise(avg_share = mean(share, na.rm = TRUE)) %>%
      pull(avg_share) %>% {if(length(.) == 0) 0 else .}
    
    tc_row_y5 <- shares_y5 %>% 
      filter(importer %in% row_countries, exporter == tc, commodity == comm) %>%
      group_by(commodity) %>%
      summarise(avg_share = mean(share, na.rm = TRUE)) %>%
      pull(avg_share) %>% {if(length(.) == 0) 0 else .}
    
    # Calculate growth rates
    china_row_growth <- ifelse(china_row_y1 == 0, 
                                ifelse(china_row_y5 > 0, Inf, 0),
                                (china_row_y5 - china_row_y1) / china_row_y1)
    
    tc_row_growth <- ifelse(tc_row_y1 == 0, 
                             ifelse(tc_row_y5 > 0, Inf, 0),
                             (tc_row_y5 - tc_row_y1) / tc_row_y1)
    
    # Check criterion 2
    crit2 <- china_row_growth > tc_row_growth
    
    if (!crit2) next
    
    # Criterion 3: Third-country imports from China vs US imports from third-country
    tc_from_china_y5 <- data_filtered %>% 
      filter(year == max(year), importer == tc, exporter == "China", commodity == comm) %>% 
      pull(value) %>% {if(length(.) == 0) 0 else sum(., na.rm = TRUE)}
    
    usa_from_tc_y5 <- data_filtered %>% 
      filter(year == max(year), importer == "USA", exporter == tc, commodity == comm) %>% 
      pull(value) %>% {if(length(.) == 0) 0 else sum(., na.rm = TRUE)}
    
    # Check criterion 3
    crit3 <- tc_from_china_y5 >= 0.75 * usa_from_tc_y5
    
    if (crit3) {
      qualifying_commodities <- c(qualifying_commodities, comm)
    }
  }
}

# Get unique list of qualifying commodities
final_commodity_list <- unique(qualifying_commodities)

# Print results
print(paste("Number of qualifying commodities:", length(final_commodity_list)))
print(final_commodity_list)
