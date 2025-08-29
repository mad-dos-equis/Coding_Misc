# CRITERION 2: ROW analysis - SIMPLE VERSION (Likely Freund's approach)
MIN_SHARE_FOR_GROWTH <- 0.005  # 0.5% minimum share for growth calculation

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
  
  # SIMPLE LOGIC: Only compare if both have established baseline
  if(china_share_y1 < MIN_SHARE_FOR_GROWTH | tc_share_y1 < MIN_SHARE_FOR_GROWTH) {
    # Skip products without established baseline trade
    tc_results[i, criterion2 := FALSE]
  } else {
    # Both have sufficient baseline - use normal growth rates
    china_growth <- (china_share_y5 - china_share_y1) / china_share_y1
    tc_growth <- (tc_share_y5 - tc_share_y1) / tc_share_y1
    tc_results[i, criterion2 := china_growth > tc_growth]
  }
}
