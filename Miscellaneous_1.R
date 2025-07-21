# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)

# Function to detect outlier countries within each HS6 code
detect_outlier_countries <- function(data, method = "consensus", threshold_iqr = 1.5, 
                                   threshold_z = 3, threshold_mad = 3.5) {
  
  # Calculate outlier metrics for each country-HS6 combination
  outlier_analysis <- data %>%
    group_by(hs6) %>%
    mutate(
      # Basic statistics
      n_exporters = n_distinct(exporter),
      median_uv = median(unit_value, na.rm = TRUE),
      mean_uv = mean(unit_value, na.rm = TRUE),
      sd_uv = sd(unit_value, na.rm = TRUE),
      
      # Calculate percentile for each observation
      percentile = round(percent_rank(unit_value) * 100, 1),
      
      # IQR method
      q1 = quantile(unit_value, 0.25, na.rm = TRUE),
      q3 = quantile(unit_value, 0.75, na.rm = TRUE),
      iqr = q3 - q1,
      lower_fence = q1 - threshold_iqr * iqr,
      upper_fence = q3 + threshold_iqr * iqr,
      is_outlier_iqr = unit_value < lower_fence | unit_value > upper_fence,
      
      # Z-score method
      z_score = (unit_value - mean_uv) / sd_uv,
      is_outlier_zscore = abs(z_score) > threshold_z,
      
      # Modified Z-score (MAD method)
      mad_uv = mad(unit_value, na.rm = TRUE),
      modified_z_score = ifelse(mad_uv > 0, 
                                0.6745 * (unit_value - median_uv) / mad_uv, 
                                0),
      is_outlier_mad = abs(modified_z_score) > threshold_mad,
      
      # Deviation metrics
      pct_deviation_from_median = (unit_value - median_uv) / median_uv * 100,
      
      # Consensus outlier (flagged by at least 2 methods)
      outlier_count = is_outlier_iqr + is_outlier_zscore + is_outlier_mad
    )
  
  # Determine final outlier status based on method choice
  outlier_analysis <- outlier_analysis %>%
    mutate(
      is_outlier = case_when(
        method == "iqr" ~ is_outlier_iqr,
        method == "zscore" ~ is_outlier_zscore,
        method == "mad" ~ is_outlier_mad,
        method == "consensus" ~ outlier_count >= 2,
        method == "strict" ~ outlier_count == 3,
        method == "any" ~ outlier_count >= 1,
        TRUE ~ outlier_count >= 2  # default to consensus
      ),
      # Flag if this is a U.S. outlier
      is_us = (exporter == "United States" | exporter == "USA" | 
               exporter == "US" | exporter == "United States of America"),
      is_us_outlier = is_us & is_outlier
    ) %>%
    ungroup()
  
  return(outlier_analysis)
}

# Create focused report on U.S. outliers
create_us_outlier_report <- function(outlier_data) {
  
  # Check if description column exists
  has_description <- "description" %in% names(outlier_data)
  
  # Check for original unit and quantity columns
  has_original_units <- all(c("UNIT1", "QTY1") %in% names(outlier_data))
  has_second_units <- all(c("UNIT2", "QTY2") %in% names(outlier_data))
  
  # First, calculate total value and quantity by HS6
  hs6_totals <- outlier_data %>%
    group_by(hs6) %>%
    summarise(
      total_value_hs6 = sum(value, na.rm = TRUE),
      total_quantity_hs6 = sum(quantity, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Report 1: U.S. outlier products with trade shares
  us_outliers <- outlier_data %>%
    filter(is_us_outlier) %>%
    left_join(hs6_totals, by = "hs6") %>%
    mutate(
      us_value_share = round((value / total_value_hs6) * 100, 2),
      us_quantity_share = round((quantity / total_quantity_hs6) * 100, 2)
    ) %>%
    arrange(desc(abs(pct_deviation_from_median))) %>%
    {if(has_description) {
      if(has_original_units && has_second_units) {
        select(., hs6, description, unit_value, median_uv, 
               pct_deviation_from_median, percentile, z_score, modified_z_score,
               is_outlier_iqr, is_outlier_zscore, is_outlier_mad,
               value, quantity, UNIT1, QTY1, UNIT2, QTY2,
               us_value_share, us_quantity_share,
               n_exporters, outlier_count)
      } else if(has_original_units) {
        select(., hs6, description, unit_value, median_uv, 
               pct_deviation_from_median, percentile, z_score, modified_z_score,
               is_outlier_iqr, is_outlier_zscore, is_outlier_mad,
               value, quantity, UNIT1, QTY1,
               us_value_share, us_quantity_share,
               n_exporters, outlier_count)
      } else {
        select(., hs6, description, unit_value, median_uv, 
               pct_deviation_from_median, percentile, z_score, modified_z_score,
               is_outlier_iqr, is_outlier_zscore, is_outlier_mad,
               value, quantity, us_value_share, us_quantity_share,
               n_exporters, outlier_count)
      }
    } else {
      if(has_original_units && has_second_units) {
        select(., hs6, unit_value, median_uv, 
               pct_deviation_from_median, percentile, z_score, modified_z_score,
               is_outlier_iqr, is_outlier_zscore, is_outlier_mad,
               value, quantity, UNIT1, QTY1, UNIT2, QTY2,
               us_value_share, us_quantity_share,
               n_exporters, outlier_count)
      } else if(has_original_units) {
        select(., hs6, unit_value, median_uv, 
               pct_deviation_from_median, percentile, z_score, modified_z_score,
               is_outlier_iqr, is_outlier_zscore, is_outlier_mad,
               value, quantity, UNIT1, QTY1,
               us_value_share, us_quantity_share,
               n_exporters, outlier_count)
      } else {
        select(., hs6, unit_value, median_uv, 
               pct_deviation_from_median, percentile, z_score, modified_z_score,
               is_outlier_iqr, is_outlier_zscore, is_outlier_mad,
               value, quantity, us_value_share, us_quantity_share,
               n_exporters, outlier_count)
      }
    }} %>%
    mutate(
      outlier_direction = ifelse(pct_deviation_from_median > 0, 
                                "Above Market", "Below Market"),
      pct_deviation_from_median = round(pct_deviation_from_median, 1),
      z_score = round(z_score, 2),
      modified_z_score = round(modified_z_score, 2),
      price_ratio = round(unit_value / median_uv, 2)
    )
  
  # Report 2: Summary of U.S. performance
  us_summary <- outlier_data %>%
    filter(is_us) %>%
    summarise(
      total_hs6_exported = n(),
      n_outlier_products = sum(is_outlier),
      outlier_rate = round(n_outlier_products / total_hs6_exported * 100, 1),
      n_above_market = sum(is_outlier & pct_deviation_from_median > 0),
      n_below_market = sum(is_outlier & pct_deviation_from_median < 0),
      avg_deviation_when_outlier = round(
        mean(abs(pct_deviation_from_median[is_outlier]), na.rm = TRUE), 1
      ),
      total_trade_value = sum(value, na.rm = TRUE)
    )
  
  # Report 3: Comparison with other major exporters
  country_comparison <- outlier_data %>%
    group_by(exporter) %>%
    summarise(
      n_products = n(),
      n_outliers = sum(is_outlier),
      outlier_rate = round(n_outliers / n_products * 100, 1),
      avg_abs_deviation = round(mean(abs(pct_deviation_from_median), na.rm = TRUE), 1),
      total_value = sum(value, na.rm = TRUE)
    ) %>%
    filter(n_products >= 10) %>%  # Only countries with substantial exports
    arrange(desc(outlier_rate)) %>%
    mutate(
      is_us = exporter %in% c("United States", "USA", "US", "United States of America"),
      rank_by_outlier_rate = row_number()
    )
  
  # Report 4: HS6 codes where U.S. is most unusual (also with trade shares)
  us_deviation_by_hs6 <- outlier_data %>%
    filter(is_us) %>%
    left_join(hs6_totals, by = "hs6") %>%
    mutate(
      us_value_share = round((value / total_value_hs6) * 100, 2),
      us_quantity_share = round((quantity / total_quantity_hs6) * 100, 2)
    ) %>%
    {if(has_description) {
      select(., hs6, description, unit_value, median_uv, pct_deviation_from_median, 
             n_exporters, is_outlier, value, us_value_share, us_quantity_share)
    } else {
      select(., hs6, unit_value, median_uv, pct_deviation_from_median, 
             n_exporters, is_outlier, value, us_value_share, us_quantity_share)
    }} %>%
    arrange(desc(abs(pct_deviation_from_median)))
  
  return(list(
    us_outliers = us_outliers,
    us_summary = us_summary,
    country_comparison = country_comparison,
    us_all_products = us_deviation_by_hs6
  ))
}

# Function to visualize U.S. vs others for specific HS6
plot_us_vs_others <- function(data, hs6_code, save_plot = FALSE) {
  
  plot_data <- data %>% 
    filter(hs6 == hs6_code) %>%
    mutate(
      country_type = case_when(
        is_us ~ "United States",
        is_outlier ~ "Other Outlier Countries",
        TRUE ~ "Other Countries"
      ),
      label = ifelse(is_us | is_outlier, exporter, "")
    )
  
  # Get US position and description if available
  us_data <- plot_data %>% filter(is_us)
  description_text <- if("description" %in% names(plot_data)) {
    unique(plot_data$description)[1]
  } else {
    ""
  }
  
  p <- ggplot(plot_data, aes(x = reorder(exporter, unit_value), y = unit_value)) +
    geom_hline(yintercept = unique(plot_data$median_uv), 
               linetype = "dashed", color = "blue", alpha = 0.5) +
    geom_hline(yintercept = unique(plot_data$lower_fence), 
               linetype = "dotted", color = "gray", alpha = 0.5) +
    geom_hline(yintercept = unique(plot_data$upper_fence), 
               linetype = "dotted", color = "gray", alpha = 0.5) +
    geom_point(aes(color = country_type, size = value), alpha = 0.7) +
    geom_text(aes(label = label), vjust = -0.5, hjust = 0.5, 
              angle = 45, size = 3) +
    scale_color_manual(values = c(
      "United States" = "red",
      "Other Outlier Countries" = "orange", 
      "Other Countries" = "gray60"
    )) +
    scale_size_continuous(name = "Trade Value", labels = scales::comma) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "bottom",
      legend.box = "vertical"
    ) +
    labs(
      title = paste("Unit Value Comparison for HS6:", hs6_code),
      subtitle = paste(
        if(description_text != "") paste0("Product: ", description_text, "\n") else "",
        "U.S. Price:", round(us_data$unit_value, 2), 
        "| Median:", round(unique(plot_data$median_uv), 2),
        "| U.S. Deviation:", round(us_data$pct_deviation_from_median, 1), "%"
      ),
      x = "Countries (ordered by unit value)",
      y = "Unit Value",
      color = "Country Type"
    )
  
  if (save_plot) {
    ggsave(paste0("us_vs_others_hs6_", hs6_code, ".png"), p, 
           width = 10, height = 6, dpi = 300)
  }
  
  return(p)
}

# Create a detailed comparison table for specific HS6
create_hs6_comparison <- function(data, hs6_code) {
  has_description <- "description" %in% names(data)
  
  hs6_data <- data %>% 
    filter(hs6 == hs6_code) %>%
    arrange(desc(unit_value)) %>%
    {if(has_description) {
      select(., exporter, unit_value, value, quantity, 
             pct_deviation_from_median, is_outlier, description)
    } else {
      select(., exporter, unit_value, value, quantity, 
             pct_deviation_from_median, is_outlier)
    }} %>%
    mutate(
      rank = row_number(),
      unit_value = round(unit_value, 2),
      pct_deviation = paste0(round(pct_deviation_from_median, 1), "%"),
      is_usa = exporter %in% c("United States", "USA", "US")
    )
  
  # Move description to the front if it exists
  if(has_description) {
    hs6_data <- hs6_data %>%
      select(rank, exporter, description, everything())
  } else {
    hs6_data <- hs6_data %>%
      select(rank, exporter, everything())
  }
  
  return(hs6_data)
}

# Main analysis workflow
# Run the outlier detection on full dataset
outlier_results <- detect_outlier_countries(china_imports)

# Generate U.S.-focused reports
us_reports <- create_us_outlier_report(outlier_results)

# Export reports
write.csv(us_reports$us_outliers, 
          "us_outlier_products.csv", 
          row.names = FALSE)

write.csv(us_reports$us_all_products, 
          "us_all_products_comparison.csv", 
          row.names = FALSE)

write.csv(us_reports$country_comparison, 
          "country_outlier_rates.csv", 
          row.names = FALSE)

# Print summary
cat("=== U.S. EXPORT PRICE ANALYSIS ===\n")
print(us_reports$us_summary)

cat("\n=== TOP 10 U.S. OUTLIER PRODUCTS ===\n")
if("description" %in% names(us_reports$us_outliers)) {
  print(us_reports$us_outliers %>% 
          select(hs6, description, pct_deviation_from_median, percentile,
                 outlier_direction, price_ratio, us_value_share, us_quantity_share,
                 is_outlier_iqr, is_outlier_zscore, is_outlier_mad) %>%
          head(10))
} else {
  print(us_reports$us_outliers %>% 
          select(hs6, pct_deviation_from_median, percentile,
                 outlier_direction, price_ratio, us_value_share, us_quantity_share,
                 is_outlier_iqr, is_outlier_zscore, is_outlier_mad) %>%
          head(10))
}

cat("\n=== COUNTRY COMPARISON (Outlier Rates) ===\n")
us_rank <- us_reports$country_comparison %>% 
  filter(is_us) %>% 
  pull(rank_by_outlier_rate)
cat("U.S. ranks #", us_rank, " out of ", nrow(us_reports$country_comparison), 
    " major exporters by outlier rate\n\n")
print(head(us_reports$country_comparison, 10))

# Visualize top U.S. outlier products
if (nrow(us_reports$us_outliers) > 0) {
  top_us_outlier_hs6 <- head(us_reports$us_outliers$hs6, 5)
  for (hs6_code in top_us_outlier_hs6) {
    p <- plot_us_vs_others(outlier_results, hs6_code, save_plot = TRUE)
    print(p)
  }
}

# Create detailed comparison for biggest U.S. outliers
if (nrow(us_reports$us_outliers) > 0) {
  biggest_outlier_hs6 <- us_reports$us_outliers$hs6[1]
  comparison_table <- create_hs6_comparison(outlier_results, biggest_outlier_hs6)
  
  cat("\n=== DETAILED COMPARISON FOR HS6", biggest_outlier_hs6, "===\n")
  if("description" %in% names(comparison_table)) {
    cat("Product:", unique(comparison_table$description)[1], "\n")
  }
  cat("Product where U.S. has largest deviation from market price\n")
  print(comparison_table)
}
