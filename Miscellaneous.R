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
      )
    ) %>%
    ungroup()
  
  return(outlier_analysis)
}

# Create a summary report of outlier countries
create_outlier_report <- function(outlier_data) {
  
  # Report 1: Outlier countries by HS6
  outlier_countries_by_hs6 <- outlier_data %>%
    filter(.data$is_outlier == TRUE) %>%
    arrange(.data$hs6, desc(abs(.data$pct_deviation_from_median))) %>%
    select(
      hs6, exporter, unit_value, median_uv, 
      pct_deviation_from_median, z_score, modified_z_score,
      value, quantity, n_exporters, outlier_count
    ) %>%
    mutate(
      outlier_type = ifelse(pct_deviation_from_median > 0, "Above", "Below"),
      pct_deviation_from_median = round(pct_deviation_from_median, 1),
      z_score = round(z_score, 2),
      modified_z_score = round(modified_z_score, 2)
    )
  
  # Report 2: Summary by HS6 - which products have most outliers
  hs6_outlier_summary <- outlier_data %>%
    group_by(hs6) %>%
    summarise(
      n_exporters = n_distinct(exporter),
      n_outlier_countries = sum(.data$is_outlier, na.rm = TRUE),
      pct_outlier_countries = round(n_outlier_countries / n_exporters * 100, 1),
      median_unit_value = round(median(unit_value, na.rm = TRUE), 2),
      cv_unit_value = round(sd(unit_value, na.rm = TRUE) / mean(unit_value, na.rm = TRUE), 3),
      min_unit_value = min(unit_value, na.rm = TRUE),
      max_unit_value = max(unit_value, na.rm = TRUE),
      price_range_ratio = round(max_unit_value / min_unit_value, 1)
    ) %>%
    filter(n_outlier_countries > 0) %>%
    arrange(desc(pct_outlier_countries))
  
  # Report 3: Summary by country - which countries are frequent outliers
  country_outlier_summary <- outlier_data %>%
    group_by(exporter) %>%
    summarise(
      n_hs6_exported = n(),
      n_times_outlier = sum(.data$is_outlier, na.rm = TRUE),
      pct_products_outlier = round(n_times_outlier / n_hs6_exported * 100, 1),
      avg_abs_deviation = round(mean(abs(pct_deviation_from_median), na.rm = TRUE), 1),
      n_high_outlier = sum(.data$is_outlier & pct_deviation_from_median > 0, na.rm = TRUE),
      n_low_outlier = sum(.data$is_outlier & pct_deviation_from_median < 0, na.rm = TRUE),
      total_trade_value = sum(value, na.rm = TRUE)
    ) %>%
    filter(n_times_outlier > 0) %>%
    arrange(desc(n_times_outlier))
  
  return(list(
    outlier_details = outlier_countries_by_hs6,
    hs6_summary = hs6_outlier_summary,
    country_summary = country_outlier_summary
  ))
}

# Function to visualize outliers for a specific HS6
plot_hs6_outliers <- function(data, hs6_code, save_plot = FALSE) {
  
  plot_data <- data %>% 
    filter(hs6 == hs6_code) %>%
    mutate(
      outlier_label = ifelse(is_outlier, exporter, ""),
      color_group = case_when(
        outlier_count == 3 ~ "Extreme outlier (3 methods)",
        outlier_count == 2 ~ "Outlier (2 methods)",
        outlier_count == 1 ~ "Potential outlier (1 method)",
        TRUE ~ "Normal"
      )
    )
  
  p <- ggplot(plot_data, aes(x = reorder(exporter, unit_value), y = unit_value)) +
    geom_hline(yintercept = unique(plot_data$median_uv), 
               linetype = "dashed", color = "blue", alpha = 0.5) +
    geom_hline(yintercept = unique(plot_data$lower_fence), 
               linetype = "dotted", color = "red", alpha = 0.5) +
    geom_hline(yintercept = unique(plot_data$upper_fence), 
               linetype = "dotted", color = "red", alpha = 0.5) +
    geom_point(aes(color = color_group, size = value), alpha = 0.7) +
    geom_text(aes(label = outlier_label), vjust = -0.5, hjust = 0.5, 
              angle = 45, size = 3) +
    scale_color_manual(values = c(
      "Normal" = "gray60",
      "Potential outlier (1 method)" = "orange",
      "Outlier (2 methods)" = "darkorange",
      "Extreme outlier (3 methods)" = "red"
    )) +
    scale_size_continuous(name = "Trade Value", labels = scales::comma) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      legend.position = "bottom",
      legend.box = "vertical"
    ) +
    labs(
      title = paste("Unit Value Distribution for HS6:", hs6_code),
      subtitle = paste("Median:", round(unique(plot_data$median_uv), 2), 
                      "| Red lines: IQR fences | Blue line: Median"),
      x = "Exporter Country",
      y = "Unit Value",
      color = "Classification"
    )
  
  if (save_plot) {
    ggsave(paste0("hs6_", hs6_code, "_outliers.png"), p, 
           width = 12, height = 8, dpi = 300)
  }
  
  return(p)
}

# Main analysis workflow
# Assuming your data is in 'china_imports'
# Run the analysis
outlier_results <- detect_outlier_countries(china_imports)

# Generate reports
reports <- create_outlier_report(outlier_results)

# Export detailed outlier report
write.csv(reports$outlier_details, 
          "outlier_countries_detailed.csv", 
          row.names = FALSE)

# Export HS6 summary (products with most outliers)
write.csv(reports$hs6_summary, 
          "hs6_outlier_summary.csv", 
          row.names = FALSE)

# Export country summary (frequent outlier countries)
write.csv(reports$country_summary, 
          "country_outlier_summary.csv", 
          row.names = FALSE)

# Print summary statistics
cat("=== OUTLIER DETECTION SUMMARY ===\n")
cat("Total HS6 codes analyzed:", n_distinct(outlier_results$hs6), "\n")
cat("Total country-product observations:", nrow(outlier_results), "\n")
cat("Total outliers detected:", sum(outlier_results$is_outlier), "\n")
cat("Outlier rate:", round(sum(outlier_results$is_outlier) / nrow(outlier_results) * 100, 2), "%\n\n")

# Show top 10 HS6 codes with highest outlier rates
cat("=== TOP 10 HS6 CODES BY OUTLIER RATE ===\n")
print(head(reports$hs6_summary, 10))

# Show top 10 countries that are most frequently outliers
cat("\n=== TOP 10 COUNTRIES BY OUTLIER FREQUENCY ===\n")
print(head(reports$country_summary, 10))

# Create visualizations for top 5 HS6 codes with most outliers
top_hs6_codes <- head(reports$hs6_summary$hs6, 5)
for (hs6_code in top_hs6_codes) {
  p <- plot_hs6_outliers(outlier_results, hs6_code, save_plot = TRUE)
  print(p)
}

# Additional analysis: Bilateral outlier patterns
# Check if certain country pairs consistently show outlier behavior
bilateral_patterns <- outlier_results %>%
  filter(.data$is_outlier == TRUE) %>%
  group_by(exporter) %>%
  summarise(
    outlier_hs6_codes = paste(unique(hs6), collapse = ", "),
    n_outlier_products = n(),
    avg_deviation = round(mean(abs(pct_deviation_from_median)), 1),
    primarily_overpriced = sum(pct_deviation_from_median > 0) > sum(pct_deviation_from_median < 0)
  ) %>%
  arrange(desc(n_outlier_products))

write.csv(bilateral_patterns, 
          "bilateral_outlier_patterns.csv", 
          row.names = FALSE)

# Function to create a focused report for a specific HS6
create_hs6_report <- function(data, hs6_code) {
  hs6_data <- data %>% filter(hs6 == hs6_code)
  
  report <- hs6_data %>%
    arrange(unit_value) %>%
    select(exporter, unit_value, pct_deviation_from_median, 
           is_outlier, value, quantity) %>%
    mutate(
      rank = row_number(),
      unit_value = round(unit_value, 2),
      pct_deviation_from_median = round(pct_deviation_from_median, 1)
    )
  
  cat("\n=== REPORT FOR HS6:", hs6_code, "===\n")
  cat("Number of exporters:", n_distinct(hs6_data$exporter), "\n")
  cat("Median unit value:", round(median(hs6_data$unit_value), 2), "\n")
  cat("Number of outliers:", sum(hs6_data$is_outlier), "\n\n")
  
  print(kable(report))
  
  return(report)
}

# Example: Create detailed report for a specific HS6
# create_hs6_report(outlier_results, "123456")
