# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Function to detect outlier HS6 products for a single exporter
detect_outlier_products_single_country <- function(data, method = "all") {
  
  # Since we have one exporter, we look for outlier products instead
  outlier_analysis <- data %>%
    mutate(
      # Log transform to handle skewed price distributions
      log_unit_value = log(unit_value + 1),
      
      # Overall statistics across all products
      median_uv_all = median(unit_value, na.rm = TRUE),
      mean_uv_all = mean(unit_value, na.rm = TRUE),
      sd_uv_all = sd(unit_value, na.rm = TRUE),
      
      median_log_uv = median(log_unit_value, na.rm = TRUE),
      mean_log_uv = mean(log_unit_value, na.rm = TRUE),
      sd_log_uv = sd(log_unit_value, na.rm = TRUE),
      
      # IQR method on log scale
      q1 = quantile(log_unit_value, 0.25, na.rm = TRUE),
      q3 = quantile(log_unit_value, 0.75, na.rm = TRUE),
      iqr = q3 - q1,
      lower_fence = q1 - 1.5 * iqr,
      upper_fence = q3 + 1.5 * iqr,
      is_outlier_iqr = log_unit_value < lower_fence | log_unit_value > upper_fence,
      
      # Z-score on log scale
      z_score = (log_unit_value - mean_log_uv) / sd_log_uv,
      is_outlier_zscore = abs(z_score) > 3,
      
      # MAD on log scale
      mad_log_uv = mad(log_unit_value, na.rm = TRUE),
      modified_z_score = ifelse(mad_log_uv > 0, 
                                0.6745 * (log_unit_value - median_log_uv) / mad_log_uv, 
                                0),
      is_outlier_mad = abs(modified_z_score) > 3.5,
      
      # Deviation metrics
      pct_deviation_from_median = (unit_value - median_uv_all) / median_uv_all * 100,
      
      # Consensus outlier
      outlier_count = is_outlier_iqr + is_outlier_zscore + is_outlier_mad,
      is_outlier = outlier_count >= 2
    )
  
  return(outlier_analysis)
}

# Function to detect outliers within product categories (2-digit HS)
detect_outliers_by_category <- function(data) {
  
  # Extract 2-digit HS code for category grouping
  data <- data %>%
    mutate(hs2 = substr(hs6, 1, 2))
  
  outlier_analysis <- data %>%
    group_by(hs2) %>%
    mutate(
      n_products_in_category = n(),
      median_uv_category = median(unit_value, na.rm = TRUE),
      mean_uv_category = mean(unit_value, na.rm = TRUE),
      sd_uv_category = sd(unit_value, na.rm = TRUE),
      
      # Category-specific outlier detection
      z_score_category = (unit_value - mean_uv_category) / sd_uv_category,
      
      # IQR within category
      q1_cat = quantile(unit_value, 0.25, na.rm = TRUE),
      q3_cat = quantile(unit_value, 0.75, na.rm = TRUE),
      iqr_cat = q3_cat - q1_cat,
      is_outlier_within_category = unit_value < (q1_cat - 1.5 * iqr_cat) | 
                                   unit_value > (q3_cat + 1.5 * iqr_cat),
      
      pct_deviation_from_category = (unit_value - median_uv_category) / median_uv_category * 100
    ) %>%
    ungroup()
  
  return(outlier_analysis)
}

# Create summary reports for single country analysis
create_single_country_report <- function(outlier_data) {
  
  # Report 1: Outlier products
  outlier_products <- outlier_data %>%
    filter(is_outlier) %>%
    arrange(desc(abs(pct_deviation_from_median))) %>%
    select(hs6, unit_value, value, quantity, 
           pct_deviation_from_median, z_score, outlier_count) %>%
    mutate(
      outlier_type = ifelse(pct_deviation_from_median > 0, "Unusually High", "Unusually Low"),
      pct_deviation_from_median = round(pct_deviation_from_median, 1),
      z_score = round(z_score, 2)
    )
  
  # Report 2: Summary statistics
  summary_stats <- outlier_data %>%
    summarise(
      total_products = n(),
      total_outliers = sum(is_outlier),
      outlier_rate = round(total_outliers / total_products * 100, 2),
      median_unit_value = median(unit_value, na.rm = TRUE),
      mean_unit_value = mean(unit_value, na.rm = TRUE),
      cv = sd(unit_value, na.rm = TRUE) / mean(unit_value, na.rm = TRUE)
    )
  
  # Report 3: Distribution of outlier scores
  outlier_distribution <- table(outlier_data$outlier_count)
  
  return(list(
    outlier_products = outlier_products,
    summary_stats = summary_stats,
    outlier_distribution = outlier_distribution
  ))
}

# Visualize price distribution
plot_price_distribution <- function(data, log_scale = TRUE) {
  
  if (log_scale) {
    p <- ggplot(data, aes(x = log(unit_value + 1))) +
      geom_histogram(bins = 50, fill = "skyblue", alpha = 0.7) +
      geom_vline(xintercept = median(log(data$unit_value + 1)), 
                 color = "red", linetype = "dashed") +
      theme_minimal() +
      labs(title = "Distribution of Log Unit Values",
           subtitle = "Red line shows median",
           x = "Log(Unit Value + 1)", y = "Count")
  } else {
    p <- ggplot(data, aes(x = unit_value)) +
      geom_histogram(bins = 50, fill = "skyblue", alpha = 0.7) +
      geom_vline(xintercept = median(data$unit_value), 
                 color = "red", linetype = "dashed") +
      theme_minimal() +
      labs(title = "Distribution of Unit Values",
           subtitle = "Red line shows median",
           x = "Unit Value", y = "Count")
  }
  
  return(p)
}

# Time series analysis if you have temporal data
analyze_temporal_outliers <- function(data, date_column = "date") {
  if (date_column %in% names(data)) {
    temporal_analysis <- data %>%
      group_by(hs6) %>%
      arrange(!!sym(date_column)) %>%
      mutate(
        # Calculate rolling statistics
        rolling_mean = zoo::rollmean(unit_value, k = 3, fill = NA, align = "right"),
        pct_change = (unit_value - lag(unit_value)) / lag(unit_value) * 100,
        
        # Flag sudden price changes
        is_price_spike = abs(pct_change) > 50  # 50% change threshold
      ) %>%
      ungroup()
    
    return(temporal_analysis)
  } else {
    message("No date column found for temporal analysis")
    return(data)
  }
}

# Main workflow for single country analysis
# Assuming your U.S.-only data is in 'us_china_exports'

# Method 1: Detect outliers across all products
outlier_results <- detect_outlier_products_single_country(us_china_exports)

# Method 2: Detect outliers within product categories
category_outliers <- detect_outliers_by_category(us_china_exports)

# Generate reports
single_country_report <- create_single_country_report(outlier_results)

# Export reports
write.csv(single_country_report$outlier_products, 
          "us_outlier_products.csv", 
          row.names = FALSE)

# Visualize
print(plot_price_distribution(us_china_exports, log_scale = TRUE))

# Print summary
cat("=== U.S. EXPORT OUTLIER ANALYSIS ===\n")
print(single_country_report$summary_stats)
cat("\nDistribution of outlier scores:\n")
print(single_country_report$outlier_distribution)

# Top 10 most unusual products
cat("\n=== TOP 10 MOST UNUSUAL PRODUCTS ===\n")
print(head(single_country_report$outlier_products, 10))

# Analyze by category if needed
category_summary <- category_outliers %>%
  group_by(hs2) %>%
  summarise(
    n_products = n(),
    n_outliers = sum(is_outlier_within_category),
    median_price = median(unit_value),
    cv = sd(unit_value) / mean(unit_value)
  ) %>%
  arrange(desc(n_outliers))

cat("\n=== CATEGORIES WITH MOST OUTLIERS ===\n")
print(head(category_summary, 10))
