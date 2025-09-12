library(tidyverse)
library(ggplot2)

# Function to identify low-value goods with high AVEs using Jenks natural breaks
identify_priority_goods <- function(data, 
                                   unit_value_col = "unit_value",
                                   ave_col = "ave",
                                   id_col = "product_name",
                                   target_value_pct = NULL,
                                   target_ave_pct = NULL,
                                   use_jenks = TRUE) {
  
  # Rename columns for consistency
  analysis_data <- data %>%
    rename(
      unit_value = !!unit_value_col,
      ave = !!ave_col,
      product_id = !!id_col
    )
  
  # Preserve all original columns
  results <- analysis_data
  
  # ==============================================================
  # Jenks Natural Breaks or Percentile-Based Identification
  # ==============================================================
  
  # Use Jenks natural breaks if thresholds not provided and use_jenks = TRUE
  if (use_jenks && (is.null(target_value_pct) || is.null(target_ave_pct))) {
    
    # Helper function for Jenks natural breaks
    jenks_breaks <- function(values, n_classes = 3) {
      if (!requireNamespace("classInt", quietly = TRUE)) {
        # Fallback implementation if classInt not available
        message("Package 'classInt' not found. Using quantile approximation for natural breaks.")
        breaks <- quantile(values, probs = seq(0, 1, length.out = n_classes + 1))
      } else {
        # Use proper Jenks natural breaks
        breaks_obj <- classInt::classIntervals(values, n = n_classes, style = "jenks")
        breaks <- breaks_obj$brks
      }
      return(breaks)
    }
    
    # For unit value - find 3 groups (low, medium, high)
    if (is.null(target_value_pct)) {
      value_breaks <- jenks_breaks(results$unit_value, n_classes = 3)
      # Use break between low and medium groups
      value_threshold <- value_breaks[2]
      target_value_pct <- ecdf(results$unit_value)(value_threshold)
      
      # Sanity check - if Jenks puts break too high or low, use fallback
      if (target_value_pct < 0.15 || target_value_pct > 0.5) {
        message(paste("Jenks break for value at", round(target_value_pct * 100, 1), 
                     "percentile seems extreme. Using 33rd percentile instead."))
        target_value_pct <- 0.33
      }
    }
    
    # For AVE - find 3 groups (low, medium, high)
    if (is.null(target_ave_pct)) {
      ave_breaks <- jenks_breaks(results$ave, n_classes = 3)
      # Use break between medium and high groups
      ave_threshold <- ave_breaks[3]
      target_ave_pct <- ecdf(results$ave)(ave_threshold)
      
      # Sanity check - if Jenks puts break too high or low, use fallback
      if (target_ave_pct < 0.5 || target_ave_pct > 0.85) {
        message(paste("Jenks break for AVE at", round(target_ave_pct * 100, 1),
                     "percentile seems extreme. Using 67th percentile instead."))
        target_ave_pct <- 0.67
      }
    }
    
    message(paste("Using Jenks natural breaks: Value <", round(target_value_pct * 100, 1),
                 "th percentile, AVE >", round(target_ave_pct * 100, 1), "th percentile"))
  } else {
    # Use provided thresholds or defaults
    if (is.null(target_value_pct)) target_value_pct <- 0.33
    if (is.null(target_ave_pct)) target_ave_pct <- 0.67
    
    message(paste("Using percentile thresholds: Value <", round(target_value_pct * 100, 1),
                 "th percentile, AVE >", round(target_ave_pct * 100, 1), "th percentile"))
  }
  
  # Calculate percentiles and identify target goods
  results <- results %>%
    mutate(
      # Calculate percentile ranks
      value_percentile = percent_rank(unit_value),
      ave_percentile = percent_rank(ave),
      
      # Actual threshold values
      value_threshold = quantile(unit_value, target_value_pct),
      ave_threshold = quantile(ave, target_ave_pct),
      
      # Binary classification
      is_low_value = unit_value <= value_threshold,
      is_high_ave = ave >= ave_threshold,
      is_priority = is_low_value & is_high_ave,
      
      # Create priority score for ranking within priority goods
      # Using sigmoid function for smooth transition
      value_score = 1 / (1 + exp(10 * (value_percentile - target_value_pct))),
      ave_score = 1 / (1 + exp(-10 * (ave_percentile - target_ave_pct))),
      priority_score = value_score * ave_score,
      
      # Category classification
      category = case_when(
        is_priority ~ "High Priority",
        is_low_value & !is_high_ave ~ "Low Value Only",
        !is_low_value & is_high_ave ~ "High AVE Only",
        TRUE ~ "Not Priority"
      )
    )
  
  # ==============================================================
  # Generate Summary Statistics
  # ==============================================================
  
  summary_stats <- list(
    total_products = nrow(results),
    
    thresholds = list(
      value_percentile_used = target_value_pct,
      ave_percentile_used = target_ave_pct,
      value_threshold = unique(results$value_threshold)[1],
      ave_threshold = unique(results$ave_threshold)[1]
    ),
    
    category_breakdown = results %>%
      group_by(category) %>%
      summarise(
        count = n(),
        pct = n() / nrow(results) * 100,
        avg_unit_value = mean(unit_value),
        avg_ave = mean(ave),
        .groups = 'drop'
      ) %>%
      arrange(match(category, c("High Priority", "Low Value Only", "High AVE Only", "Not Priority"))),
    
    priority_products = results %>%
      filter(is_priority) %>%
      arrange(desc(priority_score)) %>%
      select(product_id, unit_value, ave, priority_score, value_percentile, ave_percentile)
  )
  
  # ==============================================================
  # Create Visualizations
  # ==============================================================
  
  # Plot 1: Scatter plot with quadrants
  p1 <- ggplot(results, aes(x = unit_value, y = ave)) +
    geom_point(aes(color = category), alpha = 0.6, size = 2) +
    geom_vline(xintercept = unique(results$value_threshold)[1], 
               linetype = "dashed", color = "blue", alpha = 0.5) +
    geom_hline(yintercept = unique(results$ave_threshold)[1], 
               linetype = "dashed", color = "blue", alpha = 0.5) +
    scale_color_manual(values = c("High Priority" = "red",
                                 "Low Value Only" = "orange",
                                 "High AVE Only" = "purple",
                                 "Not Priority" = "gray60")) +
    labs(
      title = "Product Classification by Unit Value and AVE",
      subtitle = paste("Thresholds: Value <", round(unique(results$value_threshold)[1], 2),
                      ", AVE >", round(unique(results$ave_threshold)[1], 2)),
      x = "Unit Value",
      y = "Ad Valorem Equivalent (%)",
      color = "Category"
    ) +
    theme_minimal()
  
  # Plot 2: Distribution of categories
  p2 <- ggplot(results, aes(x = category, fill = category)) +
    geom_bar() +
    geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
    scale_fill_manual(values = c("High Priority" = "red",
                                "Low Value Only" = "orange",
                                "High AVE Only" = "purple",
                                "Not Priority" = "gray60")) +
    scale_x_discrete(limits = c("High Priority", "Low Value Only", "High AVE Only", "Not Priority")) +
    labs(
      title = "Distribution of Products by Category",
      x = "Category",
      y = "Count"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Return results
  return(list(
    data = results,
    summary = summary_stats,
    plots = list(
      scatter = p1,
      distribution = p2
    )
  ))
}

# ==============================================================
# EXAMPLE USAGE
# ==============================================================

# Generate sample data
set.seed(42)
sample_data <- data.frame(
  product_name = paste("Product", 1:500),
  description = paste("Description for product", 1:500),
  unit_value = exp(rnorm(500, log(50), 1.5)),
  ave = pmax(0, 70 / exp(rnorm(500, log(50), 1.5)) + rnorm(500, 0, 5))  # If AVE ≈ constant/unit_value
)

# Run analysis with Jenks natural breaks
results <- identify_priority_goods(
  data = sample_data,
  unit_value_col = "unit_value",
  ave_col = "ave",
  id_col = "product_name",
  use_jenks = TRUE  # Set to FALSE for simple percentile thresholds
)

# Access priority products
priority_products <- results$data %>%
  filter(is_priority) %>%
  arrange(desc(priority_score)) %>%
  select(product_id, description, unit_value, ave, priority_score)

# Print summary
cat("ANALYSIS SUMMARY\n")
cat("=" , strrep("=", 50), "\n")
cat("Total products analyzed:", results$summary$total_products, "\n")
cat("Value threshold:", round(results$summary$thresholds$value_threshold, 2), "\n")
cat("AVE threshold:", round(results$summary$thresholds$ave_threshold, 2), "\n\n")

cat("Category Breakdown:\n")
print(results$summary$category_breakdown)

cat("\nTop 10 Priority Products:\n")
print(priority_products %>% head(10))

# Display plots
library(gridExtra)
grid.arrange(results$plots$scatter, results$plots$distribution, ncol = 2)

# Export priority products
write.csv(
  priority_products,
  "priority_products.csv",
  row.names = FALSE
)
