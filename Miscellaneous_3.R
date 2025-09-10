library(tidyverse)
library(cluster)
library(ggplot2)
library(gridExtra)

# Function to create comprehensive consensus analysis
create_consensus_analysis <- function(data, 
                                     unit_value_col = "unit_value",
                                     ave_col = "ave",
                                     id_col = "product_name",
                                     method_weights = NULL) {
  
  # Rename columns for consistency
  analysis_data <- data %>%
    rename(
      unit_value = !!unit_value_col,
      ave = !!ave_col,
      product_id = !!id_col
    )
  
  # Initialize results dataframe
  results <- analysis_data
  
  # ==============================================================
  # METHOD 1: Percentile-Based Score
  # ==============================================================
  
  results <- results %>%
    mutate(
      # Calculate percentile ranks
      value_percentile = percent_rank(unit_value),
      ave_percentile = percent_rank(ave),
      
      # Score: low value percentile (inverted) + high AVE percentile
      score_percentile = (1 - value_percentile) * ave_percentile,
      
      # Identify if in target zones (bottom 33% value, top 33% AVE)
      in_target_zone = (value_percentile <= 0.33) & (ave_percentile >= 0.67)
    )
  
  # ==============================================================
  # METHOD 2: Normalized Ratio Score
  # ==============================================================
  
  results <- results %>%
    mutate(
      # Normalize to 0-1 scale
      unit_value_norm = (unit_value - min(unit_value)) / 
                        (max(unit_value) - min(unit_value)),
      ave_norm = (ave - min(ave)) / (max(ave) - min(ave)),
      
      # Combined score
      score_ratio = (1 - unit_value_norm) * ave_norm
    )
  
  # ==============================================================
  # METHOD 3: Statistical Outlier Score
  # ==============================================================
  
  results <- results %>%
    mutate(
      # Calculate z-scores
      value_zscore = scale(unit_value)[,1],
      ave_zscore = scale(ave)[,1],
      
      # Convert to probabilities using normal CDF
      value_prob = pnorm(-value_zscore), # Negative because we want low values
      ave_prob = pnorm(ave_zscore),
      
      # Combined outlier score
      score_outlier = value_prob * ave_prob,
      
      # Flag extreme outliers
      is_extreme_outlier = (value_zscore < -2) & (ave_zscore > 2)
    )
  
  # ==============================================================
  # METHOD 4: Regression Residual Score
  # ==============================================================
  
  # Fit model: AVE as function of log(unit_value)
  # Using log to handle potential non-linear relationship
  model_data <- results %>%
    filter(unit_value > 0) %>%  # Ensure positive values for log
    mutate(log_value = log(unit_value))
  
  regression_model <- lm(ave ~ log_value, data = model_data)
  
  # Calculate residuals and standardize
  model_data$residuals <- residuals(regression_model)
  model_data$fitted_values <- fitted(regression_model)
  model_data$standardized_residuals <- model_data$residuals / sd(model_data$residuals)
  
  # Convert to score (higher residuals = higher than expected AVE)
  model_data <- model_data %>%
    mutate(
      # Use normal CDF to convert to 0-1 score
      score_residual = pnorm(standardized_residuals),
      
      # Flag high residuals
      high_residual = standardized_residuals > 1
    )
  
  # Merge back to results
  results <- results %>%
    left_join(
      model_data %>% select(product_id, score_residual, standardized_residuals, high_residual),
      by = "product_id"
    )
  
  # ==============================================================
  # METHOD 5: Cluster Analysis Score
  # ==============================================================
  
  # Prepare data for clustering
  cluster_data <- results %>%
    select(unit_value, ave) %>%
    scale()
  
  # K-means with 4 clusters
  set.seed(123)
  kmeans_result <- kmeans(cluster_data, centers = 4, nstart = 25)
  results$cluster <- kmeans_result$cluster
  
  # Identify cluster characteristics
  cluster_summary <- results %>%
    group_by(cluster) %>%
    summarise(
      mean_value = mean(unit_value),
      mean_ave = mean(ave),
      n = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      # Score clusters based on low value and high AVE
      cluster_score = (1 - percent_rank(mean_value)) * percent_rank(mean_ave)
    ) %>%
    arrange(desc(cluster_score))
  
  # Assign cluster scores to products
  results <- results %>%
    left_join(
      cluster_summary %>% select(cluster, cluster_score),
      by = "cluster"
    ) %>%
    rename(score_cluster = cluster_score)
  
  # ==============================================================
  # CONSENSUS CALCULATION
  # ==============================================================
  
  # Define default weights if not provided
  if (is.null(method_weights)) {
    method_weights <- c(
      percentile = 0.20,
      ratio = 0.20,
      outlier = 0.20,
      residual = 0.20,
      cluster = 0.20
    )
  }
  
  # Calculate weighted consensus score
  results <- results %>%
    mutate(
      # Weighted average of all scores
      consensus_score = (
        score_percentile * method_weights["percentile"] +
        score_ratio * method_weights["ratio"] +
        score_outlier * method_weights["outlier"] +
        score_residual * method_weights["residual"] +
        score_cluster * method_weights["cluster"]
      ),
      
      # Calculate agreement between methods (standard deviation)
      method_agreement = apply(
        cbind(score_percentile, score_ratio, score_outlier, 
              score_residual, score_cluster),
        1, 
        function(x) 1 - sd(x, na.rm = TRUE)  # 1 - std dev for agreement score
      ),
      
      # Count how many methods flag this as priority (>0.7 score)
      methods_flagging = rowSums(
        cbind(
          score_percentile > 0.7,
          score_ratio > 0.7,
          score_outlier > 0.7,
          score_residual > 0.7,
          score_cluster > 0.7
        ),
        na.rm = TRUE
      ),
      
      # Create priority categories
      priority_category = case_when(
        consensus_score >= 0.8 & methods_flagging >= 4 ~ "Very High Priority",
        consensus_score >= 0.7 & methods_flagging >= 3 ~ "High Priority",
        consensus_score >= 0.6 & methods_flagging >= 2 ~ "Medium Priority",
        consensus_score >= 0.5 ~ "Low Priority",
        TRUE ~ "Not Priority"
      ),
      
      # Confidence in classification
      confidence = case_when(
        method_agreement >= 0.8 ~ "High Confidence",
        method_agreement >= 0.6 ~ "Medium Confidence",
        TRUE ~ "Low Confidence"
      )
    )
  
  # ==============================================================
  # GENERATE REPORT
  # ==============================================================
  
  # Summary statistics
  summary_stats <- list(
    total_products = nrow(results),
    
    priority_breakdown = results %>%
      group_by(priority_category) %>%
      summarise(
        count = n(),
        pct = n() / nrow(results) * 100,
        avg_consensus_score = mean(consensus_score),
        avg_unit_value = mean(unit_value),
        avg_ave = mean(ave),
        .groups = 'drop'
      ),
    
    confidence_breakdown = results %>%
      group_by(confidence) %>%
      summarise(
        count = n(),
        pct = n() / nrow(results) * 100,
        .groups = 'drop'
      ),
    
    method_correlations = cor(
      results %>% 
        select(score_percentile, score_ratio, score_outlier, 
               score_residual, score_cluster),
      use = "complete.obs"
    ),
    
    top_consensus_products = results %>%
      arrange(desc(consensus_score)) %>%
      head(20) %>%
      select(product_id, unit_value, ave, consensus_score, 
             priority_category, confidence, methods_flagging),
    
    regression_summary = summary(regression_model)
  )
  
  # ==============================================================
  # CREATE VISUALIZATIONS
  # ==============================================================
  
  # Plot 1: Scatter plot with consensus scores
  p1 <- ggplot(results, aes(x = unit_value, y = ave)) +
    geom_point(aes(color = consensus_score, size = method_agreement), alpha = 0.6) +
    scale_color_gradient2(low = "blue", mid = "yellow", high = "red", midpoint = 0.5) +
    scale_size_continuous(range = c(1, 4)) +
    labs(
      title = "Consensus Analysis: Unit Value vs AVE",
      x = "Unit Value",
      y = "Ad Valorem Equivalent (%)",
      color = "Consensus\nScore",
      size = "Method\nAgreement"
    ) +
    theme_minimal()
  
  # Plot 2: Method comparison
  method_scores <- results %>%
    select(product_id, score_percentile, score_ratio, score_outlier, 
           score_residual, score_cluster) %>%
    pivot_longer(
      cols = starts_with("score_"),
      names_to = "method",
      values_to = "score"
    ) %>%
    mutate(method = str_remove(method, "score_"))
  
  p2 <- ggplot(method_scores, aes(x = method, y = score)) +
    geom_boxplot(fill = "lightblue", alpha = 0.7) +
    geom_jitter(alpha = 0.1, width = 0.2) +
    labs(
      title = "Score Distribution by Method",
      x = "Method",
      y = "Score"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Plot 3: Priority distribution
  p3 <- ggplot(results, aes(x = priority_category, fill = confidence)) +
    geom_bar(position = "stack") +
    labs(
      title = "Priority Categories with Confidence Levels",
      x = "Priority Category",
      y = "Count",
      fill = "Confidence"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Plot 4: Consensus score distribution
  p4 <- ggplot(results, aes(x = consensus_score)) +
    geom_histogram(bins = 30, fill = "darkgreen", alpha = 0.7) +
    geom_vline(xintercept = c(0.5, 0.6, 0.7, 0.8), 
               linetype = "dashed", color = "red", alpha = 0.5) +
    labs(
      title = "Distribution of Consensus Scores",
      x = "Consensus Score",
      y = "Count"
    ) +
    theme_minimal()
  
  # Return comprehensive results
  return(list(
    data = results,
    summary = summary_stats,
    plots = list(
      scatter = p1,
      methods = p2,
      priorities = p3,
      distribution = p4
    ),
    model = regression_model,
    cluster_summary = cluster_summary
  ))
}

# ==============================================================
# EXAMPLE USAGE
# ==============================================================

# Generate sample data for demonstration
set.seed(42)
sample_data <- data.frame(
  product_name = paste("Product", 1:500),
  unit_value = exp(rnorm(500, log(50), 1.5)),
  ave = pmax(0, 15 + rnorm(500, 0, 10) - log(exp(rnorm(500, log(50), 1.5))) * 2 + 
             ifelse(runif(500) > 0.9, runif(500, 20, 40), 0))
)

# Run consensus analysis
consensus_results <- create_consensus_analysis(
  data = sample_data,
  unit_value_col = "unit_value",
  ave_col = "ave",
  id_col = "product_name"
)

# Access results
top_products <- consensus_results$data %>%
  filter(priority_category %in% c("Very High Priority", "High Priority")) %>%
  arrange(desc(consensus_score))

print("Top 10 Priority Products:")
print(top_products %>% head(10))

# View summary statistics
print("\nPriority Breakdown:")
print(consensus_results$summary$priority_breakdown)

# Plot all visualizations
grid.arrange(
  consensus_results$plots$scatter,
  consensus_results$plots$methods,
  consensus_results$plots$priorities,
  consensus_results$plots$distribution,
  ncol = 2
)

# Export final results
write.csv(
  consensus_results$data %>%
    arrange(desc(consensus_score)),
  "consensus_analysis_results.csv",
  row.names = FALSE
)

# Create detailed report for high priority items
high_priority_report <- consensus_results$data %>%
  filter(priority_category %in% c("Very High Priority", "High Priority")) %>%
  select(
    product_id, unit_value, ave, 
    consensus_score, priority_category, confidence,
    methods_flagging,
    score_percentile, score_ratio, score_outlier, 
    score_residual, score_cluster
  ) %>%
  arrange(desc(consensus_score))

write.csv(
  high_priority_report,
  "high_priority_products_detailed.csv",
  row.names = FALSE
)
