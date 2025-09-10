library(tidyverse)
library(cluster)
library(ggplot2)
library(gridExtra)

# Helper function to calculate MAD z-scores
mad_zscore <- function(x) {
  median_x <- median(x, na.rm = TRUE)
  mad_x <- mad(x, na.rm = TRUE)
  if (mad_x == 0) mad_x <- 1.4826 * IQR(x, na.rm = TRUE) / 1.349  # Fallback to IQR-based estimate
  (x - median_x) / mad_x
}

# Helper function to find optimal number of clusters
find_optimal_clusters <- function(data, max_k = 10) {
  # Scale the data
  scaled_data <- scale(data)
  
  # Calculate WSS for different k values
  wss <- sapply(1:max_k, function(k) {
    kmeans(scaled_data, centers = k, nstart = 25)$tot.withinss
  })
  
  # Calculate the elbow point using the "knee" method
  # Look for the point with maximum distance from the line connecting first and last points
  x <- 1:max_k
  y <- wss
  
  # Normalize to 0-1 scale for fair comparison
  x_norm <- (x - min(x)) / (max(x) - min(x))
  y_norm <- (y - min(y)) / (max(y) - min(y))
  
  # Calculate perpendicular distance from each point to the line
  distances <- numeric(max_k)
  for (i in 2:(max_k-1)) {
    # Distance from point to line connecting first and last points
    distances[i] <- abs((y_norm[max_k] - y_norm[1]) * x_norm[i] - 
                       (x_norm[max_k] - x_norm[1]) * y_norm[i] + 
                       x_norm[max_k] * y_norm[1] - y_norm[max_k] * x_norm[1]) / 
                   sqrt((y_norm[max_k] - y_norm[1])^2 + (x_norm[max_k] - x_norm[1])^2)
  }
  
  # Optimal k is where distance is maximum (the "elbow")
  optimal_k <- which.max(distances)
  
  # But ensure minimum of 3 clusters for meaningful segmentation
  optimal_k <- max(3, optimal_k)
  
  return(list(
    optimal_k = optimal_k,
    wss = wss,
    distances = distances
  ))
}

# Function to create comprehensive consensus analysis
create_consensus_analysis <- function(data, 
                                     unit_value_col = "unit_value",
                                     ave_col = "ave",
                                     id_col = "product_name",
                                     method_weights = NULL,
                                     target_value_pct = NULL,
                                     target_ave_pct = NULL,
                                     optimize_clusters = TRUE,
                                     max_clusters = 10) {
  
  # Rename columns for consistency
  analysis_data <- data %>%
    rename(
      unit_value = !!unit_value_col,
      ave = !!ave_col,
      product_id = !!id_col
    )
  
  # Initialize results dataframe - remove any existing score columns to avoid conflicts
  results <- analysis_data %>%
    select(-any_of(c("consensus_score", "score_percentile", "score_ratio", 
                     "score_outlier", "score_residual", "score_cluster")))
  
  # ==============================================================
  # METHOD 1: Percentile-Based Score (Using Jenks Natural Breaks)
  # ==============================================================
  
  # Use Jenks natural breaks if thresholds not provided
  if (is.null(target_value_pct) || is.null(target_ave_pct)) {
    
    # Helper function for Jenks natural breaks
    # Using Fisher-Jenks algorithm for optimal classification
    jenks_breaks <- function(values, n_classes = 3) {
      if (!requireNamespace("classInt", quietly = TRUE)) {
        # Fallback implementation if classInt not available
        # Use quantile-based breaks as approximation
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
  }
  
  results <- results %>%
    mutate(
      # Calculate percentile ranks
      value_percentile = percent_rank(unit_value),
      ave_percentile = percent_rank(ave),
      
      # Score using adaptive thresholds with smooth transition
      # Using sigmoid function for smooth transition around thresholds
      value_score = 1 / (1 + exp(10 * (value_percentile - target_value_pct))),
      ave_score = 1 / (1 + exp(-10 * (ave_percentile - target_ave_pct))),
      
      # Combined score
      score_percentile = value_score * ave_score,
      
      # Binary flag for being in target zone
      in_target_zone = (value_percentile <= target_value_pct) & 
                       (ave_percentile >= target_ave_pct)
    )
  
  # ==============================================================
  # METHOD 2: Normalized Ratio Score (Continuous Balance Measure)
  # ==============================================================
  # This method provides a smooth, continuous score that naturally balances
  # both dimensions through multiplication. It acts as a moderate voice in
  # the consensus, favoring products that perform well on BOTH metrics
  # rather than extreme outliers on just one.
  
  results <- results %>%
    mutate(
      # Robust normalization using percentile ranks to handle outliers
      unit_value_norm = 1 - percent_rank(unit_value),  # Invert so low values get high scores
      ave_norm = percent_rank(ave),
      
      # Combined score - multiplicative relationship ensures both dimensions matter
      score_ratio = unit_value_norm * ave_norm
    )
  
  # ==============================================================
  # METHOD 3: Statistical Outlier Score (Using MAD for robustness)
  # ==============================================================
  
  results <- results %>%
    mutate(
      # Calculate MAD z-scores for robustness against outliers
      value_mad_zscore = mad_zscore(unit_value),
      ave_mad_zscore = mad_zscore(ave),
      
      # Convert to probabilities using normal CDF
      # Negative for value because we want low values
      value_prob = pnorm(-value_mad_zscore),
      ave_prob = pnorm(ave_mad_zscore),
      
      # Combined outlier score
      score_outlier = value_prob * ave_prob,
      
      # Flag extreme outliers using MAD threshold (more robust)
      is_extreme_outlier = (value_mad_zscore < -2.5) & (ave_mad_zscore > 2.5)
    )
  
  # ==============================================================
  # METHOD 4: Regression Residual Score (with transformation analysis)
  # ==============================================================
  
  # Check if AVE also needs transformation
  ave_skewness <- moments::skewness(results$ave)
  use_ave_transform <- abs(ave_skewness) > 1  # Threshold for significant skew
  
  # Prepare data for regression
  model_data <- results %>%
    filter(unit_value > 0, ave > 0) %>%  # Ensure positive values
    mutate(
      # Log transform unit value (justified by typical multiplicative scale)
      log_value = log(unit_value),
      # Transform AVE only if significantly skewed
      ave_transformed = if(use_ave_transform) log(ave + 1) else ave
    )
  
  # Fit appropriate model based on transformations
  if (use_ave_transform) {
    regression_model <- lm(ave_transformed ~ log_value, data = model_data)
    
    # Back-transform predictions for residual calculation
    model_data <- model_data %>%
      mutate(
        fitted_transformed = fitted(regression_model),
        fitted_values = exp(fitted_transformed) - 1,
        residuals = ave - fitted_values
      )
  } else {
    regression_model <- lm(ave ~ log_value, data = model_data)
    
    model_data <- model_data %>%
      mutate(
        fitted_values = fitted(regression_model),
        residuals = ave - fitted_values
      )
  }
  
  # Standardize residuals using MAD for robustness
  model_data <- model_data %>%
    mutate(
      standardized_residuals = mad_zscore(residuals),
      # Use normal CDF to convert to 0-1 score
      score_residual = pnorm(standardized_residuals),
      # Flag high residuals
      high_residual = standardized_residuals > 2
    )
  
  # Merge back to results
  results <- results %>%
    left_join(
      model_data %>% select(product_id, score_residual, standardized_residuals, high_residual),
      by = "product_id"
    ) %>%
    mutate(
      # Fill NA values for products that couldn't be included in regression
      score_residual = replace_na(score_residual, 0.5)
    )
  
  # ==============================================================
  # METHOD 5: Optimized Cluster Analysis
  # ==============================================================
  
  # Prepare data for clustering
  cluster_data <- results %>%
    select(unit_value, ave)
  
  # Determine optimal number of clusters if requested
  if (optimize_clusters) {
    cluster_optimization <- find_optimal_clusters(
      cluster_data,
      max_k = min(max_clusters, nrow(cluster_data) / 10)  # Ensure reasonable cluster sizes
    )
    n_clusters <- cluster_optimization$optimal_k
    
    message(paste("Optimal number of clusters determined:", n_clusters))
  } else {
    n_clusters <- 4
  }
  
  # Perform clustering with optimal k
  cluster_data_scaled <- scale(cluster_data)
  set.seed(123)
  kmeans_result <- kmeans(cluster_data_scaled, centers = n_clusters, nstart = 50, iter.max = 100)
  results$cluster <- kmeans_result$cluster
  
  # Identify cluster characteristics
  cluster_summary <- results %>%
    group_by(cluster) %>%
    summarise(
      mean_value = mean(unit_value),
      mean_ave = mean(ave),
      median_value = median(unit_value),
      median_ave = median(ave),
      n = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      # Create cluster scores based on medians (more robust)
      value_rank = percent_rank(-median_value),  # Negative because lower is better
      ave_rank = percent_rank(median_ave),
      # Raw cluster score
      cluster_score_raw = value_rank * ave_rank,
      # Normalize cluster scores to ensure 0-1 range
      cluster_score = (cluster_score_raw - min(cluster_score_raw)) / 
                      (max(cluster_score_raw) - min(cluster_score_raw))
    ) %>%
    arrange(desc(cluster_score))
  
  # Assign normalized cluster scores to products
  results <- results %>%
    left_join(
      cluster_summary %>% select(cluster, cluster_score),
      by = "cluster"
    ) %>%
    rename(score_cluster = cluster_score)
  
  # ==============================================================
  # STANDARDIZATION CHECK - Ensure all scores are 0-1
  # ==============================================================
  
  # Verify and enforce 0-1 range for all scores
  score_columns <- c("score_percentile", "score_ratio", "score_outlier", 
                     "score_residual", "score_cluster")
  
  for (col in score_columns) {
    results[[col]] <- pmax(0, pmin(1, results[[col]]))  # Clamp to [0,1]
  }
  
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
  
  # Normalize weights to sum to 1
  method_weights <- method_weights / sum(method_weights)
  
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
      
      # Calculate agreement between methods (1 - coefficient of variation)
      method_agreement = apply(
        cbind(score_percentile, score_ratio, score_outlier, 
              score_residual, score_cluster),
        1, 
        function(x) {
          if (mean(x, na.rm = TRUE) == 0) return(0)
          1 - (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))
        }
      ),
      
      # Ensure agreement is in [0,1]
      method_agreement = pmax(0, pmin(1, method_agreement)),
      
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
      
      # Create priority categories with refined thresholds
      priority_category = case_when(
        consensus_score >= 0.8 & methods_flagging >= 4 ~ "Very High Priority",
        consensus_score >= 0.7 & methods_flagging >= 3 ~ "High Priority",
        consensus_score >= 0.6 & methods_flagging >= 2 ~ "Medium Priority",
        consensus_score >= 0.5 ~ "Low Priority",
        TRUE ~ "Not Priority"
      ),
      
      # Confidence in classification based on agreement
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
    
    thresholds_used = list(
      value_percentile_threshold = target_value_pct,
      ave_percentile_threshold = target_ave_pct,
      n_clusters = n_clusters
    ),
    
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
        avg_consensus_score = mean(consensus_score),
        .groups = 'drop'
      ),
    
    method_correlations = cor(
      results %>% 
        select(score_percentile, score_ratio, score_outlier, 
               score_residual, score_cluster),
      use = "complete.obs"
    ),
    
    method_summary = results %>%
      summarise(
        across(all_of(score_columns), 
               list(mean = ~mean(., na.rm = TRUE),
                    sd = ~sd(., na.rm = TRUE),
                    min = ~min(., na.rm = TRUE),
                    max = ~max(., na.rm = TRUE)),
               .names = "{.col}_{.fn}")
      ),
    
    top_consensus_products = results %>%
      arrange(desc(consensus_score)) %>%
      head(20) %>%
      select(product_id, unit_value, ave, consensus_score, 
             priority_category, confidence, methods_flagging,
             score_percentile, score_ratio, score_outlier,
             score_residual, score_cluster),
    
    regression_summary = summary(regression_model),
    
    ave_transformation_used = use_ave_transform
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
      subtitle = paste("Thresholds: Value <", round(target_value_pct*100, 1), 
                      "th percentile, AVE >", round(target_ave_pct*100, 1), "th percentile"),
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
    geom_violin(fill = "lightblue", alpha = 0.7) +
    geom_boxplot(width = 0.1, outlier.shape = NA) +
    labs(
      title = "Score Distribution by Method",
      subtitle = "All methods standardized to 0-1 range",
      x = "Method",
      y = "Score"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Plot 3: Priority distribution with confidence
  p3 <- ggplot(results, aes(x = priority_category, fill = confidence)) +
    geom_bar(position = "stack") +
    labs(
      title = "Priority Categories with Confidence Levels",
      x = "Priority Category",
      y = "Count",
      fill = "Confidence"
    ) +
    scale_fill_manual(values = c("High Confidence" = "#2E7D32",
                                "Medium Confidence" = "#FFA726", 
                                "Low Confidence" = "#EF5350")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Plot 4: Method agreement heatmap for top products
  top_products_scores <- results %>%
    arrange(desc(consensus_score)) %>%
    head(30) %>%
    select(product_id, score_percentile, score_ratio, score_outlier,
           score_residual, score_cluster) %>%
    pivot_longer(cols = starts_with("score_"),
                names_to = "method",
                values_to = "score") %>%
    mutate(method = str_remove(method, "score_"))
  
  p4 <- ggplot(top_products_scores, aes(x = method, y = product_id, fill = score)) +
    geom_tile() +
    scale_fill_gradient2(low = "white", mid = "yellow", high = "red", midpoint = 0.5) +
    labs(
      title = "Method Scores for Top 30 Products",
      x = "Method",
      y = "Product",
      fill = "Score"
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 6),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Return comprehensive results
  return(list(
    data = results,
    summary = summary_stats,
    plots = list(
      scatter = p1,
      methods = p2,
      priorities = p3,
      heatmap = p4
    ),
    model = regression_model,
    cluster_summary = cluster_summary,
    cluster_optimization = if(optimize_clusters) cluster_optimization else NULL
  ))
}

# ==============================================================
# EXAMPLE USAGE WITH IMPROVEMENTS
# ==============================================================

# Generate sample data for demonstration
set.seed(42)
sample_data <- data.frame(
  product_name = paste("Product", 1:500),
  unit_value = exp(rnorm(500, log(50), 1.5)),
  ave = pmax(0, 15 + rnorm(500, 0, 10) - log(exp(rnorm(500, log(50), 1.5))) * 2 + 
             ifelse(runif(500) > 0.9, runif(500, 20, 40), 0))
)

# Run consensus analysis with improved features
consensus_results <- create_consensus_analysis(
  data = sample_data,
  unit_value_col = "unit_value",
  ave_col = "ave",
  id_col = "product_name",
  optimize_clusters = TRUE,  # Auto-optimize cluster count
  max_clusters = 8  # Maximum clusters to consider
)

# Access results
top_products <- consensus_results$data %>%
  filter(priority_category %in% c("Very High Priority", "High Priority")) %>%
  arrange(desc(consensus_score))

# Print analysis summary
cat("CONSENSUS ANALYSIS SUMMARY\n")
cat("=" , strrep("=", 50), "\n")
cat("Total products analyzed:", consensus_results$summary$total_products, "\n")
cat("Optimal clusters found:", consensus_results$summary$thresholds_used$n_clusters, "\n")
cat("Value threshold (percentile):", round(consensus_results$summary$thresholds_used$value_percentile_threshold * 100, 1), "%\n")
cat("AVE threshold (percentile):", round(consensus_results$summary$thresholds_used$ave_percentile_threshold * 100, 1), "%\n")
cat("\nPriority Breakdown:\n")
print(consensus_results$summary$priority_breakdown)

cat("\nMethod Score Ranges (confirming 0-1 standardization):\n")
print(consensus_results$summary$method_summary %>% 
      select(ends_with("_min"), ends_with("_max")) %>%
      round(3))

cat("\nTop 10 Priority Products:\n")
print(top_products %>% 
      head(10) %>%
      select(product_id, unit_value, ave, consensus_score, confidence, methods_flagging) %>%
      mutate(across(where(is.numeric), ~round(., 3))))

# Plot all visualizations
grid.arrange(
  consensus_results$plots$scatter,
  consensus_results$plots$methods,
  consensus_results$plots$priorities,
  consensus_results$plots$heatmap,
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
    methods_flagging, method_agreement,
    score_percentile, score_ratio, score_outlier, 
    score_residual, score_cluster
  ) %>%
  arrange(desc(consensus_score))

write.csv(
  high_priority_report,
  "high_priority_products_detailed.csv",
  row.names = FALSE
)
