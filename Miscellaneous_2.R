# Full Iterative Bilateral Complexity Index
library(dplyr)
library(tidyr)
library(ggplot2)

# Main function for full iterative bilateral complexity
calculate_bilateral_complexity_full <- function(trade_data, max_iterations = 50, tolerance = 1e-8, verbose = TRUE) {
  
  # Data validation
  required_cols <- c("importer", "exporter", "commodity", "import_share", "export_share", 
                     "import_elasticity", "export_elasticity")
  
  missing_cols <- setdiff(required_cols, names(trade_data))
  if(length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  if(verbose) cat("Starting bilateral complexity calculation...\n")
  if(verbose) cat("Data dimensions:", nrow(trade_data), "trade flows\n")
  
  # Prepare the data
  trade_clean <- trade_data %>%
    filter(!is.na(import_share), !is.na(export_share),
           !is.na(import_elasticity), !is.na(export_elasticity)) %>%
    mutate(
      # Combined share weight (bilateral importance within commodity)
      share_weight = (import_share + export_share) / 2,
      # Elasticity factor - lower elasticity suggests higher complexity
      elasticity_factor = pmax(0.1, 1 / (1 + abs((import_elasticity + export_elasticity) / 2))),
      # Base weight before bilateral sophistication
      base_weight = share_weight * elasticity_factor
    )
  
  if(verbose) cat("Clean data:", nrow(trade_clean), "flows after removing NAs\n")
  
  # Initialize complexity scores
  countries <- unique(c(trade_clean$exporter, trade_clean$importer))
  commodities <- unique(trade_clean$commodity)
  
  if(verbose) cat("Countries:", length(countries), "| Commodities:", length(commodities), "\n")
  
  # Initialize with random small values to break symmetry
  set.seed(42)
  country_complexity <- setNames(rnorm(length(countries), 0, 0.1), countries)
  commodity_complexity <- setNames(rnorm(length(commodities), 0, 0.1), commodities)
  
  # Track convergence
  convergence_history <- data.frame()
  
  # Iterative calculation
  for(iter in 1:max_iterations) {
    
    # Store previous values for convergence check
    prev_country <- country_complexity
    prev_commodity <- commodity_complexity
    
    # ====== UPDATE COMMODITY COMPLEXITY ======
    # Commodities get complexity from the sophistication of bilateral relationships trading them
    
    commodity_updates <- trade_clean %>%
      mutate(
        # Current bilateral sophistication
        exporter_soph = country_complexity[exporter],
        importer_soph = country_complexity[importer],
        bilateral_sophistication = (exporter_soph + importer_soph) / 2,
        # Weight this trade flow's contribution to commodity complexity
        flow_weight = base_weight * bilateral_sophistication
      ) %>%
      group_by(commodity) %>%
      summarise(
        new_complexity = sum(flow_weight, na.rm = TRUE),
        n_relationships = n(),
        avg_bilateral_soph = mean(bilateral_sophistication, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      # Normalize by number of relationships to avoid bias toward high-volume commodities
      mutate(new_complexity = new_complexity / sqrt(n_relationships))
    
    # ====== UPDATE COUNTRY COMPLEXITY ======
    
    # Countries get complexity from two sources: as exporters and as importers
    
    # 1. Export complexity: weighted by how important the importer market is (import_share)
    #    and the complexity of commodities being exported
    export_complexity <- trade_clean %>%
      mutate(
        commodity_weight = commodity_complexity[commodity],
        importer_soph = country_complexity[importer],
        # Weight = how much importer depends on this trade * commodity complexity * importer sophistication
        flow_contribution = import_share * commodity_weight * importer_soph
      ) %>%
      group_by(exporter) %>%
      summarise(
        export_complexity = sum(flow_contribution, na.rm = TRUE),
        n_export_flows = n(),
        .groups = 'drop'
      ) %>%
      mutate(export_complexity = export_complexity / sqrt(n_export_flows))
    
    # 2. Import complexity: weighted by how important the exporter source is (export_share)
    #    and the complexity of commodities being imported
    import_complexity <- trade_clean %>%
      mutate(
        commodity_weight = commodity_complexity[commodity],
        exporter_soph = country_complexity[exporter],
        # Weight = how much exporter depends on this trade * commodity complexity * exporter sophistication
        flow_contribution = export_share * commodity_weight * exporter_soph
      ) %>%
      group_by(importer) %>%
      summarise(
        import_complexity = sum(flow_contribution, na.rm = TRUE),
        n_import_flows = n(),
        .groups = 'drop'
      ) %>%
      mutate(import_complexity = import_complexity / sqrt(n_import_flows))
    
    # Combine export and import complexity for each country
    all_countries_df <- data.frame(country = countries)
    
    country_updates <- all_countries_df %>%
      left_join(export_complexity, by = c("country" = "exporter")) %>%
      left_join(import_complexity, by = c("country" = "importer")) %>%
      mutate(
        export_complexity = replace_na(export_complexity, 0),
        import_complexity = replace_na(import_complexity, 0),
        # Combine export and import sophistication
        total_complexity = (export_complexity + import_complexity) / 2
      )
    
    # ====== UPDATE COMPLEXITY VECTORS ======
    commodity_complexity <- setNames(commodity_updates$new_complexity, commodity_updates$commodity)
    country_complexity <- setNames(country_updates$total_complexity, country_updates$country)
    
    # ====== NORMALIZATION ======
    # Standardize to prevent explosion/implosion and maintain interpretability
    commodity_complexity <- scale(commodity_complexity)[,1]
    country_complexity <- scale(country_complexity)[,1]
    
    # ====== CONVERGENCE CHECK ======
    country_change <- sqrt(mean((country_complexity - prev_country)^2, na.rm = TRUE))
    commodity_change <- sqrt(mean((commodity_complexity - prev_commodity)^2, na.rm = TRUE))
    
    # Handle potential NA values in convergence check
    if(is.na(country_change)) country_change <- Inf
    if(is.na(commodity_change)) commodity_change <- Inf
    
    # Store convergence metrics
    convergence_history <- bind_rows(
      convergence_history,
      data.frame(
        iteration = iter,
        country_change = country_change,
        commodity_change = commodity_change,
        total_change = country_change + commodity_change
      )
    )
    
    if(verbose && iter %% 5 == 0) {
      cat(sprintf("Iteration %d: Country change = %.6f, Commodity change = %.6f\n", 
                  iter, country_change, commodity_change))
    }
    
    # Check for convergence with NA protection
    if(!is.na(country_change) && !is.na(commodity_change) && 
       country_change < tolerance && commodity_change < tolerance) {
      if(verbose) cat(sprintf("\nConverged after %d iterations!\n", iter))
      break
    }
    
    # Debug output if we hit NA values
    if(is.infinite(country_change) || is.infinite(commodity_change)) {
      if(verbose) cat(sprintf("Warning: NA values detected at iteration %d\n", iter))
    }
  }
  
  if(iter == max_iterations) {
    warning(sprintf("Did not converge after %d iterations. Final changes: Country = %.6f, Commodity = %.6f", 
                    max_iterations, country_change, commodity_change))
  }
  
  # ====== PREPARE FINAL RESULTS ======
  
  # Final standardization for interpretability
  commodity_complexity_final <- scale(commodity_complexity)[,1]
  country_complexity_final <- scale(country_complexity)[,1]
  
  results <- list(
    commodity_complexity = data.frame(
      commodity = names(commodity_complexity_final),
      complexity_index = as.numeric(commodity_complexity_final)
    ) %>% arrange(desc(complexity_index)),
    
    country_complexity = data.frame(
      country = names(country_complexity_final),
      complexity_index = as.numeric(country_complexity_final)
    ) %>% arrange(desc(complexity_index)),
    
    convergence_info = list(
      iterations = iter,
      converged = (country_change < tolerance && commodity_change < tolerance),
      final_country_change = country_change,
      final_commodity_change = commodity_change,
      convergence_history = convergence_history
    )
  )
  
  if(verbose) {
    cat("\n=== RESULTS SUMMARY ===\n")
    cat("Top 5 Most Complex Commodities:\n")
    print(head(results$commodity_complexity, 5))
    cat("\nTop 5 Most Complex Countries:\n")
    print(head(results$country_complexity, 5))
  }
  
  return(results)
}

# Utility function to plot convergence
plot_convergence <- function(results) {
  convergence_data <- results$convergence_info$convergence_history
  
  convergence_long <- convergence_data %>%
    pivot_longer(cols = c(country_change, commodity_change), 
                 names_to = "metric", values_to = "change") %>%
    mutate(metric = case_when(
      metric == "country_change" ~ "Country Complexity Change",
      metric == "commodity_change" ~ "Commodity Complexity Change"
    ))
  
  ggplot(convergence_long, aes(x = iteration, y = change, color = metric)) +
    geom_line(size = 1) +
    scale_y_log10() +
    labs(
      title = "Bilateral Complexity Convergence",
      x = "Iteration",
      y = "Change (log scale)",
      color = "Metric"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Function to analyze results in detail
analyze_complexity_results <- function(trade_data, results) {
  
  # Merge complexity scores back to original data
  enriched_data <- trade_data %>%
    left_join(results$commodity_complexity, by = "commodity") %>%
    rename(commodity_complexity = complexity_index) %>%
    left_join(results$country_complexity, by = c("exporter" = "country")) %>%
    rename(exporter_complexity = complexity_index) %>%
    left_join(results$country_complexity, by = c("importer" = "country")) %>%
    rename(importer_complexity = complexity_index) %>%
    mutate(
      # Calculate bilateral relationship sophistication
      bilateral_sophistication = (exporter_complexity + importer_complexity) / 2,
      # Combined importance score
      relationship_importance = (import_share + export_share) / 2,
      # Overall relationship score
      relationship_score = bilateral_sophistication * relationship_importance * commodity_complexity
    )
  
  # Summary statistics
  summary_stats <- list(
    top_relationships = enriched_data %>% 
      top_n(20, relationship_score) %>%
      select(exporter, importer, commodity, relationship_score, 
             bilateral_sophistication, relationship_importance, commodity_complexity) %>%
      arrange(desc(relationship_score)),
    
    complexity_correlations = cor(enriched_data[c("commodity_complexity", "exporter_complexity", 
                                                   "importer_complexity", "import_share", 
                                                   "export_share", "import_elasticity", 
                                                   "export_elasticity")], 
                                  use = "complete.obs")
  )
  
  return(list(
    enriched_data = enriched_data,
    summary_stats = summary_stats
  ))
}

# Diagnostic function to check your data before running the main algorithm
diagnose_data <- function(trade_data) {
  cat("=== DATA DIAGNOSTICS ===\n")
  cat("Data dimensions:", nrow(trade_data), "rows,", ncol(trade_data), "columns\n")
  
  required_cols <- c("importer", "exporter", "commodity", "import_share", "export_share", 
                     "import_elasticity", "export_elasticity")
  
  # Check column names
  cat("\nColumn check:\n")
  for(col in required_cols) {
    if(col %in% names(trade_data)) {
      cat("✓", col, "- found\n")
    } else {
      cat("✗", col, "- MISSING\n")
    }
  }
  
  # Check for NAs
  cat("\nNA counts:\n")
  na_counts <- trade_data %>% 
    summarise_all(~sum(is.na(.))) %>%
    pivot_longer(everything(), names_to = "column", values_to = "na_count") %>%
    filter(na_count > 0)
  
  if(nrow(na_counts) > 0) {
    print(na_counts)
  } else {
    cat("No NAs found in any column\n")
  }
  
  # Check data ranges
  if(all(required_cols %in% names(trade_data))) {
    cat("\nData ranges:\n")
    numeric_cols <- c("import_share", "export_share", "import_elasticity", "export_elasticity")
    ranges <- trade_data[numeric_cols] %>%
      summarise_all(list(min = ~min(., na.rm = TRUE), 
                        max = ~max(., na.rm = TRUE),
                        mean = ~mean(., na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "stat", values_to = "value") %>%
      separate(stat, into = c("column", "statistic"), sep = "_(?=[^_]+$)") %>%
      pivot_wider(names_from = statistic, values_from = value)
    print(ranges)
    
    # Check for infinite values
    cat("\nInfinite values:\n")
    inf_counts <- trade_data[numeric_cols] %>%
      summarise_all(~sum(is.infinite(.))) %>%
      pivot_longer(everything(), names_to = "column", values_to = "inf_count") %>%
      filter(inf_count > 0)
    
    if(nrow(inf_counts) > 0) {
      print(inf_counts)
    } else {
      cat("No infinite values found\n")
    }
  }
  
  # Check unique counts
  cat("\nUnique counts:\n")
  cat("Countries (exporters):", length(unique(trade_data$exporter)), "\n")
  cat("Countries (importers):", length(unique(trade_data$importer)), "\n")
  cat("All countries:", length(unique(c(trade_data$exporter, trade_data$importer))), "\n")
  cat("Commodities:", length(unique(trade_data$commodity)), "\n")
  
  return(invisible(trade_data))
}
