# ============================================================================
# BILATERAL PRODUCT COMPLEXITY INDEX (HIDALGO-HAUSMANN)
# Lean R Implementation with Import/Export Statistics
# ============================================================================

library(tidyverse)
library(Matrix)

# ============================================================================
# CORE FUNCTIONS
# ============================================================================

# Calculate RCA (Revealed Comparative Advantage)
calculate_rca <- function(trade_matrix) {
  country_totals <- rowSums(trade_matrix)
  product_totals <- colSums(trade_matrix)
  world_total <- sum(trade_matrix)
  
  # Avoid division by zero
  country_totals[country_totals == 0] <- 1
  product_totals[product_totals == 0] <- 1
  
  # Calculate RCA
  rca <- sweep(trade_matrix, 1, country_totals, "/")
  rca <- sweep(rca, 2, product_totals/world_total, "/")
  
  return(rca)
}

# Hidalgo-Hausmann Method of Reflections
method_of_reflections <- function(trade_matrix, iterations = 20, threshold = 1) {
  
  # Calculate RCA
  rca_matrix <- calculate_rca(trade_matrix)
  
  # Create binary matrix (Mcp = 1 if RCA >= threshold)
  M <- ifelse(rca_matrix >= threshold, 1, 0)
  
  # Initialize
  kc <- rowSums(M)  # Diversity
  kp <- colSums(M)  # Ubiquity
  
  # Avoid division by zero
  kc[kc == 0] <- 1
  kp[kp == 0] <- 1
  
  # Store initial values
  kc0 <- kc
  kp0 <- kp
  
  # Iterative calculation
  for(i in 1:iterations) {
    if(i %% 2 == 1) {
      # Odd iteration: update product complexity
      kp_new <- colSums(M * (kc / kc0))
      kp_new <- kp_new / kp0
      kp <- kp_new
    } else {
      # Even iteration: update country complexity
      kc_new <- rowSums(M * matrix(kp / kp0, nrow = nrow(M), 
                                   ncol = ncol(M), byrow = TRUE))
      kc_new <- kc_new / kc0
      kc <- kc_new
    }
  }
  
  # Normalize to z-scores
  pci <- scale(kp)[,1]
  eci <- scale(kc)[,1]
  
  return(list(
    PCI = pci,
    ECI = eci,
    RCA = rca_matrix,
    M = M,
    diversity = kc0,
    ubiquity = kp0
  ))
}

# ============================================================================
# MAIN WORKFLOW
# ============================================================================

calculate_bilateral_complexity <- function(trade_data, 
                                         subset_countries,
                                         year_val,
                                         iterations = 20,
                                         rca_threshold = 1) {
  
  # Filter for subset and year
  subset_data <- trade_data %>%
    filter(year == year_val) %>%
    filter(exporter %in% subset_countries | importer %in% subset_countries)
  
  # Create EXPORT matrix (what each country exports)
  export_matrix <- subset_data %>%
    filter(exporter %in% subset_countries) %>%
    group_by(exporter, commodity) %>%
    summarise(
      total_exports = sum(value),
      avg_export_elasticity = weighted.mean(export_elasticity, value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      id_cols = exporter,
      names_from = commodity,
      values_from = total_exports,
      values_fill = 0
    ) %>%
    column_to_rownames("exporter") %>%
    as.matrix()
  
  # Create IMPORT matrix (what each country imports)
  import_matrix <- subset_data %>%
    filter(importer %in% subset_countries) %>%
    group_by(importer, commodity) %>%
    summarise(
      total_imports = sum(value),
      avg_import_elasticity = weighted.mean(import_elasticity, value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      id_cols = importer,
      names_from = commodity,
      values_from = total_imports,
      values_fill = 0
    ) %>%
    column_to_rownames("importer") %>%
    as.matrix()
  
  # Ensure same products in both matrices
  common_products <- intersect(colnames(export_matrix), colnames(import_matrix))
  export_matrix <- export_matrix[, common_products]
  import_matrix <- import_matrix[, common_products]
  
  # Calculate complexity for EXPORTS
  export_complexity <- method_of_reflections(export_matrix, iterations, rca_threshold)
  
  # Calculate complexity for IMPORTS (import complexity)
  import_complexity <- method_of_reflections(import_matrix, iterations, rca_threshold)
  
  # Compile product-level results
  product_complexity <- data.frame(
    product = common_products,
    # Export-based complexity
    pci_export = export_complexity$PCI,
    ubiquity_export = export_complexity$ubiquity,
    # Import-based complexity  
    pci_import = import_complexity$PCI,
    ubiquity_import = import_complexity$ubiquity,
    # Combined complexity (average)
    pci_combined = (export_complexity$PCI + import_complexity$PCI) / 2,
    stringsAsFactors = FALSE
  )
  
  # Add trade statistics
  trade_stats <- subset_data %>%
    filter(commodity %in% common_products) %>%
    group_by(commodity) %>%
    summarise(
      # Export statistics
      total_exports = sum(value[exporter %in% subset_countries]),
      n_exporters = n_distinct(exporter[exporter %in% subset_countries & value > 0]),
      avg_export_value = mean(value[exporter %in% subset_countries & value > 0]),
      # Import statistics
      total_imports = sum(value[importer %in% subset_countries]),
      n_importers = n_distinct(importer[importer %in% subset_countries & value > 0]),
      avg_import_value = mean(value[importer %in% subset_countries & value > 0]),
      # Trade balance
      trade_balance = total_exports - total_imports,
      # Elasticities
      avg_export_elasticity = weighted.mean(export_elasticity[exporter %in% subset_countries], 
                                           value[exporter %in% subset_countries], na.rm = TRUE),
      avg_import_elasticity = weighted.mean(import_elasticity[importer %in% subset_countries], 
                                           value[importer %in% subset_countries], na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    rename(product = commodity)
  
  product_complexity <- product_complexity %>%
    left_join(trade_stats, by = "product")
  
  # Compile country-level results
  # Get export countries
  export_countries <- data.frame(
    country = rownames(export_matrix),
    eci_export = export_complexity$ECI,
    diversity_export = export_complexity$diversity,
    total_exports = rowSums(export_matrix),
    n_products_exported = rowSums(export_matrix > 0),
    stringsAsFactors = FALSE
  )
  
  # Get import countries  
  import_countries <- data.frame(
    country = rownames(import_matrix),
    eci_import = import_complexity$ECI,
    diversity_import = import_complexity$diversity,
    total_imports = rowSums(import_matrix),
    n_products_imported = rowSums(import_matrix > 0),
    stringsAsFactors = FALSE
  )
  
  # Merge country results
  country_complexity <- export_countries %>%
    full_join(import_countries, by = "country") %>%
    mutate(
      eci_combined = case_when(
        !is.na(eci_export) & !is.na(eci_import) ~ (eci_export + eci_import) / 2,
        !is.na(eci_export) ~ eci_export,
        !is.na(eci_import) ~ eci_import,
        TRUE ~ NA_real_
      ),
      trade_balance = replace_na(total_exports, 0) - replace_na(total_imports, 0)
    )
  
  return(list(
    product_complexity = product_complexity,
    country_complexity = country_complexity,
    export_matrix = export_matrix,
    import_matrix = import_matrix,
    export_rca = export_complexity$RCA,
    import_rca = import_complexity$RCA
  ))
}

# ============================================================================
# COMPARISON ACROSS SUBSETS
# ============================================================================

compare_subset_complexity <- function(trade_data, 
                                     subset_list,
                                     year_val,
                                     iterations = 20) {
  
  all_results <- list()
  
  for(subset_name in names(subset_list)) {
    countries <- subset_list[[subset_name]]
    
    result <- calculate_bilateral_complexity(
      trade_data = trade_data,
      subset_countries = countries,
      year_val = year_val,
      iterations = iterations
    )
    
    all_results[[subset_name]] <- result$product_complexity %>%
      select(product, pci_export, pci_import, pci_combined) %>%
      rename_with(~paste0(., "_", subset_name), -product)
  }
  
  # Merge all results
  comparison <- reduce(all_results, left_join, by = "product")
  
  return(comparison)
}

# ============================================================================
# TEMPORAL ANALYSIS
# ============================================================================

calculate_temporal_complexity <- function(trade_data, 
                                        subset_countries,
                                        years = NULL,
                                        iterations = 20) {
  
  if(is.null(years)) {
    years <- sort(unique(trade_data$year))
  }
  
  temporal_results <- list()
  
  for(year in years) {
    result <- calculate_bilateral_complexity(
      trade_data = trade_data,
      subset_countries = subset_countries,
      year_val = year,
      iterations = iterations
    )
    
    temporal_results[[as.character(year)]] <- result$product_complexity %>%
      select(product, pci_export, pci_import, pci_combined) %>%
      mutate(year = year)
  }
  
  # Combine all years
  all_years <- bind_rows(temporal_results)
  
  # Calculate year-over-year changes
  complexity_changes <- all_years %>%
    arrange(product, year) %>%
    group_by(product) %>%
    mutate(
      pci_export_change = pci_export - lag(pci_export),
      pci_import_change = pci_import - lag(pci_import),
      pci_combined_change = pci_combined - lag(pci_combined)
    ) %>%
    ungroup()
  
  return(list(
    temporal_complexity = all_years,
    complexity_changes = complexity_changes,
    summary_stats = all_years %>%
      group_by(year) %>%
      summarise(
        mean_export_complexity = mean(pci_export, na.rm = TRUE),
        mean_import_complexity = mean(pci_import, na.rm = TRUE),
        mean_combined_complexity = mean(pci_combined, na.rm = TRUE),
        sd_export_complexity = sd(pci_export, na.rm = TRUE),
        sd_import_complexity = sd(pci_import, na.rm = TRUE),
        n_products = n(),
        .groups = 'drop'
      )
  ))
}

# ============================================================================
# ELASTICITY-ADJUSTED COMPLEXITY
# ============================================================================

calculate_elasticity_adjusted_complexity <- function(trade_data,
                                                    subset_countries,
                                                    year_val,
                                                    iterations = 20) {
  
  # Get base complexity
  base_results <- calculate_bilateral_complexity(
    trade_data = trade_data,
    subset_countries = subset_countries,
    year_val = year_val,
    iterations = iterations
  )
  
  # Adjust PCI by elasticities
  # Logic: Higher export elasticity = more substitutable = less complex
  #        Higher import elasticity (absolute) = more essential = more complex
  
  adjusted_complexity <- base_results$product_complexity %>%
    mutate(
      # Handle missing elasticities
      avg_export_elasticity = replace_na(avg_export_elasticity, 1),
      avg_import_elasticity = replace_na(avg_import_elasticity, -1),
      # Export adjustment: divide by elasticity (higher elasticity = lower complexity)
      pci_export_adjusted = pci_export / sqrt(abs(avg_export_elasticity)),
      # Import adjustment: multiply by absolute elasticity (higher = more essential)
      pci_import_adjusted = pci_import * sqrt(abs(avg_import_elasticity))
    ) %>%
    mutate(
      # Renormalize
      pci_export_adjusted = scale(pci_export_adjusted)[,1],
      pci_import_adjusted = scale(pci_import_adjusted)[,1],
      # Combined adjusted
      pci_combined_adjusted = (pci_export_adjusted + pci_import_adjusted) / 2
    )
  
  return(adjusted_complexity)
}

# ============================================================================
# EXAMPLE USAGE
# ============================================================================

# Generate sample data for testing
generate_sample_data <- function(n_countries = 15, n_products = 50, n_years = 3) {
  set.seed(123)
  
  countries <- paste0("Country_", LETTERS[1:n_countries])
  products <- paste0("HS_", sprintf("%04d", 1:n_products))
  years <- 2021:2023
  
  # Create sparse trade matrix
  data <- expand.grid(
    importer = countries,
    exporter = countries,
    commodity = sample(products, 30),
    year = years,
    stringsAsFactors = FALSE
  ) %>%
    filter(importer != exporter) %>%
    mutate(
      value = ifelse(runif(n()) > 0.3, 
                    abs(rnorm(n(), mean = 1000000, sd = 500000)), 
                    0),
      quantity = ifelse(value > 0, 
                       abs(rnorm(n(), mean = 10000, sd = 5000)), 
                       0),
      import_share = runif(n(), 0, 0.1),
      export_share = runif(n(), 0, 0.1),
      import_elasticity = runif(n(), -2, -0.5),
      export_elasticity = runif(n(), 0.5, 2)
    )
  
  return(data)
}

# Run example
print("Generating sample data...")
trade_data <- generate_sample_data()

# Define subsets
subsets <- list(
  "Region_A" = paste0("Country_", LETTERS[1:8]),
  "Region_B" = paste0("Country_", LETTERS[9:15])
)

# Calculate complexity for Region A
print("\nCalculating bilateral complexity for Region A...")
results <- calculate_bilateral_complexity(
  trade_data = trade_data,
  subset_countries = subsets$Region_A,
  year_val = 2023,
  iterations = 20
)

# Display top complex products
print("\nTop 10 Most Complex Products (Export-based):")
results$product_complexity %>%
  arrange(desc(pci_export)) %>%
  select(product, pci_export, pci_import, pci_combined, 
         total_exports, total_imports, n_exporters, n_importers) %>%
  head(10) %>%
  print()

print("\nTop 10 Most Complex Products (Import-based):")
results$product_complexity %>%
  arrange(desc(pci_import)) %>%
  select(product, pci_export, pci_import, pci_combined, 
         total_exports, total_imports, n_exporters, n_importers) %>%
  head(10) %>%
  print()

# Country complexity
print("\nCountry Complexity Rankings:")
results$country_complexity %>%
  arrange(desc(eci_combined)) %>%
  select(country, eci_export, eci_import, eci_combined, 
         n_products_exported, n_products_imported, trade_balance) %>%
  print()

# Compare regions
print("\nComparing complexity across regions...")
comparison <- compare_subset_complexity(
  trade_data = trade_data,
  subset_list = subsets,
  year_val = 2023
)

# Products with largest complexity differences
comparison %>%
  mutate(diff = abs(pci_combined_Region_A - pci_combined_Region_B)) %>%
  arrange(desc(diff)) %>%
  head(10) %>%
  print()

# Temporal analysis
print("\nCalculating temporal complexity evolution...")
temporal <- calculate_temporal_complexity(
  trade_data = trade_data,
  subset_countries = subsets$Region_A,
  years = 2021:2023
)

print("\nComplexity trends over time:")
print(temporal$summary_stats)

# Elasticity adjustment
print("\nCalculating elasticity-adjusted complexity...")
adjusted <- calculate_elasticity_adjusted_complexity(
  trade_data = trade_data,
  subset_countries = subsets$Region_A,
  year_val = 2023
)

print("\nComparison: Base vs Elasticity-Adjusted Complexity (top products):")
adjusted %>%
  select(product, pci_export, pci_export_adjusted, 
         pci_import, pci_import_adjusted, avg_export_elasticity, avg_import_elasticity) %>%
  arrange(desc(pci_combined_adjusted)) %>%
  head(10) %>%
  print()

print("\n=== Analysis Complete ===")
