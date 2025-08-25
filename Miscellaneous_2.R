# ============================================================================
# BILATERAL PRODUCT COMPLEXITY INDEX (HIDALGO-HAUSMANN)
# Lean R Implementation for Pre-filtered Data
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
# MAIN FUNCTION
# ============================================================================

calculate_bilateral_complexity <- function(trade_data, 
                                         year_val = NULL,
                                         iterations = 20,
                                         rca_threshold = 1) {
  
  # Filter by year if specified
  if(!is.null(year_val)) {
    data_to_use <- trade_data %>%
      filter(year == year_val)
  } else {
    data_to_use <- trade_data
  }
  
  # Get all unique countries
  all_exporters <- unique(data_to_use$exporter)
  all_importers <- unique(data_to_use$importer)
  
  # Create EXPORT matrix
  export_matrix <- data_to_use %>%
    group_by(exporter, commodity) %>%
    summarise(
      total_exports = sum(value),
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
  
  # Create IMPORT matrix
  import_matrix <- data_to_use %>%
    group_by(importer, commodity) %>%
    summarise(
      total_imports = sum(value),
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
  
  # Calculate complexity for IMPORTS
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
  trade_stats <- data_to_use %>%
    filter(commodity %in% common_products) %>%
    group_by(commodity) %>%
    summarise(
      # Export statistics
      total_exports = sum(value[exporter %in% all_exporters]),
      n_exporters = n_distinct(exporter[value > 0]),
      avg_export_value = mean(value[value > 0]),
      # Import statistics
      total_imports = sum(value[importer %in% all_importers]),
      n_importers = n_distinct(importer[value > 0]),
      avg_import_value = mean(value[value > 0]),
      # Trade balance
      trade_balance = total_exports - total_imports,
      # Elasticities (if you want to keep them for analysis)
      avg_export_elasticity = weighted.mean(export_elasticity, value, na.rm = TRUE),
      avg_import_elasticity = weighted.mean(import_elasticity, value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    rename(product = commodity)
  
  product_complexity <- product_complexity %>%
    left_join(trade_stats, by = "product")
  
  # Compile country-level results
  export_countries <- data.frame(
    country = rownames(export_matrix),
    eci_export = export_complexity$ECI,
    diversity_export = export_complexity$diversity,
    total_exports = rowSums(export_matrix),
    n_products_exported = rowSums(export_matrix > 0),
    stringsAsFactors = FALSE
  )
  
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
# TEMPORAL ANALYSIS
# ============================================================================

calculate_temporal_complexity <- function(trade_data, 
                                        years = NULL,
                                        iterations = 20) {
  
  if(is.null(years)) {
    years <- sort(unique(trade_data$year))
  }
  
  temporal_results <- list()
  
  for(year in years) {
    result <- calculate_bilateral_complexity(
      trade_data = trade_data,
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
# EXAMPLE USAGE
# ============================================================================

# Example with your pre-filtered data
# Assuming your data is already filtered to the countries/regions you want

# Calculate complexity for a specific year
# results <- calculate_bilateral_complexity(
#   trade_data = your_filtered_data,
#   year_val = 2023,
#   iterations = 20
# )

# Or calculate for all years in your data
# results <- calculate_bilateral_complexity(
#   trade_data = your_filtered_data,
#   year_val = NULL,  # Uses all years
#   iterations = 20
# )

# Access the results
# results$product_complexity  # Product-level complexity scores
# results$country_complexity  # Country-level complexity scores

# Temporal analysis across multiple years
# temporal <- calculate_temporal_complexity(
#   trade_data = your_filtered_data,
#   years = 2019:2023  # Or NULL for all years
# )

# Top complex products (export perspective)
# results$product_complexity %>%
#   arrange(desc(pci_export)) %>%
#   select(product, pci_export, pci_import, pci_combined, 
#          total_exports, total_imports, n_exporters, n_importers) %>%
#   head(20)

# Country rankings
# results$country_complexity %>%
#   arrange(desc(eci_combined)) %>%
#   select(country, eci_export, eci_import, eci_combined, 
#          n_products_exported, n_products_imported, trade_balance)

print("Bilateral Complexity Analysis Functions Loaded")
print("Main function: calculate_bilateral_complexity()")
print("Your data should have columns: importer, exporter, commodity, year, value, quantity, import_share, export_share, import_elasticity, export_elasticity")
