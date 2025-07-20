# Unit conversion factors to tons (only convertible units)
unit_conversion_factors <- data.frame(
  unit = c("KG", "T", "GM", "CKG", "CGM", "DMT"),
  factor_to_tons = c(
    0.001,      # KG (kilograms) - 1 kg = 0.001 tons
    1,          # T (tons) - already in tons
    0.000001,   # GM (grams) - 1 gram = 0.000001 tons
    0.1,        # CKG (hundred kilograms) - 100 kg = 0.1 tons
    0.00000001, # CGM (centigrams) - 1 cg = 0.00000001 tons
    1           # DMT (dry metric tons) - 1 DMT = 1 ton
  ),
  description = c(
    "Kilograms", "Tons", "Grams", "Hundred kilograms", "Centigrams", "Dry metric tons"
  ),
  stringsAsFactors = FALSE
)

# Vectorized function to convert units to tons (pipe-friendly)
convert_to_tons <- function(value, unit) {
  # Convert unit to uppercase for matching
  unit <- toupper(unit)
  
  # Use match() for vectorized lookup
  factor_idx <- match(unit, unit_conversion_factors$unit)
  
  # Get conversion factors (NA for non-matching units)
  factors <- unit_conversion_factors$factor_to_tons[factor_idx]
  
  # Return converted values
  return(value * factors)
}

# Alternative: Create a named vector for even faster lookup
conversion_lookup <- setNames(
  unit_conversion_factors$factor_to_tons, 
  unit_conversion_factors$unit
)

convert_to_tons_fast <- function(value, unit) {
  conversion_lookup[toupper(unit)] * value
}

# Pipe-friendly function that uses standard evaluation
add_tons_column <- function(df, value_col, unit_col, tons_col = "tons", status_col = "conversion_status") {
  # Use standard evaluation to access columns
  df[[tons_col]] <- convert_to_tons(df[[value_col]], df[[unit_col]])
  df[[status_col]] <- ifelse(is.na(df[[tons_col]]), "Not convertible", "Converted")
  return(df)
}

# Example usage with dplyr:
library(dplyr)

# Sample data
sample_data <- data.frame(
  quantity = c(1000, 500, 2, 100, 50000, 75),
  unit = c("KG", "GM", "T", "CKG", "CGM", "UNKNOWN")
)

# Method 1: Using mutate() with the vectorized function (recommended)
result1 <- sample_data %>%
  mutate(
    tons = convert_to_tons(quantity, unit),
    conversion_status = ifelse(is.na(tons), "Not convertible", "Converted")
  )

# Method 2: Using the pipe-friendly wrapper function
result2 <- sample_data %>%
  add_tons_column("quantity", "unit")

# Method 3: Using the fast lookup version
result3 <- sample_data %>%
  mutate(
    tons = convert_to_tons_fast(quantity, unit),
    conversion_status = case_when(
      is.na(tons) ~ "Not convertible",
      TRUE ~ "Converted"
    )
  )

print("Method 1 result:")
print(result1)

print("\nMethod 2 result:")
print(result2)

print("\nMethod 3 result:")
print(result3)

print("\nConvertible units:")
print(unit_conversion_factors)
