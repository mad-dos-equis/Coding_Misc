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

# Function to convert units to tons (only handles convertible units)
convert_to_tons <- function(value, unit) {
  # Convert unit to uppercase for matching
  unit <- toupper(unit)
  
  # Find conversion factor
  factor_idx <- which(unit_conversion_factors$unit == unit)
  
  if (length(factor_idx) == 0) {
    return(NA)  # Unit not convertible
  }
  
  factor <- unit_conversion_factors$factor_to_tons[factor_idx]
  return(value * factor)
}

# Function to convert a data frame column
convert_trade_data_to_tons <- function(df, value_col, unit_col) {
  # Create new column with tons
  df$tons <- mapply(convert_to_tons, df[[value_col]], df[[unit_col]])
  
  # Add conversion status
  df$conversion_status <- ifelse(is.na(df$tons), "Not convertible", "Converted")
  
  return(df)
}

# Example usage:
# sample_data <- data.frame(
#   quantity = c(1000, 500, 2, 100, 50000),
#   unit = c("KG", "GM", "T", "CKG", "CGM")
# )
# 
# result <- convert_trade_data_to_tons(sample_data, "quantity", "unit")
# print(result)

# Show convertible units
print("Convertible units:")
print(unit_conversion_factors)
