df_expanded <- df %>%
  mutate(
    # Only process rows with dashes
    value_expanded = map(value, function(v) {
      # Check if it's a range
      if (!str_detect(v, "-")) {
        return(v)  # Return as-is if not a range
      }
      
      # Try to parse the range
      tryCatch({
        parts <- str_split(v, "-")[[1]]
        start_num <- as.numeric(parts[1])
        end_digit <- as.numeric(parts[2])
        
        # Validate numbers
        if (is.na(start_num) || is.na(end_digit)) {
          return(v)  # Return original if can't parse
        }
        
        end_num <- (start_num %/% 10) * 10 + end_digit
        
        # Additional validation
        if (!is.finite(end_num) || end_num < start_num) {
          return(v)  # Return original if invalid range
        }
        
        return(as.character(seq(start_num, end_num)))
      }, error = function(e) {
        return(v)  # On any error, return original value
      })
    })
  ) %>%
  unnest(value_expanded) %>%
  mutate(
    # Convert back to numeric if possible, keep as character otherwise
    value = suppressWarnings(as.numeric(value_expanded))
  ) %>%
  select(name, age, value)

print(df_expanded)
