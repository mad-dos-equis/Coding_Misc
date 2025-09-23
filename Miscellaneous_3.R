df_expanded <- df %>%
  mutate(
    # str_detect returns NA for NA input, so we need to handle this
    is_range = !is.na(value) & str_detect(value, "-"),
    
    value_expanded = case_when(
      # Handle NA values
      is.na(value) ~ list(NA_character_),
      
      # Handle ranges
      is_range ~ map(value, function(v) {
        tryCatch({
          parts <- str_split(v, "-")[[1]]
          start_num <- as.numeric(parts[1])
          end_digit <- as.numeric(parts[2])
          end_num <- (start_num %/% 10) * 10 + end_digit
          
          if (!is.finite(end_num) || end_num < start_num) {
            return(v)
          }
          
          return(as.character(seq(start_num, end_num)))
        }, error = function(e) {
          return(v)
        })
      }),
      
      # Handle non-ranges (including asterisk values)
      TRUE ~ map(value, ~.x)
    )
  ) %>%
  unnest(value_expanded) %>%
  select(name, age, value = value_expanded)

print(df_expanded)
