library(vroom)

process_file_chunked <- function(year) {
  file_path <- glue("file.csv")
  cat("Processing", basename(file_path), "...\n")
  
  tryCatch({
    # vroom can handle UTF-16LE better
    df <- vroom(
      file_path,
      locale = locale(encoding = "UTF-16LE"),
      col_select = c(var1, var2, var3),
      col_types = cols(
        var1 = col_double(),
        var2 = col_character(),
        var3 = col_double()
      ),
      show_col_types = FALSE,
      altrep = FALSE  # Disable altrep to read everything immediately
    )
    
    result <- df %>%
      filter(!is.na(var1) & var1 == 1) %>%
      mutate(var4 = var3 + 1) %>%
      dplyr::select(var1, var4)
    
    cat("Year", year, "final result:", nrow(result), "rows\n")
    return(result)
    
  }, error = function(e) {
    cat("Error:", e$message, "\n")
    return(data.frame(var1 = numeric(0), var4 = numeric(0)))
  })
}
