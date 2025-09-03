library(dplyr)
library(glue)
library(readr)

process_file_chunked <- function(year) {
  file_path <- glue("file.csv")
  cat("Processing", basename(file_path), "...\n")
  
  # Create temp file for UTF-8 version
  temp_file <- tempfile(fileext = ".csv")
  
  tryCatch({
    # Method 1: Using base R file connections
    # Read the UTF-16LE file
    con_in <- file(file_path, open = "r", encoding = "UTF-16LE")
    lines <- readLines(con_in, warn = FALSE)
    close(con_in)
    
    # Write as UTF-8
    writeLines(lines, temp_file, useBytes = FALSE)
    
    cat("Converted to UTF-8, now processing in chunks...\n")
    
    results <- list()
    
    # Now process the UTF-8 file with chunks - no encoding issues!
    read_tsv_chunked(
      temp_file,
      chunk_size = 20000,
      col_types = cols_only(
        var1 = col_double(),
        var2 = col_character(),
        var3 = col_double()
      ),
      callback = DataFrameCallback$new(function(chunk, pos) {
        cat("  Chunk", pos, "- rows:", nrow(chunk), "\n")
        
        if(nrow(chunk) > 0) {
          processed <- chunk %>%
            filter(!is.na(var1) & var1 == 1) %>%
            mutate(var4 = var3 + 1) %>%
            dplyr::select(var1, var4)
          
          if(nrow(processed) > 0) {
            results <<- append(results, list(processed))
          }
        }
        
        gc()
      }),
      show_col_types = FALSE
    )
    
    if(length(results) > 0) {
      final_result <- do.call(rbind, results)
      cat("Year", year, "final result:", nrow(final_result), "rows\n")
      return(final_result)
    } else {
      return(data.frame(var1 = numeric(0), var4 = numeric(0)))
    }
    
  }, error = function(e) {
    cat("Error:", e$message, "\n")
    return(data.frame(var1 = numeric(0), var4 = numeric(0)))
  }, finally = {
    # Clean up temp file
    if(file.exists(temp_file)) unlink(temp_file)
  })
}
