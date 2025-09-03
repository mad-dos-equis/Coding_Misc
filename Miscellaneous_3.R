library(dplyr)
library(glue)
library(readr)

process_file_chunked <- function(year) {
  file_path <- glue("file.csv")
  cat("Processing", basename(file_path), "in chunks...\n")
  
  results <- list()
  
  # Force cleanup
  gc()
  closeAllConnections()
  
  tryCatch({
    read_tsv_chunked(
      file_path,
      locale = locale(encoding = "UTF-16LE"),
      chunk_size = 20000,  # Smaller chunks
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
        
        # Cleanup after each chunk
        gc()
      }),
      show_col_types = FALSE
    )
    
    cat("Completed chunked processing for year", year, "\n")
    
  }, error = function(e) {
    cat("Error in chunked processing:", e$message, "\n")
  })
  
  # Final cleanup
  closeAllConnections()
  gc()
  
  if(length(results) > 0) {
    final_result <- do.call(rbind, results)
    cat("Year", year, "final result:", nrow(final_result), "rows\n")
    return(final_result)
  } else {
    return(data.frame(var1 = numeric(0), var4 = numeric(0), stringsAsFactors = FALSE))
  }
}

# Process all files with chunking
df <- map_dfr(years, process_file_chunked) %>%
  mutate(var7 = var1 + var4)
