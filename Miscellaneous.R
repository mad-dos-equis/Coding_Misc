install_packages_from_folder <- function(folder_path, 
                                        file_types = c(".zip", ".tar.gz", ".tgz"),
                                        install_type = "auto",
                                        dependencies = FALSE,
                                        force = FALSE,
                                        quiet = TRUE,
                                        skip_existing = TRUE,
                                        log_file = NULL) {
  
  # Validate inputs
  if (!dir.exists(folder_path)) {
    stop("Folder path does not exist: ", folder_path)
  }
  
  # Get all package files in the folder
  all_files <- list.files(folder_path, full.names = TRUE)
  
  # Filter for package files based on extensions
  pattern <- paste0("\\", file_types, "$", collapse = "|")
  pkg_files <- all_files[grepl(pattern, all_files, ignore.case = TRUE)]
  
  if (length(pkg_files) == 0) {
    message("No package files found in folder: ", folder_path)
    return(invisible(NULL))
  }
  
  # Determine installation type if auto
  if (install_type == "auto") {
    if (Sys.info()["sysname"] == "Windows") {
      install_type <- "win.binary"
    } else if (Sys.info()["sysname"] == "Darwin") {
      install_type <- "mac.binary"  
    } else {
      install_type <- "source"
    }
  }
  
  # Get currently installed packages if skip_existing is TRUE
  if (skip_existing) {
    installed_pkgs <- rownames(installed.packages())
  }
  
  # Initialize results tracking
  results <- data.frame(
    package_file = basename(pkg_files),
    package_name = character(length(pkg_files)),
    status = character(length(pkg_files)),
    error_message = character(length(pkg_files)),
    stringsAsFactors = FALSE
  )
  
  # Setup logging
  if (!is.null(log_file)) {
    log_conn <- file(log_file, "w")
    on.exit(close(log_conn), add = TRUE)
    writeLines(paste("Installation log started:", Sys.time()), log_conn)
  }
  
  # Function to log messages
  log_message <- function(msg) {
    if (!quiet) message(msg)
    if (!is.null(log_file)) {
      writeLines(paste(Sys.time(), "-", msg), log_conn)
    }
  }
  
  log_message(paste("Found", length(pkg_files), "package files"))
  log_message(paste("Installation type:", install_type))
  
  # Install each package
  successful <- 0
  skipped <- 0
  failed <- 0
  
  for (i in seq_along(pkg_files)) {
    pkg_file <- pkg_files[i]
    pkg_basename <- basename(pkg_file)
    
    # Extract package name from filename (remove version and extension)
    pkg_name <- gsub("_.*$", "", tools::file_path_sans_ext(pkg_basename))
    results$package_name[i] <- pkg_name
    
    # Skip if already installed
    if (skip_existing && pkg_name %in% installed_pkgs) {
      results$status[i] <- "skipped"
      results$error_message[i] <- "already installed"
      skipped <- skipped + 1
      log_message(paste("Skipping", pkg_name, "(already installed)"))
      next
    }
    
    # Attempt installation
    log_message(paste("Installing", pkg_name, "from", pkg_basename))
    
    tryCatch({
      install.packages(
        pkg_file, 
        repos = NULL, 
        type = install_type,
        dependencies = dependencies,
        quiet = quiet,
        force = force
      )
      results$status[i] <- "success"
      successful <- successful + 1
      log_message(paste("Successfully installed", pkg_name))
      
    }, error = function(e) {
      results$status[i] <- "failed"
      results$error_message[i] <- as.character(e$message)
      failed <- failed + 1
      log_message(paste("Failed to install", pkg_name, "- Error:", e$message))
    }, warning = function(w) {
      # Treat warnings as success but log them
      results$status[i] <- "success_with_warnings"
      results$error_message[i] <- as.character(w$message)
      successful <- successful + 1
      log_message(paste("Installed", pkg_name, "with warnings:", w$message))
    })
  }
  
  # Print summary
  log_message("=== Installation Summary ===")
  log_message(paste("Total packages processed:", length(pkg_files)))
  log_message(paste("Successful installations:", successful))
  log_message(paste("Skipped (already installed):", skipped))
  log_message(paste("Failed installations:", failed))
  
  # Return results
  attr(results, "summary") <- list(
    total = length(pkg_files),
    successful = successful,
    skipped = skipped,
    failed = failed
  )
  
  return(results)
}

# Convenience wrapper for common use cases
install_all_packages <- function(folder_path, skip_existing = TRUE, quiet = FALSE) {
  install_packages_from_folder(
    folder_path = folder_path,
    skip_existing = skip_existing,
    quiet = quiet,
    log_file = file.path(folder_path, "installation_log.txt")
  )
}

# Example usage:
# 
# # Basic usage - install all packages from a folder
# results <- install_all_packages("C:/path/to/your/package/folder")
# 
# # Advanced usage with custom options
# results <- install_packages_from_folder(
#   folder_path = "C:/path/to/your/package/folder",
#   file_types = c(".zip", ".tar.gz"),
#   install_type = "win.binary",  # or "mac.binary", "source", "auto"
#   dependencies = TRUE,
#   skip_existing = TRUE,
#   quiet = FALSE,
#   log_file = "installation.log"
# )
# 
# # View results
# print(results)
# View(results)  # If using RStudio
# 
# # Check summary
# attr(results, "summary")
# 
# # View only failed installations
# failed_packages <- results[results$status == "failed", ]
# print(failed_packages)
