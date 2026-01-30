# ============================================================================
# U.S. Imports for Consumption via censusapi
# Supports HTS, NAICS, and End-use classification systems
# ============================================================================

library(censusapi)
library(tidyverse)

pull_imports <- function(
    start_date,
    end_date = start_date,
    frequency = c("monthly", "annual"),
    classification = c("hs", "naics", "enduse"),
    comm_lvl = NULL,
    country_code = NULL,
    rate_provision = NULL,
    district = NULL,
    summary_lvl = "DET",
    chunk_by_prefix = TRUE,
    show_call = FALSE,
    rate_limit_delay = 0.3,
    max_retries = 3
) {
  
  frequency <- match.arg(frequency)
  classification <- match.arg(classification)
  
  # --------------------------------------------------------------------------
  # Classification-specific configuration
  # --------------------------------------------------------------------------
  
  config <- switch(classification,
    
    hs = list(
      endpoint = "timeseries/intltrade/imports/hs",
      comm_var = "I_COMMODITY",
      comm_desc = "I_COMMODITY_LDESC",
      valid_levels = c("HS2", "HS4", "HS6", "HS10"),
      default_level = "HS10",
      prefix_range = sprintf("%02d", 1:99),
      has_duty = TRUE,
      has_rp = TRUE,
      has_qty = TRUE
    ),
    
    naics = list(
      endpoint = "timeseries/intltrade/imports/naics",
      comm_var = "NAICS",
      comm_desc = "NAICS_LDESC",
      valid_levels = c("NA2", "NA3", "NA4", "NA5", "NA6"),
      default_level = "NA6",
      prefix_range = c("11", "21", "22", "23", "31", "32", "33", 
                       "42", "44", "45", "48", "49", "51", "52", 
                       "53", "54", "55", "56", "61", "62", "71", 
                       "72", "81", "92"),
      has_duty = TRUE,
      has_rp = FALSE,
      has_qty = FALSE
    ),
    
    enduse = list(
      endpoint = "timeseries/intltrade/imports/enduse",
      comm_var = "I_ENDUSE",
      comm_desc = "I_ENDUSE_LDESC",
      valid_levels = c("EU1", "EU5"),
      default_level = "EU5",
      prefix_range = as.character(0:9),
      has_duty = TRUE,
      has_rp = FALSE,
      has_qty = FALSE
    )
  )
  
  # Set default comm_lvl if not provided
 if (is.null(comm_lvl)) {
    comm_lvl <- config$default_level
  }
  
  # Validate comm_lvl
  if (!comm_lvl %in% config$valid_levels) {
    stop(sprintf(
      "Invalid comm_lvl '%s' for %s classification. Valid options: %s",
      comm_lvl, classification, paste(config$valid_levels, collapse = ", ")
    ))
  }
  
  # --------------------------------------------------------------------------
  # Build time periods
  # --------------------------------------------------------------------------
  
  if (frequency == "monthly") {
    start <- ym(start_date)
    end <- ym(end_date)
    periods <- format(seq(start, end, by = "month"), "%Y-%m")
    suffix <- "_MO"
  } else {
    start <- as.integer(str_extract(start_date, "^\\d{4}"))
    end <- as.integer(str_extract(end_date, "^\\d{4}"))
    periods <- as.character(seq(start, end))
    suffix <- "_YR"
  }
  
  # --------------------------------------------------------------------------
  # Build variable list based on classification
  # --------------------------------------------------------------------------
  
  # Core value variables (all endpoints have these)
  vars <- c(
    "CTY_CODE", "CTY_NAME",
    config$comm_var, config$comm_desc,
    paste0("GEN_VAL", suffix),
    paste0("CON_VAL", suffix)
  )
  
  # Duty variable (all endpoints)
  if (config$has_duty) {
    vars <- c(vars, paste0("CAL_DUT", suffix))
  }
  
  # Quantity variables (HS only)
  if (config$has_qty) {
    vars <- c(vars,
      paste0("GEN_QY1", suffix),
      paste0("GEN_QY2", suffix),
      paste0("CON_QY1", suffix),
      paste0("CON_QY2", suffix),
      "UNIT_QY1", "UNIT_QY2"
    )
  }
  
  # Rate provision (HS only)
  if (config$has_rp && !is.null(rate_provision)) {
    vars <- c(vars, "RP")
  }
  
  # Warn if RP requested for non-HS
 if (!config$has_rp && !is.null(rate_provision)) {
    warning("Rate provision (RP) filtering only available for HS classification. Ignoring.")
    rate_provision <- NULL
 }
  
  # --------------------------------------------------------------------------
  # Inner function: pull a single time period
  # --------------------------------------------------------------------------
  
  pull_single_period <- function(period) {
    
    base_call <- function(prefix_filter = NULL) {
      
      args <- list(
        name = config$endpoint,
        vars = vars,
        time = period,
        show_call = show_call,
        convert_variables = FALSE,
        COMM_LVL = comm_lvl,
        SUMMARY_LVL = summary_lvl
      )
      
      # Optional filters
      if (!is.null(country_code)) args$CTY_CODE <- country_code
      if (!is.null(district)) args$DISTRICT <- district
      
      # RP only for HS
      if (config$has_rp && !is.null(rate_provision)) {
        args$RP <- rate_provision
      }
      
      # Commodity prefix filter (for chunking)
      if (!is.null(prefix_filter)) {
        args[[config$comm_var]] <- prefix_filter
      }
      
      args
    }
    
    # ------------------------------------------------------------------
    # Chunked by prefix
    # ------------------------------------------------------------------
    if (chunk_by_prefix) {
      
      results <- map(config$prefix_range, \(prefix) {
        Sys.sleep(rate_limit_delay)
        
        args <- base_call(prefix_filter = paste0(prefix, "*"))
        
        for (attempt in 1:max_retries) {
          out <- tryCatch(
            do.call(getCensus, args),
            error = function(e) {
              if (attempt < max_retries) Sys.sleep(2^attempt)
              NULL
            }
          )
          if (!is.null(out) && nrow(out) > 0) break
        }
        out
      })
      
      df <- bind_rows(compact(results))
      
    # ------------------------------------------------------------------
    # Single pull
    # ------------------------------------------------------------------
    } else {
      
      args <- base_call()
      
      for (attempt in 1:max_retries) {
        df <- tryCatch(
          do.call(getCensus, args),
          error = function(e) {
            message(sprintf("Attempt %d failed for %s: %s",
                            attempt, period, e$message))
            if (attempt < max_retries) Sys.sleep(2^attempt)
            NULL
          }
        )
        if (!is.null(df)) break
      }
    }
    
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    df$period <- period
    df
  }
  
  # --------------------------------------------------------------------------
  # Loop over periods
  # --------------------------------------------------------------------------
  
  message(sprintf(
    paste0(
      "Pulling %d %s period(s): %s to %s\n",
      "  Classification: %s | Level: %s\n",
      "  Country: %s | RP: %s | District: %s"
    ),
    length(periods), frequency,
    periods[1], periods[length(periods)],
    toupper(classification), comm_lvl,
    if (is.null(country_code)) "ALL" else country_code,
    if (is.null(rate_provision)) "ALL" else rate_provision,
    if (is.null(district)) "ALL" else district
  ))
  
  results <- imap(periods, \(p, i) {
    message(sprintf("[%d/%d] %s", i, length(periods), p))
    pull_single_period(p)
  })
  
  # --------------------------------------------------------------------------
  # Combine and standardize
  # --------------------------------------------------------------------------
  
  df <- bind_rows(compact(results))
  
  if (nrow(df) == 0) {
    warning("No data returned.")
    return(NULL)
  }
  
  # Build rename map dynamically based on what exists
  rename_map <- c(
    gen_value = paste0("GEN_VAL", suffix),
    con_value = paste0("CON_VAL", suffix),
    duties = paste0("CAL_DUT", suffix)
  )
  
  if (config$has_qty) {
    rename_map <- c(rename_map,
      gen_qty1 = paste0("GEN_QY1", suffix),
      gen_qty2 = paste0("GEN_QY2", suffix),
      con_qty1 = paste0("CON_QY1", suffix),
      con_qty2 = paste0("CON_QY2", suffix)
    )
  }
  
  # Standardize commodity column name
  rename_map <- c(rename_map, setNames(config$comm_var, "commodity"))
  rename_map <- c(rename_map, setNames(config$comm_desc, "commodity_desc"))
  
  # Only rename columns that exist
  rename_map <- rename_map[rename_map %in% names(df)]
  
  numeric_cols <- c("gen_value", "con_value", "duties",
                    "gen_qty1", "gen_qty2", "con_qty1", "con_qty2")
  
  df <- df |>
    rename(all_of(rename_map)) |>
    mutate(
      across(any_of(numeric_cols), as.numeric),
      frequency = frequency,
      classification = classification
    )
  
  df
}

# ----------------------------------------------------------------------------
# Usage Examples
# ----------------------------------------------------------------------------

# HTS-10 level, China, monthly
china_hs10 <- pull_imports(
  start_date = "2024-01",
  end_date = "2024-03",
  classification = "hs",
  comm_lvl = "HS10",
  country_code = "5700"
)

# NAICS 6-digit, all countries, monthly
naics_full <- pull_imports(
  start_date = "2024-01",
  classification = "naics",
  comm_lvl = "NA6"
)

# NAICS 4-digit, annual, multiple years
naics_annual <- pull_imports(
  start_date = "2020",
  end_date = "2023",
  frequency = "annual",
  classification = "naics",
  comm_lvl = "NA4",
  chunk_by_prefix = FALSE
)

# End-use 5-digit, China
enduse_china <- pull_imports(
  start_date = "2024-01",
  end_date = "2024-06",
  classification = "enduse",
  comm_lvl = "EU5",
  country_code = "5700"
)

# End-use 1-digit (broad categories), all countries
enduse_broad <- pull_imports(
  start_date = "2024-01",
  classification = "enduse",
  comm_lvl = "EU1",
  chunk_by_prefix = FALSE
)

# HTS with rate provision filter (MFN dutiable only)
hs_mfn <- pull_imports(
  start_date = "2024-01",
  classification = "hs",
  comm_lvl = "HS6",
  country_code = "5700",
  rate_provision = "61"
)

# Compare: RP filtering only works for HS
# This will warn and ignore the RP parameter:
naics_with_rp <- pull_imports(
  start_date = "2024-01",
  classification = "naics",
  rate_provision = "61"  
)



# HS variables
hs_vars <- listCensusMetadata(
  name = "timeseries/intltrade/imports/hs",
  type = "variables"
)

# NAICS variables
naics_vars <- listCensusMetadata(
  name = "timeseries/intltrade/imports/naics",
  type = "variables"
)

# End-use variables
enduse_vars <- listCensusMetadata(
  name = "timeseries/intltrade/imports/enduse",
  type = "variables"
)
