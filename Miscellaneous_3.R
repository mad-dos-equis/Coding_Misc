# ============================================================================
# U.S. Imports for Consumption via censusapi
# Comprehensive data pull with logging, caching, validation, and progress
# 
# Author: [Your Name]
# Last Updated: 2025-01-30
# 
# Dependencies: censusapi, tidyverse, digest (optional, for caching)
# API Key: https://api.census.gov/data/key_signup.html
# ============================================================================

library(censusapi)
library(tidyverse)

# ============================================================================
# SCHEDULE C COUNTRY CODES (Official Census Bureau)
# Source: https://www.census.gov/foreign-trade/schedules/c/countryname.html
# ============================================================================

schedule_c_countries <- tribble(
  ~code, ~name, ~iso,
  "1000", "United States of America", "US",
  "1010", "Greenland", "GL",
  "1220", "Canada", "CA",
  "1610", "Saint Pierre and Miquelon", "PM",

  "2010", "Mexico", "MX",
  "2050", "Guatemala", "GT",
  "2080", "Belize", "BZ",
  "2110", "El Salvador", "SV",
  "2150", "Honduras", "HN",
  "2190", "Nicaragua", "NI",
  "2230", "Costa Rica", "CR",
  "2250", "Panama", "PA",
  "2320", "Bermuda", "BM",
  "2360", "Bahamas", "BS",
  "2390", "Cuba", "CU",
  "2410", "Jamaica", "JM",
  "2430", "Turks and Caicos Islands", "TC",
  "2440", "Cayman Islands", "KY",
  "2450", "Haiti", "HT",
  "2470", "Dominican Republic", "DO",
  "2481", "Anguilla", "AI",
  "2482", "British Virgin Islands", "VG",
  "2483", "Saint Kitts and Nevis", "KN",
  "2484", "Antigua and Barbuda", "AG",
  "2485", "Montserrat", "MS",
  "2486", "Dominica", "DM",
  "2487", "Saint Lucia", "LC",
  "2488", "Saint Vincent and the Grenadines", "VC",
  "2489", "Grenada", "GD",
  "2720", "Barbados", "BB",
  "2740", "Trinidad and Tobago", "TT",
  "2774", "Sint Maarten", "SX",
  "2777", "Curacao", "CW",
  "2779", "Aruba", "AW",
  "2831", "Guadeloupe", "GP",
  "2839", "Martinique", "MQ",

  "3010", "Colombia", "CO",
  "3070", "Venezuela", "VE",
  "3120", "Guyana", "GY",
  "3150", "Suriname", "SR",
  "3170", "French Guiana", "GF",
  "3310", "Ecuador", "EC",
  "3330", "Peru", "PE",
  "3350", "Bolivia", "BO",
  "3370", "Chile", "CL",
  "3510", "Brazil", "BR",
  "3530", "Paraguay", "PY",
  "3550", "Uruguay", "UY",
  "3570", "Argentina", "AR",
  "3720", "Falkland Islands (Islas Malvinas)", "FK",

  "4000", "Iceland", "IS",
  "4010", "Sweden", "SE",
  "4031", "Svalbard and Jan Mayen", "SJ",
  "4039", "Norway", "NO",
  "4050", "Finland", "FI",
  "4091", "Faroe Islands", "FO",
  "4099", "Denmark", "DK",
  "4120", "United Kingdom", "GB",
  "4190", "Ireland", "IE",
  "4210", "Netherlands", "NL",
  "4231", "Belgium", "BE",
  "4239", "Luxembourg", "LU",
  "4271", "Andorra", "AD",
  "4272", "Monaco", "MC",
  "4279", "France", "FR",
  "4280", "Germany", "DE",
  "4330", "Austria", "AT",
  "4351", "Czech Republic", "CZ",
  "4359", "Slovakia", "SK",
  "4370", "Hungary", "HU",
  "4411", "Liechtenstein", "LI",
  "4419", "Switzerland", "CH",
  "4470", "Estonia", "EE",
  "4490", "Latvia", "LV",
  "4510", "Lithuania", "LT",
  "4550", "Poland", "PL",
  "4621", "Russia", "RU",
  "4622", "Belarus", "BY",
  "4623", "Ukraine", "UA",
  "4631", "Armenia", "AM",
  "4632", "Azerbaijan", "AZ",
  "4633", "Georgia", "GE",
  "4634", "Kazakhstan", "KZ",
  "4635", "Kyrgyzstan", "KG",
  "4641", "Moldova", "MD",
  "4642", "Tajikistan", "TJ",
  "4643", "Turkmenistan", "TM",
  "4644", "Uzbekistan", "UZ",
  "4700", "Spain", "ES",
  "4710", "Portugal", "PT",
  "4720", "Gibraltar", "GI",
  "4730", "Malta", "MT",
  "4751", "San Marino", "SM",
  "4752", "Holy See (Vatican City)", "VA",
  "4759", "Italy", "IT",
  "4791", "Croatia", "HR",
  "4792", "Slovenia", "SI",
  "4793", "Bosnia and Herzegovina", "BA",
  "4794", "North Macedonia", "MK",
  "4801", "Serbia", "RS",
  "4803", "Kosovo", "KV",
  "4804", "Montenegro", "ME",
  "4810", "Albania", "AL",
  "4840", "Greece", "GR",
  "4850", "Romania", "RO",
  "4870", "Bulgaria", "BG",
  "4890", "Turkey", "TR",
  "4910", "Cyprus", "CY",

  "5020", "Syria", "SY",
  "5040", "Lebanon", "LB",
  "5050", "Iraq", "IQ",
  "5070", "Iran", "IR",
  "5081", "Israel", "IL",
  "5082", "Gaza Strip", "GZ",
  "5083", "West Bank", "WE",
  "5110", "Jordan", "JO",
  "5130", "Kuwait", "KW",
  "5170", "Saudi Arabia", "SA",
  "5180", "Qatar", "QA",
  "5200", "United Arab Emirates", "AE",
  "5210", "Yemen", "YE",
  "5230", "Oman", "OM",
  "5250", "Bahrain", "BH",
  "5310", "Afghanistan", "AF",
  "5330", "India", "IN",
  "5350", "Pakistan", "PK",
  "5360", "Nepal", "NP",
  "5380", "Bangladesh", "BD",
  "5420", "Sri Lanka", "LK",
  "5460", "Burma (Myanmar)", "MM",
  "5490", "Thailand", "TH",
  "5520", "Vietnam", "VN",
  "5530", "Laos", "LA",
  "5550", "Cambodia", "KH",
  "5570", "Malaysia", "MY",
  "5590", "Singapore", "SG",
  "5600", "Indonesia", "ID",
  "5601", "Timor-Leste", "TL",
  "5610", "Brunei", "BN",
  "5650", "Philippines", "PH",
  "5660", "Macao", "MO",
  "5682", "Bhutan", "BT",
  "5683", "Maldives", "MV",
  "5700", "China", "CN",
  "5740", "Mongolia", "MN",
  "5790", "North Korea", "KP",
  "5800", "South Korea", "KR",
  "5820", "Hong Kong", "HK",
  "5830", "Taiwan", "TW",
  "5880", "Japan", "JP",

  "6021", "Australia", "AU",
  "6022", "Norfolk Island", "NF",
  "6023", "Cocos (Keeling) Islands", "CC",
  "6024", "Christmas Island", "CX",
  "6029", "Heard Island and McDonald Islands", "HM",
  "6040", "Papua New Guinea", "PG",
  "6141", "New Zealand", "NZ",
  "6142", "Cook Islands", "CK",
  "6143", "Tokelau", "TK",
  "6144", "Niue", "NU",
  "6150", "Samoa", "WS",
  "6223", "Solomon Islands", "SB",
  "6224", "Vanuatu", "VU",
  "6225", "Pitcairn Islands", "PN",
  "6226", "Kiribati", "KI",
  "6227", "Tuvalu", "TV",
  "6412", "New Caledonia", "NC",
  "6413", "Wallis and Futuna", "WF",
  "6414", "French Polynesia", "PF",
  "6810", "Marshall Islands", "MH",
  "6820", "Micronesia", "FM",
  "6830", "Palau", "PW",
  "6862", "Nauru", "NR",
  "6863", "Fiji", "FJ",
  "6864", "Tonga", "TO",

  "7140", "Morocco", "MA",
  "7210", "Algeria", "DZ",
  "7230", "Tunisia", "TN",
  "7250", "Libya", "LY",
  "7290", "Egypt", "EG",
  "7321", "Sudan", "SD",
  "7323", "South Sudan", "SS",
  "7380", "Equatorial Guinea", "GQ",
  "7410", "Mauritania", "MR",
  "7420", "Cameroon", "CM",
  "7440", "Senegal", "SN",
  "7450", "Mali", "ML",
  "7460", "Guinea", "GN",
  "7470", "Sierra Leone", "SL",
  "7480", "Cote d'Ivoire", "CI",
  "7490", "Ghana", "GH",
  "7500", "Gambia", "GM",
  "7510", "Niger", "NE",
  "7520", "Togo", "TG",
  "7530", "Nigeria", "NG",
  "7540", "Central African Republic", "CF",
  "7550", "Gabon", "GA",
  "7560", "Chad", "TD",
  "7580", "Saint Helena", "SH",
  "7600", "Burkina Faso", "BF",
  "7610", "Benin", "BJ",
  "7620", "Angola", "AO",
  "7630", "Congo, Republic", "CG",
  "7642", "Guinea-Bissau", "GW",
  "7643", "Cabo Verde", "CV",
  "7644", "Sao Tome and Principe", "ST",
  "7650", "Liberia", "LR",
  "7660", "Congo, Democratic Republic", "CD",
  "7670", "Burundi", "BI",
  "7690", "Rwanda", "RW",
  "7700", "Somalia", "SO",
  "7741", "Eritrea", "ER",
  "7749", "Ethiopia", "ET",
  "7770", "Djibouti", "DJ",
  "7780", "Uganda", "UG",
  "7790", "Kenya", "KE",
  "7800", "Seychelles", "SC",
  "7810", "British Indian Ocean Territory", "IO",
  "7830", "Tanzania", "TZ",
  "7850", "Mauritius", "MU",
  "7870", "Mozambique", "MZ",
  "7880", "Madagascar", "MG",
  "7881", "Mayotte", "YT",
  "7890", "Comoros", "KM",
  "7904", "Reunion", "RE",
  "7905", "French Southern and Antarctic Lands", "TF",
  "7910", "South Africa", "ZA",
  "7920", "Namibia", "NA",
  "7930", "Botswana", "BW",
  "7940", "Zambia", "ZM",
  "7950", "Eswatini", "SZ",
  "7960", "Zimbabwe", "ZW",
  "7970", "Malawi", "MW",
  "7990", "Lesotho", "LS",

  "9030", "Puerto Rico", "PR",
  "9110", "Virgin Islands of the United States", "VI",
  "9350", "Guam", "GU",
  "9510", "American Samoa", "AS",
  "9610", "Northern Mariana Islands", "MP",
  "9800", "United States Minor Outlying Islands", "UM"
)

# ============================================================================
# RATE PROVISION CODES
# Source: https://www.census.gov/foreign-trade/reference/rpcodes.html
# ============================================================================

rate_provision_codes <- tribble(
  ~code, ~description, ~dutiable,
  "00", "Bonded warehouse/FTZ, duty not applicable", FALSE,
  "10", "Free under HTS chapters 01-97", FALSE,
  "11", "U.S. Virgin Islands, no duty", FALSE,
  "13", "Free, processed under bond for export", FALSE,
  "14", "Free, supplies for vessels/aircraft", FALSE,
  "16", "Free, U.S. government imports", FALSE,
  "17", "Free under HTS 9817.00.92/94/96", FALSE,
  "18", "Free under legislation/proclamation (GSP, Ch. 99 programs)", FALSE,
  "19", "Free under HTS Chapter 99", FALSE,
  "61", "Dutiable, General (Column 1/MFN) rates", TRUE,
  "62", "Dutiable, Column 2 rates", TRUE,
  "64", "Dutiable, Special rates", TRUE,
  "69", "Dutiable, Chapter 99 rates (duty reported)", TRUE,
  "70", "Dutiable, various/special rates (no duty calculated)", TRUE,
  "79", "Dutiable, Chapter 99 (no duty calculated)", TRUE
)

# ============================================================================
# HELPER: Country Code Lookup
# ============================================================================

#' Look up country code by name or ISO
#' @param query Country name (partial match) or ISO code
#' @return Character country code or NULL if not found
lookup_country_code <- function(query) {
  query_lower <- tolower(query)
  
  # Try exact ISO match first
 match <- schedule_c_countries |>
    filter(tolower(iso) == query_lower)
  
  if (nrow(match) == 1) return(match$code)
  
  # Try name match (partial)
  match <- schedule_c_countries |>
    filter(str_detect(tolower(name), query_lower))
  
  if (nrow(match) == 1) return(match$code)
  if (nrow(match) > 1) {
    message("Multiple matches found:")
    print(match |> select(code, name, iso))
    return(NULL)
  }
  
  message(sprintf("No match found for '%s'", query))
  NULL
}

#' Look up country name by code
#' @param code 4-digit country code
#' @return Country name or NULL
lookup_country_name <- function(code) {
  match <- schedule_c_countries |>
    filter(code == !!code)
  
  if (nrow(match) == 1) return(match$name)
  NULL
}

#' Get all country codes
#' @return Character vector of all valid country codes
get_all_country_codes <- function() {
  schedule_c_countries$code
}

# ============================================================================
# HELPER: Logging
# ============================================================================

create_logger <- function(verbose = TRUE, log_file = NULL) {
  function(msg, level = "INFO") {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    formatted <- sprintf("[%s] %s: %s", timestamp, level, msg)
    
    if (verbose) message(formatted)
    
    if (!is.null(log_file)) {
      cat(formatted, "\n", file = log_file, append = TRUE)
    }
  }
}

# ============================================================================
# HELPER: Validate API Key
# ============================================================================

validate_api_key <- function(key) {
  if (is.null(key) || key == "" || nchar(key) < 30) {
    stop(paste0(
      "Invalid or missing API key.\n",
      "Get one at: https://api.census.gov/data/key_signup.html\n",
      "Then either:\n",
      "  1. Add CENSUS_KEY=your_key to .Renviron (run usethis::edit_r_environ())\n",
      "  2. Pass key = 'your_key' to the function"
    ))
  }
  invisible(TRUE)
}

# ============================================================================
# HELPER: Validate Country Codes
# ============================================================================

validate_country_codes <- function(codes, log_fn = message) {
  valid_codes <- c(schedule_c_countries$code, "-")
  
  invalid <- setdiff(codes, valid_codes)
  
  if (length(invalid) > 0) {
    log_fn(sprintf("Invalid country codes: %s", paste(invalid, collapse = ", ")), level = "ERROR")
    return(FALSE)
  }
  
  TRUE
}

# ============================================================================
# HELPER: Rate Limiter with Backoff
# ============================================================================

create_rate_limiter <- function(base_delay = 0.3, max_delay = 60) {
  consecutive_failures <- 0
  
  list(
    wait = function() {
      delay <- min(base_delay * (2 ^ consecutive_failures), max_delay)
      Sys.sleep(delay)
    },
    
    success = function() {
      consecutive_failures <<- 0
    },
    
    failure = function() {
      consecutive_failures <<- consecutive_failures + 1
    },
    
    get_failures = function() {
      consecutive_failures
    }
  )
}

# ============================================================================
# HELPER: File-based Cache
# ============================================================================

create_cache <- function(cache_dir = NULL) {
  if (is.null(cache_dir)) {
    return(list(
      get = function(...) NULL,
      set = function(...) invisible(NULL),
      has = function(...) FALSE,
      clear = function(...) invisible(NULL)
    ))
  }
  
  # Check for digest package
  if (!requireNamespace("digest", quietly = TRUE)) {
    warning("Package 'digest' not installed. Caching disabled. Install with: install.packages('digest')")
    return(list(
      get = function(...) NULL,
      set = function(...) invisible(NULL),
      has = function(...) FALSE,
      clear = function(...) invisible(NULL)
    ))
  }
  
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  make_key <- function(...) {
    args <- list(...)
    digest::digest(args, algo = "md5")
  }
  
  list(
    get = function(...) {
      key <- make_key(...)
      path <- file.path(cache_dir, paste0(key, ".rds"))
      if (file.exists(path)) readRDS(path) else NULL
    },
    
    set = function(data, ...) {
      key <- make_key(...)
      path <- file.path(cache_dir, paste0(key, ".rds"))
      saveRDS(data, path)
      invisible(data)
    },
    
    has = function(...) {
      key <- make_key(...)
      path <- file.path(cache_dir, paste0(key, ".rds"))
      file.exists(path)
    },
    
    clear = function() {
      files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
      file.remove(files)
      invisible(length(files))
    }
  )
}

# ============================================================================
# HELPER: Progress Tracker
# ============================================================================

create_progress <- function(total, log_fn) {
  completed <- 0
  failed <- 0
  start_time <- Sys.time()
  
  list(
    increment = function(success = TRUE) {
      completed <<- completed + 1
      if (!success) failed <<- failed + 1
      
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      rate <- completed / elapsed
      remaining <- if (rate > 0) (total - completed) / rate else NA
      
      log_fn(sprintf(
        "Progress: %d/%d (%.1f%%) | Failed: %d | ETA: %.1f min",
        completed, total, 100 * completed / total,
        failed, remaining / 60
      ))
    },
    
    summary = function() {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
      list(
        total = total,
        completed = completed,
        failed = failed,
        elapsed_minutes = round(elapsed, 2)
      )
    }
  )
}

# ============================================================================
# HELPER: Parse and Validate Dates
# ============================================================================

parse_date_input <- function(date_str, frequency) {
  if (frequency == "monthly") {
    parsed <- tryCatch(
      ym(date_str),
      error = function(e) NULL
    )
    if (is.null(parsed)) {
      stop(sprintf(
        "Invalid date format '%s'. Expected YYYY-MM (e.g., '2024-01').",
        date_str
      ))
    }
  } else {
    parsed <- tryCatch(
      as.integer(date_str),
      error = function(e) NULL
    )
    if (is.null(parsed) || parsed < 1990 || parsed > 2100) {
      stop(sprintf(
        "Invalid year '%s'. Expected 4-digit year (e.g., '2024').",
        date_str
      ))
    }
  }
  parsed
}

# ============================================================================
# MAIN FUNCTION: pull_imports
# ============================================================================

#' Pull U.S. Imports for Consumption Data
#'
#' @param start_date Start date (YYYY-MM for monthly, YYYY for annual)
#' @param end_date End date (defaults to start_date)
#' @param frequency "monthly" or "annual"
#' @param classification "hs" (HTS), "naics", or "enduse"
#' @param comm_lvl Commodity level (e.g., "HS10", "HS6", "NA6", "EU5")
#' @param country_code Character vector of 4-digit country codes (for country_scope = "specified")
#' @param country_scope "specified" (use country_code), "all_individual", or "total"
#' @param rate_provision 2-digit rate provision code (HS only)
#' @param district 2-digit district code
#' @param summary_lvl "DET" (detail) or "CGP" (country groupings)
#' @param show_call Display underlying API call
#' @param rate_limit_delay Base delay between API calls (seconds)
#' @param max_retries Maximum retry attempts per API call
#' @param key Census API key
#' @param cache_dir Directory for caching results (NULL to disable)
#' @param log_file File path for logging (NULL for console only)
#' @param verbose Print progress messages
#' @param stop_on_error Stop on first error (FALSE to continue and collect errors)
#'
#' @return Data frame with import data; metadata attached as attribute
#'
#' @examples
#' \dontrun{
#' # Single country, monthly
#' china <- pull_imports("2024-01", "2024-06", country_code = "5700", key = "your_key")
#'
#' # Multiple countries
#' asia <- pull_imports("2024-01", country_code = c("5700", "5800", "5880"), key = "your_key")
#'
#' # All countries individually
#' all <- pull_imports("2024-01", country_scope = "all_individual", key = "your_key")
#'
#' # Total (no country breakout)
#' total <- pull_imports("2024-01", country_scope = "total", key = "your_key")
#'
#' # Using ISO codes
#' china <- pull_imports("2024-01", country_code = lookup_country_code("CN"), key = "your_key")
#' }
pull_imports <- function(
    start_date,
    end_date = start_date,
    frequency = c("monthly", "annual"),
    classification = c("hs", "naics", "enduse"),
    comm_lvl = NULL,
    country_code = NULL,
    country_scope = c("specified", "all_individual", "total"),
    rate_provision = NULL,
    district = NULL,
    summary_lvl = "DET",
    show_call = FALSE,
    rate_limit_delay = 0.3,
    max_retries = 5,
    key = Sys.getenv("CENSUS_KEY"),
    cache_dir = NULL,
    log_file = NULL,
    verbose = TRUE,
    stop_on_error = FALSE
) {
  
  # --------------------------------------------------------------------------
  # Setup
  # --------------------------------------------------------------------------
  
  log <- create_logger(verbose, log_file)
  cache <- create_cache(cache_dir)
  rate_limiter <- create_rate_limiter(rate_limit_delay)
  
  log("Starting import data pull")
  
  # --------------------------------------------------------------------------
  # Validate inputs
  # --------------------------------------------------------------------------
  
  frequency <- match.arg(frequency)
  classification <- match.arg(classification)
  country_scope <- match.arg(country_scope)
  
  validate_api_key(key)
  
  # Validate country_code + country_scope
  if (country_scope == "specified") {
    if (is.null(country_code) || length(country_code) == 0) {
      stop("country_scope = 'specified' requires country_code.")
    }
    if (!validate_country_codes(country_code, log)) {
      stop("Invalid country codes. Use schedule_c_countries to see valid codes.")
    }
  }
  
  if (country_scope %in% c("all_individual", "total") && !is.null(country_code)) {
    log("country_code ignored for this country_scope", level = "WARN")
    country_code <- NULL
  }
  
  # --------------------------------------------------------------------------
  # Classification configuration
  # --------------------------------------------------------------------------
  
  config <- switch(classification,
    hs = list(
      endpoint = "timeseries/intltrade/imports/hs",
      comm_var = "I_COMMODITY",
      comm_desc = "I_COMMODITY_LDESC",
      valid_levels = c("HS2", "HS4", "HS6", "HS10"),
      default_level = "HS10",
      commodity_prefixes = sprintf("%02d", 1:99),
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
      commodity_prefixes = c("11", "21", "22", "23", "31", "32", "33",
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
      commodity_prefixes = as.character(0:9),
      has_duty = TRUE,
      has_rp = FALSE,
      has_qty = FALSE
    )
  )
  
  if (is.null(comm_lvl)) {
    comm_lvl <- config$default_level
  }
  
  if (!comm_lvl %in% config$valid_levels) {
    stop(sprintf(
      "Invalid comm_lvl '%s' for %s. Valid: %s",
      comm_lvl, classification, paste(config$valid_levels, collapse = ", ")
    ))
  }
  
  if (!config$has_rp && !is.null(rate_provision)) {
    log("Rate provision filtering only available for HS. Ignoring.", level = "WARN")
    rate_provision <- NULL
  }
  
  # --------------------------------------------------------------------------
  # Parse and validate dates
  # --------------------------------------------------------------------------
  
  start_parsed <- parse_date_input(start_date, frequency)
  end_parsed <- parse_date_input(end_date, frequency)
  
  if (frequency == "monthly") {
    if (start_parsed > end_parsed) {
      stop("start_date must be before or equal to end_date.")
    }
    periods <- format(seq(start_parsed, end_parsed, by = "month"), "%Y-%m")
  } else {
    if (start_parsed > end_parsed) {
      stop("start_date must be before or equal to end_date.")
    }
    periods <- as.character(seq(start_parsed, end_parsed))
  }
  
  suffix <- if (frequency == "monthly") "_MO" else "_YR"
  
  # --------------------------------------------------------------------------
  # Build variable list
  # --------------------------------------------------------------------------
  
  vars <- c(
    "CTY_CODE", "CTY_NAME",
    config$comm_var, config$comm_desc,
    paste0("GEN_VAL", suffix),
    paste0("CON_VAL", suffix)
  )
  
  if (config$has_duty) {
    vars <- c(vars, paste0("CAL_DUT", suffix))
  }
  
  if (config$has_qty) {
    vars <- c(vars,
      paste0("GEN_QY1", suffix),
      paste0("GEN_QY2", suffix),
      paste0("CON_QY1", suffix),
      paste0("CON_QY2", suffix),
      "UNIT_QY1", "UNIT_QY2"
    )
  }
  
  if (config$has_rp && !is.null(rate_provision)) {
    vars <- c(vars, "RP")
  }
  
  # --------------------------------------------------------------------------
  # Core API call with caching and retry logic
  # --------------------------------------------------------------------------
  
  make_api_call <- function(period, commodity_filter, country_filter) {
    
    # Check cache first
    if (cache$has(period, commodity_filter, country_filter, comm_lvl, rate_provision)) {
      return(cache$get(period, commodity_filter, country_filter, comm_lvl, rate_provision))
    }
    
    args <- list(
      name = config$endpoint,
      vars = vars,
      time = period,
      key = key,
      show_call = show_call,
      convert_variables = FALSE,
      COMM_LVL = comm_lvl,
      SUMMARY_LVL = summary_lvl
    )
    
    if (!is.null(country_filter)) args$CTY_CODE <- country_filter
    if (!is.null(district)) args$DISTRICT <- district
    if (config$has_rp && !is.null(rate_provision)) args$RP <- rate_provision
    if (!is.null(commodity_filter)) args[[config$comm_var]] <- commodity_filter
    
    for (attempt in 1:max_retries) {
      rate_limiter$wait()
      
      result <- tryCatch({
        out <- do.call(getCensus, args)
        rate_limiter$success()
        
        if (!is.null(out) && nrow(out) > 0) {
          cache$set(out, period, commodity_filter, country_filter, comm_lvl, rate_provision)
          out
        } else {
          NULL
        }
      },
      error = function(e) {
        rate_limiter$failure()
        log(sprintf(
          "Attempt %d/%d failed for %s|%s|%s: %s",
          attempt, max_retries,
          period, commodity_filter %||% "ALL", country_filter %||% "ALL",
          conditionMessage(e)
        ), level = "ERROR")
        
        if (attempt == max_retries) {
          if (stop_on_error) {
            stop(e)
          } else {
            return(NULL)
          }
        }
        "RETRY"
      })
      
      if (!identical(result, "RETRY")) return(result)
    }
    
    NULL
  }
  
  # --------------------------------------------------------------------------
  # Pull by commodity prefix
  # --------------------------------------------------------------------------
  
  pull_by_commodity_prefix <- function(period, country_filter = NULL) {
    results <- map(config$commodity_prefixes, \(prefix) {
      make_api_call(period, paste0(prefix, "*"), country_filter)
    })
    compact(results) |> bind_rows()
  }
  
  # --------------------------------------------------------------------------
  # Calculate total chunks for progress tracking
  # --------------------------------------------------------------------------
  
  n_prefixes <- length(config$commodity_prefixes)
  n_periods <- length(periods)
  n_countries <- if (country_scope == "specified") length(country_code) else 1
  
  total_chunks <- n_periods * n_countries * n_prefixes
  
  # --------------------------------------------------------------------------
  # Log configuration
  # --------------------------------------------------------------------------
  
  log(sprintf(
    paste0(
      "Configuration:\n",
      "  Periods: %d (%s to %s) | Frequency: %s\n",
      "  Classification: %s | Level: %s\n",
      "  Country scope: %s | Countries: %s\n",
      "  RP: %s | District: %s\n",
      "  Total API calls (approx): %d"
    ),
    n_periods, periods[1], periods[length(periods)], frequency,
    toupper(classification), comm_lvl,
    country_scope,
    if (is.null(country_code)) "—" else paste(country_code, collapse = ", "),
    rate_provision %||% "ALL",
    district %||% "ALL",
    total_chunks
  ))
  
  # --------------------------------------------------------------------------
  # Main pull loop
  # --------------------------------------------------------------------------
  
  all_results <- list()
  errors <- list()
  
  for (i in seq_along(periods)) {
    period <- periods[i]
    log(sprintf("Processing period %d/%d: %s", i, n_periods, period))
    
    period_results <- tryCatch({
      switch(country_scope,
        
        specified = {
          map(country_code, \(cty) {
            log(sprintf("  Country: %s (%s)", cty, lookup_country_name(cty) %||% "Unknown"))
            pull_by_commodity_prefix(period, cty)
          }) |> compact() |> bind_rows()
        },
        
        all_individual = {
          pull_by_commodity_prefix(period, NULL)
        },
        
        total = {
          pull_by_commodity_prefix(period, "-")
        }
      )
    },
    error = function(e) {
      errors[[length(errors) + 1]] <<- list(
        period = period,
        error = conditionMessage(e)
      )
      log(sprintf("Period %s failed: %s", period, conditionMessage(e)), level = "ERROR")
      NULL
    })
    
    if (!is.null(period_results) && nrow(period_results) > 0) {
      period_results$period <- period
      all_results[[i]] <- period_results
    }
  }
  
  # --------------------------------------------------------------------------
  # Combine and standardize
  # --------------------------------------------------------------------------
  
  df <- bind_rows(compact(all_results))
  
  if (nrow(df) == 0) {
    log("No data returned.", level = "WARN")
    return(NULL)
  }
  
  rename_map <- c(
    gen_value = paste0("GEN_VAL", suffix),
    con_value = paste0("CON_VAL", suffix),
    duties = paste0("CAL_DUT", suffix),
    commodity = config$comm_var,
    commodity_desc = config$comm_desc
  )
  
  if (config$has_qty) {
    rename_map <- c(rename_map,
      gen_qty1 = paste0("GEN_QY1", suffix),
      gen_qty2 = paste0("GEN_QY2", suffix),
      con_qty1 = paste0("CON_QY1", suffix),
      con_qty2 = paste0("CON_QY2", suffix)
    )
  }
  
  rename_map <- rename_map[rename_map %in% names(df)]
  
  numeric_cols <- c("gen_value", "con_value", "duties",
                    "gen_qty1", "gen_qty2", "con_qty1", "con_qty2")
  
  df <- df |>
    rename(all_of(rename_map)) |>
    mutate(
      across(any_of(numeric_cols), as.numeric),
      frequency = frequency,
      classification = classification,
      country_scope = country_scope
    )
  
  # --------------------------------------------------------------------------
  # Final summary
  # --------------------------------------------------------------------------
  
  elapsed <- as.numeric(difftime(Sys.time(), 
                                  parse_date_time(log("", level = "TIME") |> str_extract("\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}"), 
                                                  orders = "ymd HMS"), 
                                  units = "mins"))
  
  log(sprintf(
    "Complete. Rows: %s | Errors: %d",
    format(nrow(df), big.mark = ","),
    length(errors)
  ))
  
  # Attach metadata as attribute
  attr(df, "pull_metadata") <- list(
    start_date = start_date,
    end_date = end_date,
    frequency = frequency,
    classification = classification,
    comm_lvl = comm_lvl,
    country_scope = country_scope,
    country_code = country_code,
    rate_provision = rate_provision,
    pulled_at = Sys.time(),
    rows = nrow(df),
    errors = errors
  )
  
  df
}

# ============================================================================
# CONVENIENCE: Inspect Pull Metadata
# ============================================================================

#' Get metadata from a pull_imports result
#' @param df Data frame returned by pull_imports
#' @return List of pull parameters and statistics
pull_metadata <- function(df) {
  attr(df, "pull_metadata")
}

# ============================================================================
# CONVENIENCE: Retry Failed Periods
# ============================================================================

#' Retry failed periods from a previous pull
#' @param df Data frame returned by pull_imports with errors
#' @param key Census API key
#' @param ... Additional arguments passed to pull_imports
#' @return Combined data frame with original + retried data
retry_failed <- function(df, key = Sys.getenv("CENSUS_KEY"), ...) {
  meta <- pull_metadata(df)
  
  if (is.null(meta) || length(meta$errors) == 0) {
    message("No failed periods to retry.")
    return(df)
  }
  
  failed_periods <- map_chr(meta$errors, "period")
  message(sprintf("Retrying %d failed periods: %s",
                  length(failed_periods),
                  paste(failed_periods, collapse = ", ")))
  
  retry_df <- pull_imports(
    start_date = min(failed_periods),
    end_date = max(failed_periods),
    frequency = meta$frequency,
    classification = meta$classification,
    comm_lvl = meta$comm_lvl,
    country_code = meta$country_code,
    country_scope = meta$country_scope,
    rate_provision = meta$rate_provision,
    key = key,
    ...
  )
  
  # Filter to only the failed periods
  retry_df <- retry_df |> filter(period %in% failed_periods)
  
  # Combine with original (excluding failed periods)
  combined <- bind_rows(
    df |> filter(!period %in% failed_periods),
    retry_df
  ) |> arrange(period, commodity)
  
  combined
}

# ============================================================================
# CONVENIENCE: Pull by ISO Codes
# ============================================================================

#' Pull imports using ISO country codes
#' @param start_date Start date
#' @param end_date End date
#' @param iso_codes Character vector of ISO codes (e.g., c("CN", "MX"))
#' @param ... Additional arguments passed to pull_imports
#' @return Data frame from pull_imports
pull_imports_iso <- function(start_date, end_date = start_date, iso_codes, ...) {
  country_codes <- map_chr(iso_codes, \(iso) {
    code <- lookup_country_code(iso)
    if (is.null(code)) stop(sprintf("Invalid ISO code: %s", iso))
    code
  })
  
  pull_imports(
    start_date = start_date,
    end_date = end_date,
    country_scope = "specified",
    country_code = country_codes,
    ...
  )
}

# ============================================================================
# CONVENIENCE: List Available API Metadata
# ============================================================================

#' List variables available in an endpoint
#' @param classification "hs", "naics", or "enduse"
#' @return Data frame of variable metadata
list_import_variables <- function(classification = c("hs", "naics", "enduse")) {
  classification <- match.arg(classification)
  
  endpoint <- switch(classification,
    hs = "timeseries/intltrade/imports/hs",
    naics = "timeseries/intltrade/imports/naics",
    enduse = "timeseries/intltrade/imports/enduse"
  )
  
  listCensusMetadata(name = endpoint, type = "variables")
}

# ============================================================================
# USAGE EXAMPLES
# ============================================================================

# Examples are wrapped in if(FALSE) to prevent execution on source()

if (FALSE) {
  
  # --------------------------------------------------
  # Setup: Add your API key
  # --------------------------------------------------
  
  my_key <- "your_census_api_key_here"
  
  # Or set in .Renviron (recommended):
  # usethis::edit_r_environ()
  # Add: CENSUS_KEY=your_key_here
  
  # --------------------------------------------------
  # Example 1: Single country, monthly HTS-10
  # --------------------------------------------------
  
  china_monthly <- pull_imports(
    start_date = "2024-01",
    end_date = "2024-06",
    classification = "hs",
    comm_lvl = "HS10",
    country_scope = "specified",
    country_code = "5700",  # China
    key = my_key
  )
  
  # --------------------------------------------------
  # Example 2: Multiple countries using ISO codes
  # --------------------------------------------------
  
  asia_major <- pull_imports_iso(
    start_date = "2024-01",
    end_date = "2024-03",
    iso_codes = c("CN", "JP", "KR", "TW", "VN"),
    classification = "hs",
    comm_lvl = "HS6",
    key = my_key
  )
  
  # --------------------------------------------------
  # Example 3: All countries individually
  # --------------------------------------------------
  
  all_countries <- pull_imports(
    start_date = "2024-01",
    classification = "hs",
    comm_lvl = "HS6",
    country_scope = "all_individual",
    key = my_key
  )
  
  # --------------------------------------------------
  # Example 4: Total (aggregated across countries)
  # --------------------------------------------------
  
  totals <- pull_imports(
    start_date = "2024-01",
    end_date = "2024-12",
    classification = "hs",
    comm_lvl = "HS6",
    country_scope = "total",
    key = my_key
  )
  
  # --------------------------------------------------
  # Example 5: NAICS classification
  # --------------------------------------------------
  
  naics_data <- pull_imports(
    start_date = "2024-01",
    classification = "naics",
    comm_lvl = "NA6",
    country_scope = "specified",
    country_code = "5700",
    key = my_key
  )
  
  # --------------------------------------------------
  # Example 6: End-use classification
  # --------------------------------------------------
  
  enduse_data <- pull_imports(
    start_date = "2024-01",
    classification = "enduse",
    comm_lvl = "EU5",
    country_scope = "all_individual",
    key = my_key
  )
  
  # --------------------------------------------------
  # Example 7: Annual data
  # --------------------------------------------------
  
  annual_data <- pull_imports(
    start_date = "2020",
    end_date = "2023",
    frequency = "annual",
    classification = "hs",
    comm_lvl = "HS6",
    country_scope = "specified",
    country_code = "5700",
    key = my_key
  )
  
  # --------------------------------------------------
  # Example 8: With rate provision filter (HS only)
  # --------------------------------------------------
  
  mfn_dutiable <- pull_imports(
    start_date = "2024-01",
    classification = "hs",
    comm_lvl = "HS10",
    country_scope = "specified",
    country_code = "5700",
    rate_provision = "61",  # MFN dutiable
    key = my_key
  )
  
  # --------------------------------------------------
  # Example 9: With caching and logging
  # --------------------------------------------------
  
  cached_pull <- pull_imports(
    start_date = "2024-01",
    end_date = "2024-12",
    classification = "hs",
    comm_lvl = "HS10",
    country_scope = "specified",
    country_code = "5700",
    cache_dir = "cache/census_imports",
    log_file = "logs/import_pull.log",
    key = my_key
  )
  
  # --------------------------------------------------
  # Example 10: Check metadata and retry failures
  # --------------------------------------------------
  
  # View pull metadata
  pull_metadata(cached_pull)
  
  # Retry any failed periods
  cached_pull <- retry_failed(cached_pull, key = my_key)
  
  # --------------------------------------------------
  # Example 11: Look up country codes
  # --------------------------------------------------
  
  lookup_country_code("CN")
  #> [1] "5700"
  
  lookup_country_code("germany")
  #> [1] "4280"
  
  lookup_country_name("5700")
  #> [1] "China"
  
  # View all countries
  View(schedule_c_countries)
  
  # View rate provision codes
  View(rate_provision_codes)
  
}
