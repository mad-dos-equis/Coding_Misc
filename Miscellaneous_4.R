###############################################################################
# pull_china_exports.R
#
# Pull monthly HTS-10 U.S. export data to China (CTY_CODE = 5700)
# for January 2020 – November 2025 via the Census Bureau International
# Trade API, using the {censusapi} package.
#
# Companion script to pull_china_imports.R — same structure, adapted
# for the exports/hs endpoint.
#
# Inputs:
#   - A Census API key (stored in ~/.Renviron as CENSUS_KEY)
#   - A vector of exact HTS-10 (Schedule B) codes
#   - Optional HS4/HS6 codes (pulled via wildcard to get all
#     underlying HTS-10 lines)
#
# Key differences from imports:
#   - Endpoint: timeseries/intltrade/exports/hs
#   - Commodity variable: E_COMMODITY (not I_COMMODITY)
#   - Value variables: ALL_VAL_MO (total), AIR_VAL_MO, VES_VAL_MO,
#     CNT_VAL_MO — no general/consumption/dutiable split
#   - Extra dimension: DF (1 = domestic exports, 2 = foreign exports)
#
# Strategy:
#   - One API call per code, using time = "from 2020-01 to 2025-11"
#   - Wildcard pulls for HS4/HS6 prefixes return all HTS-10 children.
#   - Results are deduplicated and combined into a single data frame.
#
# Output:
#   - china_exports_2020_2025.csv in the working directory
###############################################################################

library(censusapi)
library(purrr)
library(dplyr)
library(readr)

# ── 0. CONFIG ────────────────────────────────────────────────────────────────

# Census API key — reads from .Renviron (expects CENSUS_KEY=your_key_here)
RENVIRON_PATH <- file.path(Sys.getenv("HOME"), ".Renviron")
message(sprintf("Looking for API key in: %s", RENVIRON_PATH))

if (!file.exists(RENVIRON_PATH)) {
  stop(
    sprintf(".Renviron file not found at: %s\n", RENVIRON_PATH),
    "Create it and add: CENSUS_KEY=your_key_here\n",
    "Get a free key at: https://api.census.gov/data/key_signup.html"
  )
}

CENSUS_KEY <- Sys.getenv("CENSUS_KEY")
if (CENSUS_KEY == "") {
  CENSUS_KEY <- Sys.getenv("CENSUS_API_KEY")
}
if (CENSUS_KEY == "") {
  stop(
    sprintf(".Renviron exists at %s but no CENSUS_KEY or CENSUS_API_KEY found.\n", RENVIRON_PATH),
    "Add CENSUS_KEY=your_key to that file and restart R.\n",
    "Get a free key at: https://api.census.gov/data/key_signup.html"
  )
}
message(sprintf("Census API key loaded: %s...%s",
                substr(CENSUS_KEY, 1, 4),
                substr(CENSUS_KEY, nchar(CENSUS_KEY) - 3, nchar(CENSUS_KEY))))

# Time range
TIME_RANGE <- "from 2020-01 to 2025-11"

# Country: China
CTY <- "5700"

# Domestic/Foreign filter: "1" = domestic exports, "2" = foreign exports
# Set to NULL to retrieve both
DF_FILTER <- "1"

# Variables to retrieve from the exports/hs endpoint
EXPORT_VARS <- c(
  "E_COMMODITY",          # Schedule B / HTS-10 code
  "E_COMMODITY_LDESC",    # Long description
  "ALL_VAL_MO",           # Total export value (monthly)
  "ALL_VAL_YR",           # Total export value (year-to-date)
  "AIR_VAL_MO",           # Air value (monthly)
  "AIR_VAL_YR",           # Air value (YTD)
  "AIR_WGT_MO",           # Air shipping weight (monthly)
  "VES_VAL_MO",           # Vessel value (monthly)
  "VES_VAL_YR",           # Vessel value (YTD)
  "VES_WGT_MO",           # Vessel shipping weight (monthly)
  "CNT_VAL_MO",           # Containerized vessel value (monthly)
  "CNT_VAL_YR",           # Containerized vessel value (YTD)
  "CNT_WGT_MO",           # Containerized vessel shipping weight (monthly)
  "QTY_1_MO",             # Quantity 1 (monthly)
  "QTY_1_MO_FLAG",        # True-zero flag for quantity 1
  "UNIT_QY1",             # Unit of quantity 1
  "DF",                   # Domestic (1) or Foreign (2) export indicator
  "CTY_NAME"              # Country name
)


# ── 1. DEFINE YOUR CODES ────────────────────────────────────────────────────
# Replace these placeholders with your actual codes.
# NOTE: Export codes use Schedule B, which shares the same first 6 digits
# as HTS but can differ at the 10-digit level. Verify your codes against
# the Schedule B classification.

# Exact HTS-10 / Schedule B codes (10-digit strings, no dots)
hs10_codes <- c(
  # "8517120060",
  # "8471300100",
  # ... add your codes here ...
  NULL
) |> Filter(Negate(is.null), x = _)

# HS4 codes (4-digit strings) — will pull all HTS-10 underneath
hs4_codes <- c(
  # "8517",
  NULL
) |> Filter(Negate(is.null), x = _)

# HS6 codes (6-digit strings) — will pull all HTS-10 underneath
hs6_codes <- c(
  # "854231",
  # "847130",
  NULL
) |> Filter(Negate(is.null), x = _)


# ── 2. PULL FUNCTIONS ───────────────────────────────────────────────────────

#' Pull all months for a single exact HTS-10 export code
pull_exact <- function(code, call_num = NULL, total_calls = NULL) {
  progress <- if (!is.null(call_num)) {
    sprintf(" [%d/%d]", call_num, total_calls)
  } else {
    ""
  }
  message(sprintf("Pulling HTS-10: %s%s", code, progress))

  # Build the call arguments
  call_args <- list(
    name        = "timeseries/intltrade/exports/hs",
    vars        = EXPORT_VARS,
    time        = TIME_RANGE,
    CTY_CODE    = CTY,
    COMM_LVL    = "HS10",
    SUMMARY_LVL = "DET",
    E_COMMODITY = code
  )
  if (!is.null(DF_FILTER)) call_args$DF <- DF_FILTER

  tryCatch({
    df <- do.call(getCensus, call_args)
    df$pull_type <- "exact"
    df
  }, error = function(e) {
    warning(sprintf("  FAILED %s: %s", code, conditionMessage(e)))
    NULL
  })
}

#' Pull all months for all HTS-10 codes under a wildcard prefix (HS4 or HS6)
pull_wildcard <- function(prefix, call_num = NULL, total_calls = NULL) {
  progress <- if (!is.null(call_num)) {
    sprintf(" [%d/%d]", call_num, total_calls)
  } else {
    ""
  }
  n_digits <- nchar(prefix)
  level <- if (n_digits == 4) "HS4" else if (n_digits == 6) "HS6" else "other"
  message(sprintf("Pulling %s wildcard: %s*%s", level, prefix, progress))

  # Build the call arguments
  call_args <- list(
    name        = "timeseries/intltrade/exports/hs",
    vars        = EXPORT_VARS,
    time        = TIME_RANGE,
    CTY_CODE    = CTY,
    COMM_LVL    = "HS10",
    SUMMARY_LVL = "DET",
    E_COMMODITY = paste0(prefix, "*")
  )
  if (!is.null(DF_FILTER)) call_args$DF <- DF_FILTER

  tryCatch({
    df <- do.call(getCensus, call_args)
    df$pull_type   <- "wildcard"
    df$pull_prefix <- prefix
    df
  }, error = function(e) {
    warning(sprintf("  FAILED %s*: %s", prefix, conditionMessage(e)))
    NULL
  })
}


# ── 3. EXECUTE PULLS ────────────────────────────────────────────────────────

total_exact    <- length(hs10_codes)
total_wildcard <- length(hs4_codes) + length(hs6_codes)
total_calls    <- total_exact + total_wildcard

if (total_calls == 0) {
  stop("No codes defined. Populate hs10_codes, hs4_codes, or hs6_codes in section 1.")
}

message(sprintf(
  "\n=== Starting %d EXPORT API calls (%d exact + %d wildcard) ===\n",
  total_calls, total_exact, total_wildcard
))

start_time <- Sys.time()

# 3a. Exact HTS-10 pulls
exact_results <- if (total_exact > 0) {
  imap_dfr(hs10_codes, function(code, i) {
    pull_exact(code, call_num = i, total_calls = total_calls)
  })
} else {
  NULL
}

# 3b. Wildcard pulls (HS4 + HS6 prefixes)
wildcard_prefixes <- c(hs4_codes, hs6_codes)
wildcard_results <- if (length(wildcard_prefixes) > 0) {
  imap_dfr(wildcard_prefixes, function(prefix, i) {
    pull_wildcard(
      prefix,
      call_num    = total_exact + i,
      total_calls = total_calls
    )
  })
} else {
  NULL
}

elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 1)
message(sprintf("\n=== All export pulls complete in %s minutes ===\n", elapsed))


# ── 4. COMBINE & DEDUPLICATE ────────────────────────────────────────────────

china_exports <- bind_rows(exact_results, wildcard_results)

if (nrow(china_exports) == 0) {
  warning("No data returned. Check your codes and time range.")
} else {
  # Deduplicate: if a wildcard pull returned a code also in the exact list,
  # keep the exact version.
  china_exports <- china_exports |>
    arrange(pull_type) |>
    distinct(time, E_COMMODITY, DF, .keep_all = TRUE)

  # Clean up helper columns
  china_exports <- china_exports |>
    select(-pull_type, -any_of("pull_prefix"))

  # Parse the time column into YEAR and MONTH if not already present
  if (!"YEAR" %in% names(china_exports)) {
    china_exports <- china_exports |>
      mutate(
        YEAR  = substr(time, 1, 4),
        MONTH = substr(time, 6, 7)
      )
  }

  # ── 5. SUMMARY ────────────────────────────────────────────────────────────

  n_codes  <- n_distinct(china_exports$E_COMMODITY)
  n_months <- n_distinct(china_exports$time)
  n_rows   <- nrow(china_exports)

  message(sprintf("Result: %s rows | %d unique HTS-10 codes | %d months",
                  format(n_rows, big.mark = ","), n_codes, n_months))

  month_coverage <- china_exports |>
    distinct(YEAR, MONTH) |>
    arrange(YEAR, MONTH)
  message(sprintf("Date range: %s-%s to %s-%s",
                  first(month_coverage$YEAR), first(month_coverage$MONTH),
                  last(month_coverage$YEAR), last(month_coverage$MONTH)))

  # Check for failed codes (present in input but absent from results)
  returned_codes <- unique(china_exports$E_COMMODITY)
  missing_exact  <- setdiff(hs10_codes, returned_codes)
  if (length(missing_exact) > 0) {
    warning(sprintf(
      "%d exact codes returned no data: %s",
      length(missing_exact),
      paste(head(missing_exact, 10), collapse = ", ")
    ))
  }

  # ── 6. EXPORT ──────────────────────────────────────────────────────────────

  output_path <- "china_exports_2020_2025.csv"
  write_csv(china_exports, output_path)
  message(sprintf("Saved to: %s", output_path))
}
