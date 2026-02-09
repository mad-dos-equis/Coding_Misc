###############################################################################
# pull_china_imports.R
#
# Pull monthly HTS-10 U.S. import data from China (CTY_CODE = 5700)
# for January 2020 – November 2025 via the Census Bureau International
# Trade API, using the {censusapi} package.
#
# Inputs:
#   - A Census API key (stored in .Renviron or passed directly)
#   - A vector of 312 exact HTS-10 codes
#   - 1 HS4 code and 2 HS6 codes (pulled via wildcard to get all
#     underlying HTS-10 lines)
#
# Strategy:
#   - One API call per code, using time = "from 2020-01 to 2025-11"
#     to retrieve all 71 months in a single request.
#   - Wildcard pulls for HS4/HS6 prefixes return all HTS-10 children.
#   - Results are deduplicated and combined into a single data frame.
#   - Total calls: ~315. Estimated runtime: 5–10 minutes.
#
# Output:
#   - china_imports_2020_2025.csv in the working directory
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

# Variables to retrieve from the imports/hs endpoint
IMPORT_VARS <- c(
  "I_COMMODITY",          # HTS-10 code

"I_COMMODITY_LDESC",    # Long description
  "GEN_VAL_MO",           # General imports value (monthly)
  "GEN_VAL_YR",           # General imports value (year-to-date)
  "CON_VAL_MO",           # Imports for consumption value (monthly)
  "CON_VAL_YR",           # Imports for consumption value (YTD)
  "DUT_VAL_MO",           # Dutiable value (monthly)
  "DUT_VAL_YR",           # Dutiable value (YTD)
  "CAL_DUT_MO",           # Calculated duty (monthly)
  "CAL_DUT_YR",           # Calculated duty (YTD)
  "GEN_CHA_MO",           # General imports charges (monthly)
  "GEN_CIF_MO",           # General imports CIF value (monthly)
  "GEN_QY1_MO",           # General imports quantity 1 (monthly)
  "GEN_QY1_MO_FLAG",      # True-zero flag for quantity 1
  "UNIT_QY1",             # Unit of quantity 1
  "CTY_NAME"              # Country name
)

# ── 1. DEFINE YOUR CODES ────────────────────────────────────────────────────
# Replace these placeholders with your actual codes.

# 312 exact HTS-10 codes (10-digit strings, no dots)
hs10_codes <- c(
  # "8517120060",
  # "8471300100",
  # ... add all 312 codes here ...
  NULL
) |> Filter(Negate(is.null), x = _)

# 1 HS4 code (4-digit string) — will pull all HTS-10 underneath
hs4_code <- c(
  # "8517"
  NULL
) |> Filter(Negate(is.null), x = _)

# 2 HS6 codes (6-digit strings) — will pull all HTS-10 underneath
hs6_codes <- c(
  # "854231",
  # "847130"
  NULL
) |> Filter(Negate(is.null), x = _)


# ── 2. PULL FUNCTIONS ───────────────────────────────────────────────────────

#' Pull all months for a single exact HTS-10 code
pull_exact <- function(code, call_num = NULL, total_calls = NULL) {
  progress <- if (!is.null(call_num)) {
    sprintf(" [%d/%d]", call_num, total_calls)
  } else {
    ""
  }
  message(sprintf("Pulling HTS-10: %s%s", code, progress))

  tryCatch({
    df <- getCensus(
      name    = "timeseries/intltrade/imports/hs",
      vars    = IMPORT_VARS,
      time    = TIME_RANGE,
      CTY_CODE    = CTY,
      COMM_LVL    = "HS10",
      SUMMARY_LVL = "DET",
      I_COMMODITY  = code
    )
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

  tryCatch({
    df <- getCensus(
      name    = "timeseries/intltrade/imports/hs",
      vars    = IMPORT_VARS,
      time    = TIME_RANGE,
      CTY_CODE    = CTY,
      COMM_LVL    = "HS10",
      SUMMARY_LVL = "DET",
      I_COMMODITY  = paste0(prefix, "*")
    )
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
total_wildcard <- length(hs4_code) + length(hs6_codes)
total_calls    <- total_exact + total_wildcard

message(sprintf(
  "\n=== Starting %d API calls (%d exact + %d wildcard) ===\n",
  total_calls, total_exact, total_wildcard
))

start_time <- Sys.time()

# 3a. Exact HTS-10 pulls
exact_results <- imap_dfr(hs10_codes, function(code, i) {
  pull_exact(code, call_num = i, total_calls = total_calls)
})

# 3b. Wildcard pulls (HS4 + HS6 prefixes)
wildcard_prefixes <- c(hs4_code, hs6_codes)
wildcard_results <- imap_dfr(wildcard_prefixes, function(prefix, i) {
  pull_wildcard(
    prefix,
    call_num  = total_exact + i,
    total_calls = total_calls
  )
})

elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 1)
message(sprintf("\n=== All pulls complete in %s minutes ===\n", elapsed))


# ── 4. COMBINE & DEDUPLICATE ────────────────────────────────────────────────

china_imports <- bind_rows(exact_results, wildcard_results)

# Deduplicate: if a wildcard pull returned an HTS-10 code that was also
# in the exact list, keep the exact version.
china_imports <- china_imports |>
  arrange(pull_type) |>                           # "exact" sorts before "wildcard"
  distinct(time, I_COMMODITY, .keep_all = TRUE)

# Clean up helper columns
china_imports <- china_imports |>
  select(-pull_type, -any_of("pull_prefix"))

# Parse the time column into YEAR and MONTH if not already present
if (!"YEAR" %in% names(china_imports)) {
  china_imports <- china_imports |>
    mutate(
      YEAR  = substr(time, 1, 4),
      MONTH = substr(time, 6, 7)
    )
}

# ── 5. SUMMARY ──────────────────────────────────────────────────────────────

n_codes  <- n_distinct(china_imports$I_COMMODITY)
n_months <- n_distinct(china_imports$time)
n_rows   <- nrow(china_imports)

message(sprintf("Result: %s rows | %d unique HTS-10 codes | %d months",
                format(n_rows, big.mark = ","), n_codes, n_months))

# Quick check: which months are present?
month_coverage <- china_imports |>
  distinct(YEAR, MONTH) |>
  arrange(YEAR, MONTH)
message(sprintf("Date range: %s-%s to %s-%s",
                first(month_coverage$YEAR), first(month_coverage$MONTH),
                last(month_coverage$YEAR), last(month_coverage$MONTH)))


# ── 6. EXPORT ────────────────────────────────────────────────────────────────

output_path <- "china_imports_2020_2025.csv"
write_csv(china_imports, output_path)
message(sprintf("Saved to: %s", output_path))
