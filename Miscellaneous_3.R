# ============================================================================
# Census Imports API - Usage Examples
# 
# First, source the main module:
#   source("pull_census_imports.R")
# ============================================================================

source("pull_census_imports.R")

# --------------------------------------------------
# Setup: Add your API key
# --------------------------------------------------

# Option 1: Set in .Renviron (recommended, persists across sessions)
# usethis::edit_r_environ()
# Add line: CENSUS_KEY=your_key_here
# Restart R

# Option 2: Set for this session
Sys.setenv(CENSUS_KEY = "your_census_api_key_here")

# Option 3: Pass directly to functions (shown below)
my_key <- Sys.getenv("CENSUS_KEY")

# --------------------------------------------------
# Example 1: Single country, monthly HTS-10
# --------------------------------------------------

china_monthly <- pull_imports(
  start_date = "2024-01",
  end_date = "2024-06",
  classification = "hs",
  comm_lvl = "HS10",
  country_scope = "specified",
  country_code = "5700",
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

# Rate provision codes:
#   61 = MFN/Column 1 dutiable
#   69 = Chapter 99 (Section 301/232) with duty reported
#   18 = Free under GSP/special programs
# See rate_provision_codes for full list

mfn_dutiable <- pull_imports(
  start_date = "2024-01",
  classification = "hs",
  comm_lvl = "HS10",
  country_scope = "specified",
  country_code = "5700",
  rate_provision = "61",
  key = my_key
)

# Section 301/232 tariffs
ch99_dutiable <- pull_imports(
  start_date = "2024-01",
  classification = "hs",
  comm_lvl = "HS10",
  country_scope = "specified",
  country_code = "5700",
  rate_provision = "69",
  key = my_key
)

# --------------------------------------------------
# Example 9: With caching and logging
# --------------------------------------------------

# Caching saves API responses to disk; subsequent runs skip completed calls
# Requires: install.packages("digest")

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

# View what was pulled
pull_metadata(cached_pull)

# If there were errors, retry just those periods
cached_pull <- retry_failed(cached_pull, key = my_key)

# --------------------------------------------------
# Example 11: Country code lookups
# --------------------------------------------------

# By ISO code
lookup_country_code("CN")
#> [1] "5700"

# By name (partial match)
lookup_country_code("germany")
#> [1] "4280"

lookup_country_code("korea")
#> Multiple matches found:
#> # A tibble: 2 × 3
#>   code  name        iso
#> 1 5790  North Korea KP
#> 2 5800  South Korea KR

lookup_country_code("south korea")
#> [1] "5800"

# Reverse lookup
lookup_country_name("5700")
#> [1] "China"

# View all countries
View(schedule_c_countries)

# View rate provision codes
View(rate_provision_codes)

# --------------------------------------------------
# Example 12: Common country groupings
# --------------------------------------------------

# USMCA
usmca <- c("1220", "2010")  # Canada, Mexico

# Major Asian trading partners
asia_major_codes <- c(
  "5700",  # China
  "5880",  # Japan
  "5800",  # South Korea
  "5830",  # Taiwan
  "5520",  # Vietnam
  "5330",  # India
  "5490",  # Thailand
  "5570",  # Malaysia
  "5600"   # Indonesia
)

# EU top 5
eu_top <- c(
  "4280",  # Germany
  "4279",  # France
  "4759",  # Italy
  "4210",  # Netherlands
  "4190"   # Ireland
)

# Pull for a grouping
asia_imports <- pull_imports(
  start_date = "2024-01",
  classification = "hs",
  comm_lvl = "HS6",
  country_scope = "specified",
  country_code = asia_major_codes,
  key = my_key
)

# --------------------------------------------------
# Example 13: List available API variables
# --------------------------------------------------

hs_vars <- list_import_variables("hs")
naics_vars <- list_import_variables("naics")
enduse_vars <- list_import_variables("enduse")
