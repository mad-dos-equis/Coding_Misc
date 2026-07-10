# ===========================================================================
# Cities per congressional district -> continuous choropleth
# ---------------------------------------------------------------------------
# Your dataset has a congressional-district column ("ME-02", "NY-20", ...) and
# a city column, but no geometry. This script:
#   1. Counts cities per district.
#   2. Pulls 119th-Congress district polygons from tigris (no shapefile to
#      download by hand -- tigris fetches + caches Census TIGER/Line files).
#   3. Reconciles the two on a normalized "XX-NN" key (handles the FIPS-vs-
#      abbreviation mismatch, at-large "00" coding, and prints anything that
#      fails to match so you can eyeball it).
#   4. Draws a continuous-fill map: more cities = darker.
#
# Expected input (CSV shown; swap the reader for xlsx/parquet if needed):
#   cd,city
#   ME-02,Bangor
#   ME-02,Lewiston
#   NY-20,Albany
#   ...
# ===========================================================================

## ---- 0. Packages ----------------------------------------------------------
# install.packages(c("tigris","sf","dplyr","tidyr","stringr","ggplot2","readr"))
library(tigris)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readr)

options(tigris_use_cache = TRUE)   # cache shapefiles between runs (recommended)

## ---- 1. Parameters you may need to change ---------------------------------
data_path <- "your_data.csv"       # <- path to your dataset
cd_col    <- "cd"                  # <- column holding "ME-02", "NY-20", ...
city_col  <- "city"                # <- column holding city names

cd_year   <- 2024                  # 2024 = 119th Congress (current default).
                                   #   2022 or 2023 = 118th Congress.
                                   #   IMPORTANT: match this to the redistricting
                                   #   vintage your city->district assignment was
                                   #   built on. AL, GA, LA, NY, NC changed
                                   #   between the 118th and 119th.

count_distinct_cities <- TRUE      # TRUE  = distinct city names per district
                                   # FALSE = raw row count per district
missing_is_zero <- TRUE            # districts absent from your data ->
                                   #   TRUE : drawn as 0 (colored)
                                   #   FALSE: drawn as NA (grey)
drop_territories <- TRUE           # drop PR / island-area delegate records
                                   #   (keeps the 50 states + DC)

## ---- 2. Read data + count cities per district -----------------------------
raw <- read_csv(data_path, show_col_types = FALSE)

# Normalize a district code to "XX-NN": uppercase 2-letter state, 2-digit
# district. Anything with no district number (e.g. "AK", "AK-AL") -> "-00",
# which we then reconcile against the geometry's actual single-district code.
norm_cd <- function(x) {
  x     <- toupper(trimws(as.character(x)))
  state <- sub("^([A-Z]{2}).*$", "\\1", x)
  digs  <- gsub("\\D", "", sub("^[A-Z]{2}", "", x))   # digits after the state
  digs  <- ifelse(digs == "", "00", digs)
  digs  <- str_pad(digs, width = 2, pad = "0")
  paste0(state, "-", digs)
}

counts <- raw %>%
  transmute(
    cd_key = norm_cd(.data[[cd_col]]),
    city   = .data[[city_col]]
  ) %>%
  filter(!is.na(city), grepl("^[A-Z]{2}-", cd_key)) %>%
  group_by(cd_key) %>%
  summarise(
    n_cities = if (count_distinct_cities) n_distinct(city) else n(),
    .groups  = "drop"
  )

## ---- 3. District geometry from tigris -------------------------------------
# cb = TRUE  -> generalized cartographic-boundary file (small, fast, national)
# resolution -> "20m" is coarse but plenty for a national choropleth; use "5m"
#               or "500k" for more detail.
cd_geo <- congressional_districts(
  cb = TRUE, resolution = "20m", year = cd_year, progress_bar = FALSE
)

# The district-number column is named per congress: CD119FP, CD118FP, ...
# Detect it instead of hard-coding, so changing cd_year just works.
cd_field <- grep("^CD\\d+FP$", names(cd_geo), value = TRUE)[1]
stopifnot(!is.na(cd_field))

# State FIPS -> USPS abbreviation crosswalk (ships with tigris).
st_xwalk <- tigris::fips_codes %>%
  distinct(state_code, state) %>%          # state = "ME", state_code = "23"
  rename(STATEFP = state_code, st_abbr = state)

cd_geo <- cd_geo %>%
  left_join(st_xwalk, by = "STATEFP") %>%
  mutate(cd_digits = .data[[cd_field]],
         cd_key    = paste0(st_abbr, "-", cd_digits)) %>%
  filter(cd_digits != "ZZ")                # drop "unassigned area" slivers
                                           # (CT, IL, NH carry these in 2024)

if (drop_territories) {
  cd_geo <- cd_geo %>% filter(as.integer(STATEFP) <= 56)  # PR=72, VI=78, etc.
}

## ---- 3b. Reconcile single-district ("at-large") states --------------------
# Census codes a single-seat state's district as "00" (DC's delegate as "98").
# Your data might say "AK-01" or "AK-AL". Remap any single-record state in your
# counts to whatever code the geometry actually uses, derived from the geometry
# itself so no hard-coded list goes stale with the next apportionment.
single_cd <- cd_geo %>%
  st_drop_geometry() %>%
  add_count(st_abbr, name = "n_cd") %>%
  filter(n_cd == 1) %>%
  distinct(st_abbr, cd_digits)

counts <- counts %>%
  mutate(st = sub("-.*$", "", cd_key)) %>%
  left_join(single_cd, by = c("st" = "st_abbr")) %>%
  mutate(cd_key = ifelse(!is.na(cd_digits), paste0(st, "-", cd_digits), cd_key)) %>%
  select(-st, -cd_digits) %>%
  group_by(cd_key) %>%
  summarise(n_cities = sum(n_cities), .groups = "drop")

## ---- 4. Join counts to geometry + diagnostics -----------------------------
cd_map <- cd_geo %>% left_join(counts, by = "cd_key")

# Anything in your data that found no polygon? (bad code, wrong year vintage,
# territory you dropped, at-large coding this script didn't catch.)
unmatched <- setdiff(counts$cd_key, cd_geo$cd_key)
if (length(unmatched) > 0) {
  message("WARNING: district codes in your data with no matching geometry:\n  ",
          paste(sort(unmatched), collapse = ", "),
          "\n  -> check the cd_year vintage and at-large / territory coding.")
}

if (missing_is_zero) {
  cd_map <- cd_map %>% mutate(n_cities = replace_na(n_cities, 0))
}

## ---- 5. Reposition Alaska & Hawaii, then plot -----------------------------
# shift_geometry() moves AK/HI beneath the lower 48 and returns an equal-area
# CRS (US National Atlas / EPSG:5070) suited to a national choropleth.
cd_map <- shift_geometry(cd_map)

p <- ggplot(cd_map) +
  geom_sf(aes(fill = n_cities), color = "white", linewidth = 0.05) +
  # mako runs light -> dark; direction = -1 makes MORE cities = DARKER.
  # Simpler alternative: scale_fill_gradient(low = "grey90", high = "navy").
  scale_fill_viridis_c(
    option = "mako", direction = -1,
    na.value = "grey85", name = "Cities per\ndistrict"
  ) +
  labs(
    title   = "Cities per congressional district",
    caption = "Geometry: U.S. Census Bureau via tigris (119th Congress)."
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5),
    plot.caption    = element_text(color = "grey40"),
    legend.position = "right"
  )

print(p)

ggsave("cities_per_cd.png", p, width = 11, height = 7, dpi = 300, bg = "white")
