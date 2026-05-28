library(sf)
library(tigris)
library(dplyr)
options(tigris_use_cache = TRUE)

# 1. County polygons (national, or pass state= to subset)
counties_sf <- counties(cb = TRUE, year = 2023, class = "sf") %>%
  st_transform(4326) %>%
  select(GEOID, NAME, STATEFP, COUNTYFP)   # GEOID = 5-digit county FIPS

# 2. Points -> sf (note: order is X=long, Y=lat)
pts_sf <- df1 %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# 3. Point-in-polygon join
pts_joined <- st_join(pts_sf, counties_sf, join = st_within)

# 4. Back to a plain data frame, then merge to BLS
result <- pts_joined %>%
  st_drop_geometry() %>%
  left_join(bls, by = c("GEOID" = "county_fips_col"))   # adjust BLS key
