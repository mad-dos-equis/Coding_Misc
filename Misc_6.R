# =============================================================================
# ROBUST COUNTRY × RESTAURANT-CHAIN MAP EXPORT WORKFLOW
#
# Purpose:
#   Produce one standardized map for every observed country–site combination.
#
# Examples:
#   Country A × Restaurant Chain A
#   Country A × Restaurant Chain B
#   Country B × Restaurant Chain A
#
# Recommended input structure:
#
#   1. country_boundaries
#      An sf polygon object containing complete country geometries.
#
#      Required column:
#        country
#
#   2. restaurant_locations
#      An sf point object containing restaurant locations.
#
#      Required columns:
#        country
#        site
#
#      "site" identifies the restaurant chain, such as:
#        McDonald's
#        Burger King
#        Subway
#
# Important design choice:
#   The country polygon determines the map extent and projection.
#   The restaurant locations determine which points appear on each map.
#
# This ensures that a country map is consistently framed even when a particular
# restaurant chain has only one or two locations in that country.
#
# Required packages:
#
# install.packages(c(
#   "sf",
#   "dplyr",
#   "ggplot2",
#   "purrr",
#   "stringr",
#   "tibble",
#   "ragg"
# ))
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Load packages
# -----------------------------------------------------------------------------

library(sf)
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(tibble)
library(ragg)


# -----------------------------------------------------------------------------
# 2. Global map and export settings
#
# Every saved map uses the same canvas dimensions. The geographic viewing
# window changes by country, but the output image size does not.
# -----------------------------------------------------------------------------

map_spec <- list(
  width = 10,
  height = 7,
  units = "in",
  dpi = 300,

  # Geographic whitespace around the country boundary
  padding = 0.06,

  # Restaurant-point appearance
  point_size = 2.2,
  point_alpha = 0.85,
  point_stroke = 0.25,

  # Boundary appearance
  boundary_linewidth = 0.25,

  background = "white"
)


# -----------------------------------------------------------------------------
# 3. Country-specific map overrides
#
# Most countries should work with the default settings.
#
# extent_method options:
#
#   "all"
#     Use all polygons associated with the country to calculate the extent.
#
#   "largest"
#     Use only the largest polygon to calculate the extent. This is useful when
#     remote territories would otherwise make the principal landmass very small.
#
# The complete country boundary can still be plotted when "largest" is used,
# but portions outside the selected extent will be clipped.
# -----------------------------------------------------------------------------

map_overrides <- tribble(
  ~country,           ~padding, ~extent_method,
  "Chile",                 0.04, "all",
  "France",                0.06, "largest",
  "United States",         0.04, "all",
  "New Zealand",           0.08, "all"
)


# -----------------------------------------------------------------------------
# 4. Retrieve settings for one country
#
# Countries not listed in map_overrides receive the default settings.
# -----------------------------------------------------------------------------

get_country_settings <- function(
    country_name,
    overrides = map_overrides,
    default_padding = map_spec$padding
) {

  settings <- overrides |>
    filter(.data$country == country_name)

  if (nrow(settings) == 0) {
    return(list(
      padding = default_padding,
      extent_method = "all"
    ))
  }

  list(
    padding = settings$padding[[1]],
    extent_method = settings$extent_method[[1]]
  )
}


# -----------------------------------------------------------------------------
# 5. Validate the two input datasets
# -----------------------------------------------------------------------------

validate_input_data <- function(
    country_boundaries,
    restaurant_locations,
    country_column,
    site_column
) {

  if (!inherits(country_boundaries, "sf")) {
    stop("country_boundaries must be an sf object.")
  }

  if (!inherits(restaurant_locations, "sf")) {
    stop("restaurant_locations must be an sf object.")
  }

  if (!country_column %in% names(country_boundaries)) {
    stop(
      "The country column '",
      country_column,
      "' is missing from country_boundaries."
    )
  }

  missing_location_columns <- setdiff(
    c(country_column, site_column),
    names(restaurant_locations)
  )

  if (length(missing_location_columns) > 0) {
    stop(
      "The following columns are missing from restaurant_locations: ",
      paste(missing_location_columns, collapse = ", ")
    )
  }

  if (is.na(st_crs(country_boundaries))) {
    stop("country_boundaries does not have a defined CRS.")
  }

  if (is.na(st_crs(restaurant_locations))) {
    stop("restaurant_locations does not have a defined CRS.")
  }

  boundary_types <- unique(
    as.character(st_geometry_type(country_boundaries))
  )

  acceptable_boundary_types <- c(
    "POLYGON",
    "MULTIPOLYGON",
    "GEOMETRYCOLLECTION"
  )

  if (!all(boundary_types %in% acceptable_boundary_types)) {
    warning(
      "Some country-boundary geometries are not polygons or multipolygons."
    )
  }

  invisible(TRUE)
}


# -----------------------------------------------------------------------------
# 6. Validate one country–site combination
# -----------------------------------------------------------------------------

validate_combination <- function(
    country_geometry,
    site_locations,
    country_name,
    site_name
) {

  if (nrow(country_geometry) == 0) {
    stop("No country boundary found for: ", country_name)
  }

  if (all(st_is_empty(country_geometry))) {
    stop("All country-boundary geometries are empty for: ", country_name)
  }

  if (nrow(site_locations) == 0) {
    stop(
      "No restaurant locations found for combination: ",
      country_name,
      " × ",
      site_name
    )
  }

  if (all(st_is_empty(site_locations))) {
    stop(
      "All restaurant geometries are empty for combination: ",
      country_name,
      " × ",
      site_name
    )
  }

  invisible(TRUE)
}


# -----------------------------------------------------------------------------
# 7. Clean country polygon geometry
# -----------------------------------------------------------------------------

prepare_country_boundary <- function(country_geometry) {

  country_clean <- country_geometry |>
    st_make_valid() |>
    filter(!st_is_empty(geometry))

  if (nrow(country_clean) == 0) {
    stop("No valid country geometry remains after cleaning.")
  }

  country_clean
}


# -----------------------------------------------------------------------------
# 8. Clean restaurant-location geometry
#
# This function removes empty point geometries.
#
# It does not deduplicate restaurants because multiple restaurants may occupy
# the same coordinate, such as locations within a mall or airport. Deduplicate
# earlier if duplicate observations represent data errors.
# -----------------------------------------------------------------------------

prepare_site_locations <- function(site_locations) {

  locations_clean <- site_locations |>
    filter(!st_is_empty(geometry))

  if (nrow(locations_clean) == 0) {
    stop("No valid restaurant locations remain after cleaning.")
  }

  locations_clean
}


# -----------------------------------------------------------------------------
# 9. Create a locally centered map projection
#
# A Lambert azimuthal equal-area projection is created separately for each
# country. The country boundary—not the restaurant locations—determines its
# center.
#
# Consequently, every restaurant-chain map for the same country uses the same
# projection and geographic framing.
# -----------------------------------------------------------------------------

make_local_crs <- function(country_geometry) {

  country_wgs84 <- country_geometry |>
    st_transform(4326)

  country_center <- country_wgs84 |>
    st_geometry() |>
    st_union() |>
    st_centroid()

  center_coordinates <- st_coordinates(country_center)

  center_longitude <- center_coordinates[1, "X"]
  center_latitude <- center_coordinates[1, "Y"]

  paste0(
    "+proj=laea",
    " +lat_0=", center_latitude,
    " +lon_0=", center_longitude,
    " +datum=WGS84",
    " +units=m",
    " +no_defs"
  )
}


# -----------------------------------------------------------------------------
# 10. Project the country boundary and restaurant locations together
#
# Both layers must use the same CRS before plotting.
# -----------------------------------------------------------------------------

project_map_layers <- function(
    country_geometry,
    site_locations
) {

  local_crs <- make_local_crs(country_geometry)

  list(
    country = st_transform(
      country_geometry,
      local_crs
    ),

    locations = st_transform(
      site_locations,
      local_crs
    )
  )
}


# -----------------------------------------------------------------------------
# 11. Select the largest country polygon
#
# This is used only when extent_method = "largest".
#
# It is particularly useful for countries whose remote islands or territories
# create an extremely large bounding box.
# -----------------------------------------------------------------------------

get_largest_component <- function(country_geometry) {

  country_components <- country_geometry |>
    st_geometry() |>
    st_union() |>
    st_cast("POLYGON") |>
    st_sf(geometry = _) |>
    mutate(component_area = st_area(geometry))

  country_components |>
    slice_max(
      order_by = .data$component_area,
      n = 1,
      with_ties = FALSE
    )
}


# -----------------------------------------------------------------------------
# 12. Choose the geometry that determines zoom
# -----------------------------------------------------------------------------

get_extent_geometry <- function(
    country_geometry,
    extent_method = "all"
) {

  if (extent_method == "all") {
    return(country_geometry)
  }

  if (extent_method == "largest") {
    return(get_largest_component(country_geometry))
  }

  stop(
    "Unknown extent_method: ",
    extent_method,
    ". Use either 'all' or 'largest'."
  )
}


# -----------------------------------------------------------------------------
# 13. Calculate the country-specific viewing window
#
# Restaurant points are deliberately not used here.
#
# If restaurant points determined the map extent, a chain with one location
# could create a map zoomed tightly around that restaurant. Using the country
# boundary keeps every site map within a country directly comparable.
# -----------------------------------------------------------------------------

calculate_map_view <- function(
    extent_geometry,
    padding = map_spec$padding
) {

  bbox <- st_bbox(extent_geometry)

  x_range <- unname(bbox["xmax"] - bbox["xmin"])
  y_range <- unname(bbox["ymax"] - bbox["ymin"])

  if (
    !is.finite(x_range) ||
    !is.finite(y_range) ||
    x_range <= 0 ||
    y_range <= 0
  ) {
    stop("The country geometry has an invalid bounding box.")
  }

  x_padding <- x_range * padding
  y_padding <- y_range * padding

  list(
    xlim = c(
      unname(bbox["xmin"]) - x_padding,
      unname(bbox["xmax"]) + x_padding
    ),

    ylim = c(
      unname(bbox["ymin"]) - y_padding,
      unname(bbox["ymax"]) + y_padding
    )
  )
}


# -----------------------------------------------------------------------------
# 14. Standard map theme
# -----------------------------------------------------------------------------

theme_country_site_map <- function(base_size = 11) {

  theme_void(base_size = base_size) +
    theme(
      plot.title.position = "plot",

      plot.title = element_text(
        size = 15,
        face = "bold",
        margin = margin(b = 3)
      ),

      plot.subtitle = element_text(
        size = 11,
        margin = margin(b = 8)
      ),

      plot.caption = element_text(
        size = 8,
        hjust = 0,
        margin = margin(t = 8)
      ),

      plot.margin = margin(
        t = 10,
        r = 10,
        b = 10,
        l = 10
      )
    )
}


# -----------------------------------------------------------------------------
# 15. Construct one country–site map
#
# The output title and subtitle can be adjusted to match your report style.
#
# The location count is calculated after filtering to the relevant combination.
# -----------------------------------------------------------------------------

make_country_site_map <- function(
    country_geometry,
    site_locations,
    country_name,
    site_name,
    padding = map_spec$padding,
    extent_method = "all",
    specification = map_spec
) {

  validate_combination(
    country_geometry = country_geometry,
    site_locations = site_locations,
    country_name = country_name,
    site_name = site_name
  )

  country_clean <- prepare_country_boundary(country_geometry)
  locations_clean <- prepare_site_locations(site_locations)

  projected_layers <- project_map_layers(
    country_geometry = country_clean,
    site_locations = locations_clean
  )

  country_projected <- projected_layers$country
  locations_projected <- projected_layers$locations

  extent_geometry <- get_extent_geometry(
    country_geometry = country_projected,
    extent_method = extent_method
  )

  map_view <- calculate_map_view(
    extent_geometry = extent_geometry,
    padding = padding
  )

  location_count <- nrow(locations_projected)

  ggplot() +

    # Country background
    geom_sf(
      data = country_projected,
      fill = "grey95",
      color = "grey45",
      linewidth = specification$boundary_linewidth
    ) +

    # Restaurant locations for the selected chain
    geom_sf(
      data = locations_projected,
      shape = 21,
      fill = "black",
      color = "white",
      size = specification$point_size,
      alpha = specification$point_alpha,
      stroke = specification$point_stroke
    ) +

    # Use the country boundary to define the map view
    coord_sf(
      xlim = map_view$xlim,
      ylim = map_view$ylim,
      expand = FALSE,
      datum = NA
    ) +

    labs(
      title = site_name,
      subtitle = paste0(
        country_name,
        " — ",
        format(location_count, big.mark = ","),
        ifelse(location_count == 1, " location", " locations")
      )
    ) +

    theme_country_site_map()
}


# -----------------------------------------------------------------------------
# 16. Create filesystem-safe names
# -----------------------------------------------------------------------------

make_safe_filename <- function(x) {

  x |>
    str_replace_all("[^A-Za-z0-9]+", "_") |>
    str_remove("^_") |>
    str_remove("_$") |>
    str_to_lower()
}


# -----------------------------------------------------------------------------
# 17. Save one country–site map
#
# Directory structure:
#
# output/maps/
#   country_a/
#     restaurant_chain_a.png
#     restaurant_chain_b.png
#
#   country_b/
#     restaurant_chain_a.png
#
# Organizing files by country is easier to maintain than placing every map in
# one directory.
# -----------------------------------------------------------------------------

save_country_site_map <- function(
    plot,
    country_name,
    site_name,
    output_directory = "output/maps",
    specification = map_spec
) {

  safe_country_name <- make_safe_filename(country_name)
  safe_site_name <- make_safe_filename(site_name)

  country_directory <- file.path(
    output_directory,
    safe_country_name
  )

  dir.create(
    country_directory,
    recursive = TRUE,
    showWarnings = FALSE
  )

  output_file <- file.path(
    country_directory,
    paste0(safe_site_name, ".png")
  )

  ggsave(
    filename = output_file,
    plot = plot,
    width = specification$width,
    height = specification$height,
    units = specification$units,
    dpi = specification$dpi,
    device = ragg::agg_png,
    bg = specification$background,
    limitsize = FALSE
  )

  output_file
}


# -----------------------------------------------------------------------------
# 18. Process one country–site combination
#
# This is the equivalent of one iteration of the nested loop.
# -----------------------------------------------------------------------------

process_country_site_map <- function(
    country_name,
    site_name,
    country_boundaries,
    restaurant_locations,
    country_column,
    site_column,
    output_directory = "output/maps"
) {

  # Select the complete boundary for the current country
  country_geometry <- country_boundaries |>
    filter(.data[[country_column]] == country_name)

  # Select restaurant locations for the current country and current site
  site_locations <- restaurant_locations |>
    filter(
      .data[[country_column]] == country_name,
      .data[[site_column]] == site_name
    )

  country_settings <- get_country_settings(country_name)

  country_site_plot <- make_country_site_map(
    country_geometry = country_geometry,
    site_locations = site_locations,
    country_name = country_name,
    site_name = site_name,
    padding = country_settings$padding,
    extent_method = country_settings$extent_method
  )

  output_file <- save_country_site_map(
    plot = country_site_plot,
    country_name = country_name,
    site_name = site_name,
    output_directory = output_directory
  )

  tibble(
    country = country_name,
    site = site_name,
    location_count = nrow(site_locations),
    status = "success",
    output_file = output_file,
    error_message = NA_character_
  )
}


# -----------------------------------------------------------------------------
# 19. Export every observed country–site combination
#
# Instead of writing two explicit for-loops, this function:
#
#   1. Finds the distinct country–site combinations in restaurant_locations.
#   2. Runs process_country_site_map() once for each combination.
#
# This is logically equivalent to a nested loop, but it avoids attempting
# combinations that do not exist in the data.
#
# For example, if Restaurant Chain C has no locations in Country B, the
# workflow will not generate an empty Country B × Chain C map.
# -----------------------------------------------------------------------------

export_all_country_site_maps <- function(
    country_boundaries,
    restaurant_locations,
    country_column = "country",
    site_column = "site",
    output_directory = "output/maps"
) {

  validate_input_data(
    country_boundaries = country_boundaries,
    restaurant_locations = restaurant_locations,
    country_column = country_column,
    site_column = site_column
  )

  # Build the list of observed country–site combinations.
  #
  # Sorting makes export order deterministic across runs.
  combinations <- restaurant_locations |>
    st_drop_geometry() |>
    transmute(
      country = .data[[country_column]],
      site = .data[[site_column]]
    ) |>
    filter(
      !is.na(.data$country),
      !is.na(.data$site)
    ) |>
    distinct(.data$country, .data$site) |>
    arrange(.data$country, .data$site)

  if (nrow(combinations) == 0) {
    stop("No valid country–site combinations were found.")
  }

  # safely() captures errors so one problematic combination does not stop the
  # remaining maps from being generated.
  safe_process_combination <- safely(
    process_country_site_map,
    otherwise = NULL
  )

  raw_results <- pmap(
    combinations,
    function(country, site) {
      safe_process_combination(
        country_name = country,
        site_name = site,
        country_boundaries = country_boundaries,
        restaurant_locations = restaurant_locations,
        country_column = country_column,
        site_column = site_column,
        output_directory = output_directory
      )
    }
  )

  # Convert successful and failed operations into one results table.
  results_log <- map2_dfr(
    raw_results,
    seq_len(nrow(combinations)),
    function(result, row_number) {

      country_name <- combinations$country[[row_number]]
      site_name <- combinations$site[[row_number]]

      if (is.null(result$error)) {
        return(result$result)
      }

      tibble(
        country = country_name,
        site = site_name,
        location_count = NA_integer_,
        status = "failed",
        output_file = NA_character_,
        error_message = conditionMessage(result$error)
      )
    }
  )

  dir.create(
    output_directory,
    recursive = TRUE,
    showWarnings = FALSE
  )

  # Write an overall export log.
  log_file <- file.path(
    output_directory,
    "country_site_map_export_log.csv"
  )

  write.csv(
    results_log,
    file = log_file,
    row.names = FALSE
  )

  results_log
}


# =============================================================================
# EXAMPLE USAGE
# =============================================================================

# Assume you have:
#
#   country_boundaries
#     An sf polygon object with a column named "country".
#
#   restaurant_locations
#     An sf point object with:
#       country
#       site
#       geometry
#
# Each restaurant location would ordinarily occupy one row:
#
# country          site                 geometry
# United States    Restaurant Chain A   POINT(...)
# United States    Restaurant Chain A   POINT(...)
# United States    Restaurant Chain B   POINT(...)
# Canada           Restaurant Chain A   POINT(...)
#
#
# Run the complete export:
#
# map_results <- export_all_country_site_maps(
#   country_boundaries = country_boundaries,
#   restaurant_locations = restaurant_locations,
#   country_column = "country",
#   site_column = "site",
#   output_directory = "output/country_site_maps"
# )
#
#
# Review the results:
#
# print(map_results)
#
#
# Review failures:
#
# map_results |>
#   filter(status == "failed")
#
#
# Review successful exports:
#
# map_results |>
#   filter(status == "success")
#
#
# Output structure:
#
# output/country_site_maps/
#   canada/
#     restaurant_chain_a.png
#
#   united_states/
#     restaurant_chain_a.png
#     restaurant_chain_b.png
#
#   country_site_map_export_log.csv
# =============================================================================
