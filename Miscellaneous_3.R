# Load required libraries
library(ggplot2)
library(sf)
library(dplyr)
library(scales)
library(viridis)

# Example: Create sample data (replace with your actual data)
# If you have your own sf object with geometries, use that instead
# This example uses the rnaturalearth package data
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
library(rnaturalearth)
library(rnaturalearthdata)

# Get world map geometries
world <- ne_countries(scale = "medium", returnclass = "sf")

# Create sample measure data (replace with your actual measure)
set.seed(123)
measure_data <- data.frame(
  iso_a3 = world$iso_a3,
  measure_value = rnorm(nrow(world), mean = 0, sd = 2)
)

# Join measure data with spatial data
world_data <- world %>%
  left_join(measure_data, by = "iso_a3")

# Find the maximum absolute value for symmetric scale
max_abs_value <- max(abs(world_data$measure_value), na.rm = TRUE)

# Create the map with diverging color scale (red to green)
map_diverging <- ggplot(data = world_data) +
  geom_sf(aes(fill = measure_value), 
          color = "white", 
          size = 0.1) +
  
  # Use a diverging color scale from red (negative) to green (positive)
  scale_fill_gradient2(
    low = "#D73027",      # Red for negative values
    mid = "#FFFFBF",      # Light yellow for values near zero
    high = "#1A9850",     # Green for positive values
    midpoint = 0,         # Center the scale at zero
    limits = c(-max_abs_value, max_abs_value),  # Symmetric limits
    breaks = pretty_breaks(n = 7),  # Create nice break points
    labels = number_format(accuracy = 0.1),  # Format labels
    name = "Measure\nValue",  # Legend title
    guide = guide_colorbar(
      barwidth = 1.5,
      barheight = 15,
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = 0.5,
      ticks.colour = "black",
      ticks.linewidth = 0.5,
      frame.colour = "black",
      frame.linewidth = 0.5
    )
  ) +
  
  # Add labels and theme
  labs(
    title = "Global Distribution of Your Measure",
    subtitle = "Negative values (red) to Positive values (green)",
    caption = "Data source: Your data source here"
  ) +
  
  # Customize theme
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid = element_line(color = "gray95"),
    panel.background = element_rect(fill = "aliceblue"),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  
  # Set coordinate system
  coord_sf(crs = 4326)  # WGS84 projection

# Display the map
print(map_diverging)

# Alternative: Using viridis-style diverging palette
map_viridis_diverging <- ggplot(data = world_data) +
  geom_sf(aes(fill = measure_value), 
          color = "white", 
          size = 0.1) +
  
  # Use viridis-inspired diverging scale
  scale_fill_gradientn(
    colors = c("#440154", "#31688E", "#35B779", "#FDE724"),  # Viridis colors
    values = rescale(c(-max_abs_value, -max_abs_value/3, max_abs_value/3, max_abs_value)),
    limits = c(-max_abs_value, max_abs_value),
    breaks = pretty_breaks(n = 7),
    labels = number_format(accuracy = 0.1),
    name = "Measure\nValue",
    guide = guide_colorbar(
      barwidth = 1.5,
      barheight = 15,
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = 0.5,
      ticks.colour = "black",
      ticks.linewidth = 0.5,
      frame.colour = "black",
      frame.linewidth = 0.5
    )
  ) +
  
  labs(
    title = "Global Distribution of Your Measure",
    subtitle = "Viridis-style diverging color scale",
    caption = "Data source: Your data source here"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50"),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid = element_line(color = "gray95"),
    panel.background = element_rect(fill = "aliceblue"),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  
  coord_sf(crs = 4326)

# Display the alternative map
print(map_viridis_diverging)

# Save the maps (optional)
# ggsave("map_diverging_redgreen.png", plot = map_diverging, width = 12, height = 8, dpi = 300)
# ggsave("map_diverging_viridis.png", plot = map_viridis_diverging, width = 12, height = 8, dpi = 300)

# If you have your own data, replace the world_data creation section with:
# world_data <- your_sf_object %>%
#   mutate(measure_value = your_measure_column)

# Additional customization options:

# 1. For a more pronounced diverging scale with white at zero:
map_pronounced <- ggplot(data = world_data) +
  geom_sf(aes(fill = measure_value), 
          color = "gray30", 
          size = 0.2) +
  
  scale_fill_gradient2(
    low = scales::muted("red"),
    mid = "white",
    high = scales::muted("green"),
    midpoint = 0,
    limits = c(-max_abs_value, max_abs_value),
    breaks = seq(-ceiling(max_abs_value), ceiling(max_abs_value), by = 1),
    labels = number_format(accuracy = 0.1),
    name = "Measure Value",
    guide = guide_colorbar(
      barwidth = 2,
      barheight = 20,
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  
  labs(
    title = "Your Analysis Title Here",
    subtitle = "Clear diverging scale with white midpoint",
    caption = "Note: Gray countries indicate missing data"
  ) +
  
  theme_void() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray40", margin = margin(b = 10)),
    plot.caption = element_text(size = 10, color = "gray50", margin = margin(t = 10)),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(10, 10, 10, 10)
  )

print(map_pronounced)

# 2. Focus on a specific region (e.g., Europe)
europe_bbox <- c(xmin = -10, xmax = 40, ymin = 35, ymax = 70)
map_europe <- map_diverging +
  coord_sf(xlim = c(europe_bbox["xmin"], europe_bbox["xmax"]),
           ylim = c(europe_bbox["ymin"], europe_bbox["ymax"]),
           expand = FALSE) +
  labs(title = "European Distribution of Your Measure")

print(map_europe)
