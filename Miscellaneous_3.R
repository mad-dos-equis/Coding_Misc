# Create the map with customized styling
map_diverging <- ggplot() +
  geom_sf(data = world_data,
          aes(fill = measure_value), 
          color = "gray30",        # Change outline color
          linewidth = 0.3) +       # Change outline thickness
  
  scale_fill_gradient2(
    low = "#D73027",              # Red for negative values
    mid = "#FFFFBF",              # Light yellow for values near zero
    high = "#1A9850",             # Green for positive values
    midpoint = 0,                 # Center the scale at zero
    limits = c(-max_abs_value, max_abs_value),  # Symmetric limits
    breaks = c(-max_abs_value, seq(-ceiling(max_abs_value/2), ceiling(max_abs_value/2), length.out = 5), max_abs_value),  # Include min/max
    labels = number_format(accuracy = 0.1),
    name = "Measure Value",       # Simplified for horizontal layout
    guide = guide_colorbar(
      barwidth = 20,              # Wider for horizontal
      barheight = 0.8,            # Thinner for horizontal
      title.position = "top",     # Title above the bar
      title.hjust = 0.5,          # Center the title
      label.position = "bottom",  # Labels below the bar
      direction = "horizontal",    # Make it horizontal
      ticks.colour = "black",
      ticks.linewidth = 0.5,
      frame.colour = "black",
      frame.linewidth = 0.5
    )
  ) +
  
  labs(
    title = "Global Distribution of Your Measure",
    subtitle = "Negative values (red) to Positive values (green)",
    caption = "Data source: Your data source here"
  ) +
  
  theme_void() +  # Start with theme_void() to remove all elements
  theme(
    # Text elements
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40", margin = margin(b = 10)),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 1, margin = margin(t = 10)),
    plot.caption.position = "plot",  # Position caption relative to entire plot
    
    # Legend positioning and styling
    legend.position = "bottom",       # Move legend to bottom
    legend.justification = "center",  # Center the legend
    legend.box.margin = margin(t = 10),
    legend.title = element_text(size = 11, face = "bold", margin = margin(b = 5)),
    legend.text = element_text(size = 9),
    
    # Clean background
    panel.background = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    
    # Margins
    plot.margin = margin(10, 10, 10, 10)
  ) +
  
  coord_sf(crs = 4326,
           datum = NA)  # datum = NA removes the graticule lines and degree labels

# Display the map
print(map_diverging)
