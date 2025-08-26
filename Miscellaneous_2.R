# Load required libraries
library(shiny)
library(plotly)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)

# Assuming your data is already loaded as:
# car_data: data frame with columns like location_id, car_id, value, etc.
# shapefile_data: sf object with location boundaries

# Function to merge car data with shapefile
prepare_map_data <- function(shapefile_data, car_data, selected_car) {
  # Filter data for selected car
  filtered_data <- car_data %>%
    filter(car_id == selected_car)
  
  # Merge with shapefile (adjust column names as needed)
  merged_data <- shapefile_data %>%
    left_join(filtered_data, by = c("location_name" = "location_id")) # Adjust column names
  
  return(merged_data)
}

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Car Value Map"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select car
      selectInput("selected_car",
                  "Select Car:",
                  choices = unique(car_data$car_id),
                  selected = unique(car_data$car_id)[1]),
      
      # Optional: Add value range filter
      sliderInput("value_range",
                  "Value Range:",
                  min = min(car_data$value, na.rm = TRUE),
                  max = max(car_data$value, na.rm = TRUE),
                  value = c(min(car_data$value, na.rm = TRUE),
                           max(car_data$value, na.rm = TRUE)),
                  step = 1000),
      
      # Additional plotly-specific options
      selectInput("color_scheme",
                  "Color Scheme:",
                  choices = c("viridis", "plasma", "inferno", "magma", "cividis"),
                  selected = "viridis"),
      
      checkboxInput("show_borders",
                   "Show Location Borders",
                   value = TRUE),
      
      # Legend information
      h4("Instructions"),
      p("• Hover over regions for details"),
      p("• Use mouse wheel to zoom"),
      p("• Click and drag to pan"),
      p("• Double-click to reset zoom"),
      
      textOutput("selected_info"),
      br(),
      textOutput("data_summary")
    ),
    
    mainPanel(
      plotlyOutput("plotly_map", height = "600px")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive data based on user selection
  map_data <- reactive({
    data <- prepare_map_data(shapefile_data, car_data, input$selected_car)
    
    # Filter by value range
    data <- data %>%
      filter(value >= input$value_range[1] & value <= input$value_range[2])
    
    return(data)
  })
  
  # Create the plotly map
  output$plotly_map <- renderPlotly({
    data <- map_data()
    
    if (nrow(data) > 0 && any(!is.na(data$value))) {
      # Create the base ggplot
      p <- ggplot(data) +
        geom_sf(aes(fill = value, 
                   text = paste0("Location: ", location_name,
                               "<br>Car: ", input$selected_car,
                               "<br>Value: $", format(round(value, 0), big.mark = ",", scientific = FALSE))),
               color = if(input$show_borders) "white" else NA, 
               size = if(input$show_borders) 0.3 else 0) +
        scale_fill_viridis_c(name = "Car Value ($)", 
                           na.value = "grey90",
                           option = input$color_scheme,
                           labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
        theme_void() +
        theme(
          legend.position = "right",
          legend.title = element_text(size = 10),
          plot.title = element_text(size = 14, hjust = 0.5, margin = margin(b = 20))
        ) +
        labs(title = paste("Car Values:", input$selected_car))
      
      # Convert to plotly with custom configuration
      ggplotly(p, tooltip = "text") %>%
        config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"),
          displaylogo = FALSE
        ) %>%
        layout(
          hoverlabel = list(
            bgcolor = "white",
            bordercolor = "black",
            font = list(family = "Arial", size = 12)
          )
        )
    } else {
      # Handle case with no data
      plotly_empty() %>%
        layout(
          title = list(
            text = "No data available for selected filters",
            font = list(size = 16)
          ),
          showlegend = FALSE
        )
    }
  })
  
  # Display selected car info
  output$selected_info <- renderText({
    paste("Currently showing:", input$selected_car)
  })
  
  # Display data summary
  output$data_summary <- renderText({
    data <- map_data()
    if (nrow(data) > 0) {
      valid_data <- data[!is.na(data$value), ]
      if (nrow(valid_data) > 0) {
        paste0("Locations displayed: ", nrow(valid_data),
               "\nValue range: $", format(min(valid_data$value, na.rm = TRUE), big.mark = ","),
               " - $", format(max(valid_data$value, na.rm = TRUE), big.mark = ","))
      } else {
        "No valid data for current selection"
      }
    } else {
      "No data matches current filters"
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

# Helper function to check data compatibility
check_data_structure <- function(car_data, shapefile_data) {
  cat("Car data structure:\n")
  str(car_data)
  cat("\n\nShapefile data structure:\n")
  str(shapefile_data)
  cat("\n\nUnique cars:", length(unique(car_data$car_id)))
  cat("\nUnique locations in car data:", length(unique(car_data$location_id)))
  cat("\nUnique locations in shapefile:", nrow(shapefile_data))
  
  # Check for matching locations
  car_locations <- unique(car_data$location_id)
  shape_locations <- shapefile_data$location_name # Adjust column name as needed
  
  missing_in_shapefile <- setdiff(car_locations, shape_locations)
  missing_in_car_data <- setdiff(shape_locations, car_locations)
  
  if(length(missing_in_shapefile) > 0) {
    cat("\nLocations in car data but not in shapefile:")
    print(missing_in_shapefile)
  }
  
  if(length(missing_in_car_data) > 0) {
    cat("\nLocations in shapefile but not in car data:")
    print(missing_in_car_data)
  }
}
