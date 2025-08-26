# Quick test to verify data setup - RUN THIS FIRST
test_data_setup <- function() {
  cat("=== TESTING DATA SETUP ===\n")
  
  # Check if data exists
  if(!exists("car_data")) {
    cat("ERROR: car_data not found\n")
    return(FALSE)
  }
  if(!exists("shapefile_data")) {
    cat("ERROR: shapefile_data not found\n")
    return(FALSE)
  }
  
  cat("✓ Both datasets exist\n")
  
  # Check required columns
  missing_cols <- setdiff(c(CAR_LOCATION_COL, CAR_ID_COL, MEASUREMENT_COLS), colnames(car_data))
  if(length(missing_cols) > 0) {
    cat("ERROR: Missing columns in car_data:", paste(missing_cols, collapse = ", "), "\n")
    cat("Available columns:", paste(colnames(car_data), collapse = ", "), "\n")
    return(FALSE)
  }
  
  if(!SHAPEFILE_LOCATION_COL %in% colnames(shapefile_data)) {
    cat("ERROR: Missing column in shapefile_data:", SHAPEFILE_LOCATION_COL, "\n")
    cat("Available columns:", paste(colnames(shapefile_data), collapse = ", "), "\n")
    return(FALSE)
  }
  
  cat("✓ All required columns exist\n")
  
  # Check for column name conflicts between datasets
  car_cols <- colnames(car_data)
  shape_cols <- colnames(shapefile_data)
  conflicts <- intersect(car_cols, shape_cols)
  conflicts <- conflicts[!conflicts %in% c(CAR_LOCATION_COL, SHAPEFILE_LOCATION_COL)]  # Exclude join columns
  
  if(length(conflicts) > 0) {
    cat("WARNING: Column name conflicts found:", paste(conflicts, collapse = ", "), "\n")
    cat("These columns exist in both datasets and may cause join issues\n")
  }
  
  # Test data joining
  test_car <- unique(car_data[[CAR_ID_COL]])[1]
  cat("Testing with car:", test_car, "\n")
  
  # Get locations from both datasets
  car_locs <- unique(car_data[car_data[[CAR_ID_COL]] == test_car, CAR_LOCATION_COL])
  shape_locs <- unique(shapefile_data[[SHAPEFILE_LOCATION_COL]])
  
  cat("Car data locations (first 10):", paste(head(car_locs, 10), collapse = ", "), "\n")
  cat("Shapefile locations (first 10):", paste(head(shape_locs, 10), collapse = ", "), "\n")
  
  # Check for exact matches
  matches <- intersect(car_locs, shape_locs)
  cat("Exact matching locations:", length(matches), "out of", length(car_locs), "car locations\n")
  
  if(length(matches) == 0) {
    cat("ERROR: No matching locations found between datasets!\n")
    cat("This is likely the cause of the 'no data available' message.\n")
    
    # Try to find similar names
    cat("Trying fuzzy matching to find similar names...\n")
    for(car_loc in head(car_locs, 5)) {
      distances <- sapply(shape_locs, function(s) {
        # Simple character distance check
        sum(strsplit(tolower(car_loc), "")[[1]] %in% strsplit(tolower(s), "")[[1]])
      })
      best_match <- shape_locs[which.max(distances)]
      cat("  '", car_loc, "' might match '", best_match, "'\n")
    }
    return(FALSE)
  }
  
  # Test the actual merge
  cat("Testing data merge...\n")
  test_merged <- prepare_map_data(shapefile_data, car_data, test_car)
  
  # Check that measurement columns made it through
  cat("Checking measurement columns in merged data...\n")
  for(col in MEASUREMENT_COLS) {
    if(col %in% colnames(test_merged)) {
      valid_count <- sum(!is.na(test_merged[[col]]))
      cat("  ✓", col, ":", valid_count, "valid values\n")
    } else {
      cat("  ✗", col, ": MISSING from merged data\n")
    }
  }
  
  # Final check
  measurement_columns_present <- all(MEASUREMENT_COLS %in% colnames(test_merged))
  any_valid_data <- any(sapply(MEASUREMENT_COLS, function(col) {
    if(col %in% colnames(test_merged)) sum(!is.na(test_merged[[col]])) > 0 else FALSE
  }))
  
  if(!measurement_columns_present) {
    cat("ERROR: Some measurement columns missing from merged data\n")
    return(FALSE)
  }
  
  if(!any_valid_data) {
    cat("ERROR: No valid measurement data after merge\n")
    return(FALSE)
  }
  
  cat("✓ Data setup appears to be working!\n")
  cat("Ready to run the Shiny app\n")
  return(TRUE)
}# Helper functions for debugging
debug_shapefile <- function(shapefile_data) {
  cat("=== SHAPEFILE DIAGNOSTICS ===\n")
  cat("Class:", class(shapefile_data), "\n")
  cat("Dimensions:", nrow(shapefile_data), "rows,", ncol(shapefile_data), "columns\n")
  cat("CRS:", st_crs(shapefile_data)$input, "\n")
  cat("Geometry type:", unique(st_geometry_type(shapefile_data)), "\n")
  
  # Check bounds
  bbox <- st_bbox(shapefile_data)
  cat("Bounding box:\n")
  print(bbox)
  
  # Check for invalid geometries
  valid_geoms <- st_is_valid(shapefile_data)
  if(!all(valid_geoms)) {
    cat("WARNING: Invalid geometries found in", sum(!valid_geoms), "features\n")
  }
  
  # Show first few rows
  cat("\nFirst few rows of data:\n")
  print(head(as.data.frame(shapefile_data)))
  
  cat("\nColumn names:\n")
  print(colnames(shapefile_data))
}

debug_car_data <- function(car_data) {
  cat("\n=== CAR DATA DIAGNOSTICS ===\n")
  cat("Class:", class(car_data), "\n")
  cat("Dimensions:", nrow(car_data), "rows,", ncol(car_data), "columns\n")
  cat("Column names:\n")
  print(colnames(car_data))
  
  cat("\nFirst few rows:\n")
  print(head(car_data))
  
  # Check measurement columns
  cat("\nMeasurement columns summary:\n")
  for(col in intersect(MEASUREMENT_COLS, colnames(car_data))) {
    values <- car_data[[col]]
    cat(col, ": min =", min(values, na.rm = TRUE), 
        ", max =", max(values, na.rm = TRUE),
        ", NA =", sum(is.na(values)), "\n")
  }
}

# Quick test function to create a simple static plot
test_static_plot <- function(shapefile_data, car_data, test_car = NULL, test_measurement = DEFAULT_MEASUREMENT) {
  if(is.null(test_car)) {
    test_car <- unique(car_data[[CAR_ID_COL]])[1]
  }
  
  cat("Testing with car:", test_car, "and measurement:", test_measurement, "\n")
  
  # Prepare data
  test_data <- prepare_map_data(shapefile_data, car_data, test_car)
  
  cat("Merged data dimensions:", nrow(test_data), "x", ncol(test_data), "\n")
  cat("Non-NA values:", sum(!is.na(test_data[[test_measurement]])), "\n")
  
  if(nrow(test_data) > 0 && any(!is.na(test_data[[test_measurement]]))) {
    # Create simple ggplot
    p <- ggplot(test_data) +
      geom_sf(aes(fill = .data[[test_measurement]]), color = "white", size = 0.2) +
      scale_fill_viridis_c() +
      theme_void() +
      labs(title = paste("Test plot:", test_measurement, "for", test_car))
    
    print(p)
    return(test_data)
  } else {
    cat("ERROR: No valid data for plotting\n")
    return(test_data)
  }
}# Load required libraries
library(shiny)
library(plotly)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)

# =============================================================================
# CONFIGURATION SECTION - UPDATE THESE TO MATCH YOUR DATA
# =============================================================================

# Data source names (update these to match your actual data objects)
CAR_DATA_NAME <- "car_data"           # Name of your car dataset
SHAPEFILE_DATA_NAME <- "shapefile_data"  # Name of your shapefile data

# Column names in your car dataset
CAR_LOCATION_COL <- "location_id"     # Column with location identifiers
CAR_ID_COL <- "car_id"               # Column with car identifiers  

# Measurement columns (all numeric columns you want to visualize)
# *** REPLACE THESE WITH YOUR ACTUAL COLUMN NAMES ***
MEASUREMENT_COLS <- c("value", "price", "mileage", "age", "maintenance_cost")

# Display names for better UI (optional) - format: actual_column_name = "Display Name"
MEASUREMENT_DISPLAY_NAMES <- c(
  "value" = "Car Value ($)",
  "price" = "Purchase Price ($)", 
  "mileage" = "Mileage (miles)",
  "age" = "Age (years)",
  "maintenance_cost" = "Maintenance Cost ($)"
)

# Default measurement to show on startup - must match one of MEASUREMENT_COLS
DEFAULT_MEASUREMENT <- "value"

# Column names in your shapefile dataset
SHAPEFILE_LOCATION_COL <- "location_name"  # Column with location identifiers (should match car data locations)

# Load your actual data (replace with your actual data loading)
car_data <- get(CAR_DATA_NAME)           # Your car dataset
shapefile_data <- get(SHAPEFILE_DATA_NAME)  # Your shapefile data

# Optional: Set limits for performance (set to NULL for no limits)
MAX_CARS_FOR_DROPDOWN <- 1000  # If more cars than this, use server-side selectize
MAX_LOCATIONS_FOR_SLIDER <- 10000  # If more locations than this, disable slider

# Validate that default measurement exists
if(!DEFAULT_MEASUREMENT %in% MEASUREMENT_COLS) {
  stop("DEFAULT_MEASUREMENT must be one of the columns listed in MEASUREMENT_COLS")
}

# Create display names if not provided
if(!exists("MEASUREMENT_DISPLAY_NAMES") || is.null(MEASUREMENT_DISPLAY_NAMES)) {
  MEASUREMENT_DISPLAY_NAMES <- setNames(MEASUREMENT_COLS, MEASUREMENT_COLS)
} else {
  # Fill in any missing display names
  missing_names <- setdiff(MEASUREMENT_COLS, names(MEASUREMENT_DISPLAY_NAMES))
  if(length(missing_names) > 0) {
    MEASUREMENT_DISPLAY_NAMES <- c(MEASUREMENT_DISPLAY_NAMES, setNames(missing_names, missing_names))
  }
}

# =============================================================================
# END CONFIGURATION SECTION
# =============================================================================

# Function to merge car data with shapefile
prepare_map_data <- function(shapefile_data, car_data, selected_car) {
  cat("=== PREPARE_MAP_DATA DEBUG ===\n")
  cat("Selected car:", selected_car, "\n")
  
  # Filter data for selected car using parameterized column names
  filtered_data <- car_data %>%
    filter(.data[[CAR_ID_COL]] == selected_car)
  
  cat("Filtered car data rows:", nrow(filtered_data), "\n")
  cat("Columns in filtered data:", paste(colnames(filtered_data), collapse = ", "), "\n")
  
  if(nrow(filtered_data) == 0) {
    cat("ERROR: No data found for car:", selected_car, "\n")
    cat("Available cars:", paste(head(unique(car_data[[CAR_ID_COL]]), 10), collapse = ", "), "\n")
    
    # Return shapefile with empty measurement columns
    result <- shapefile_data
    for(col in MEASUREMENT_COLS) {
      result[[col]] <- NA_real_
    }
    return(result)
  }
  
  # Show sample of locations from both datasets
  car_locations <- unique(filtered_data[[CAR_LOCATION_COL]])
  shape_locations <- unique(shapefile_data[[SHAPEFILE_LOCATION_COL]])
  
  cat("Sample car locations:", paste(head(car_locations, 5), collapse = ", "), "\n")
  cat("Sample shapefile locations:", paste(head(shape_locations, 5), collapse = ", "), "\n")
  
  # Check for exact matches
  matching_locations <- intersect(car_locations, shape_locations)
  cat("Matching locations found:", length(matching_locations), "\n")
  
  if(length(matching_locations) > 0) {
    cat("Example matches:", paste(head(matching_locations, 3), collapse = ", "), "\n")
  } else {
    cat("WARNING: No exact location matches found!\n")
    cat("Trying case-insensitive matching...\n")
    
    # Try case-insensitive matching
    car_locs_lower <- tolower(trimws(car_locations))
    shape_locs_lower <- tolower(trimws(shape_locations))
    case_matches <- intersect(car_locs_lower, shape_locs_lower)
    cat("Case-insensitive matches:", length(case_matches), "\n")
  }
  
  # Merge with shapefile using parameterized column names
  join_vars <- setNames(CAR_LOCATION_COL, SHAPEFILE_LOCATION_COL)
  cat("Join variables:", paste(names(join_vars), "=", join_vars), "\n")
  
  # Ensure we're selecting all needed columns from filtered_data
  columns_to_join <- c(CAR_LOCATION_COL, CAR_ID_COL, MEASUREMENT_COLS)
  existing_columns <- intersect(columns_to_join, colnames(filtered_data))
  cat("Columns being joined:", paste(existing_columns, collapse = ", "), "\n")
  
  # Select only the columns we need from filtered_data to avoid conflicts
  filtered_data_clean <- filtered_data %>%
    select(all_of(existing_columns))
  
  merged_data <- shapefile_data %>%
    left_join(filtered_data_clean, by = join_vars)
  
  cat("Merged data rows:", nrow(merged_data), "\n")
  cat("Columns in merged data:", paste(colnames(merged_data), collapse = ", "), "\n")
  
  # Check how many locations got data
  for(col in MEASUREMENT_COLS) {
    if(col %in% colnames(merged_data)) {
      non_na_count <- sum(!is.na(merged_data[[col]]))
      cat("Non-NA values in", col, ":", non_na_count, "\n")
    } else {
      cat("WARNING: Column", col, "not found in merged data\n")
    }
  }
  
  return(merged_data)
}

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Car Value Map"),
  
  sidebarLayout(
    sidebarPanel(
      # Measurement selection dropdown
      selectInput("selected_measurement",
                  "Select Measurement:",
                  choices = MEASUREMENT_DISPLAY_NAMES,
                  selected = DEFAULT_MEASUREMENT),
      
      # Car selection dropdown - always use server-side selectize for performance
      selectizeInput("selected_car",
                    "Select Car:",
                    choices = NULL,  # Populated server-side for better performance
                    options = list(
                      placeholder = "Type to search cars...",
                      maxOptions = 100,  # Show more options at once
                      searchField = c('value', 'text'),
                      render = I('{
                        option: function(item, escape) {
                          return "<div>" + escape(item.value) + "</div>";
                        }
                      }')
                    )),
      
      # Conditional value range filter
      conditionalPanel(
        condition = paste0("output.show_slider == true"),
        sliderInput("value_range",
                   "Value Range:",
                   min = 0, max = 100, value = c(0, 100))  # Will be updated by server
      ),
      
      conditionalPanel(
        condition = paste0("output.show_slider == false"),
        div(
          h5("Value Range Filter"),
          p("(Disabled for large datasets)", 
            style = "color: grey; font-style: italic;")
        )
      ),
      
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
      textOutput("data_summary"),
      br(),
      textOutput("performance_info")
    ),
    
    mainPanel(
      plotlyOutput("plotly_map", height = "600px")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Always use server-side selectize for better performance
  updateSelectizeInput(session, "selected_car", 
                      choices = unique(car_data[[CAR_ID_COL]]), 
                      selected = unique(car_data[[CAR_ID_COL]])[1],
                      server = TRUE)
  
  # Determine if slider should be shown based on data size
  output$show_slider <- reactive({
    nrow(car_data) <= MAX_LOCATIONS_FOR_SLIDER
  })
  outputOptions(output, "show_slider", suspendWhenHidden = FALSE)
  
  # Update slider range when measurement changes
  observe({
    req(input$selected_measurement)
    
    if(nrow(car_data) <= MAX_LOCATIONS_FOR_SLIDER) {
      current_col <- input$selected_measurement
      
      # Get min/max from the entire dataset for this measurement
      all_values <- car_data[[current_col]][!is.na(car_data[[current_col]])]
      
      if(length(all_values) > 0) {
        min_val <- min(all_values)
        max_val <- max(all_values)
        step_val <- max(1, round((max_val - min_val) / 1000))
        
        updateSliderInput(session, "value_range",
                         label = paste(MEASUREMENT_DISPLAY_NAMES[current_col], "Range:"),
                         min = min_val,
                         max = max_val,
                         value = c(min_val, max_val),
                         step = step_val)
      }
    }
  })
  
  # Reactive data based on user selection
  map_data <- reactive({
    req(input$selected_car, input$selected_measurement)
    
    data <- prepare_map_data(shapefile_data, car_data, input$selected_car)
    
    # Apply value range filter only if slider exists and is available
    if(nrow(car_data) <= MAX_LOCATIONS_FOR_SLIDER && !is.null(input$value_range)) {
      current_col <- input$selected_measurement
      
      if(current_col %in% colnames(data)) {
        before_filter <- nrow(data)
        data <- data %>%
          filter(is.na(.data[[current_col]]) | 
                 (.data[[current_col]] >= input$value_range[1] & 
                  .data[[current_col]] <= input$value_range[2]))
      }
    }
    
    return(data)
  })
  
  # Create the plotly map
  output$plotly_map <- renderPlotly({
    req(input$selected_measurement)
    data <- map_data()
    current_col <- input$selected_measurement
    display_name <- MEASUREMENT_DISPLAY_NAMES[current_col]
    
    if(!current_col %in% colnames(data)) {
      return(plotly_empty() %>% 
        layout(title = list(text = paste("Column", current_col, "not found"), font = list(size = 16))))
    }
    
    # Check for valid data
    valid_data_count <- sum(!is.na(data[[current_col]]))
    
    if (nrow(data) > 0 && valid_data_count > 0) {
      # Get the range of values for proper color scaling
      value_range <- range(data[[current_col]], na.rm = TRUE)
      
      # Transform to WGS84 if needed
      if(!is.na(st_crs(data)$input) && st_crs(data)$input != "EPSG:4326") {
        tryCatch({
          data <- st_transform(data, 4326)
        }, error = function(e) {
          cat("CRS transformation failed:", e$message, "\n")
        })
      }
      
      # Create the base ggplot - fix the color mapping issue
      p <- ggplot(data) +
        geom_sf(aes(fill = !!sym(current_col),  # Use !! and sym() for proper column reference
                   text = paste0("Location: ", !!sym(SHAPEFILE_LOCATION_COL),
                               "<br>Car: ", input$selected_car,
                               "<br>", display_name, ": ", 
                               if(grepl("\\$", display_name)) {
                                 paste0("$", format(round(!!sym(current_col), 0), big.mark = ",", scientific = FALSE))
                               } else {
                                 format(round(!!sym(current_col), 2), big.mark = ",", scientific = FALSE)
                               })),
               color = if(input$show_borders) "white" else NA, 
               size = if(input$show_borders) 0.3 else 0) +
        scale_fill_viridis_c(name = display_name, 
                           na.value = "grey90",
                           option = input$color_scheme,
                           limits = value_range,  # Ensure proper color scaling
                           labels = function(x) {
                             if(grepl("\\$", display_name)) {
                               paste0("$", format(x, big.mark = ",", scientific = FALSE))
                             } else {
                               format(x, big.mark = ",", scientific = FALSE)
                             }
                           }) +
        theme_void() +
        theme(
          legend.position = "right",
          legend.title = element_text(size = 10),
          plot.title = element_text(size = 14, hjust = 0.5, margin = margin(b = 20)),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()
        ) +
        labs(title = paste(display_name, "for", input$selected_car)) +
        coord_sf(expand = FALSE, crs = st_crs(4326))
      
      # Convert to plotly
      plotly_obj <- ggplotly(p, tooltip = "text") %>%
        config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d", "autoScale2d", 
                                   "hoverClosestCartesian", "hoverCompareCartesian"),
          displaylogo = FALSE
        ) %>%
        layout(
          hoverlabel = list(
            bgcolor = "white",
            bordercolor = "black",
            font = list(family = "Arial", size = 12)
          ),
          margin = list(l = 0, r = 50, t = 50, b = 0)
        )
      
      return(plotly_obj)
      
    } else {
      return(plotly_empty() %>%
        layout(
          title = list(
            text = paste("No data available: ", nrow(data), "rows,", valid_data_count, "valid values"),
            font = list(size = 16)
          ),
          showlegend = FALSE
        ))
    }
  })
  
  # Display selected car and measurement info
  output$selected_info <- renderText({
    req(input$selected_car, input$selected_measurement)
    paste("Showing:", MEASUREMENT_DISPLAY_NAMES[input$selected_measurement], "for", input$selected_car)
  })
  
  # Display data summary
  output$data_summary <- renderText({
    req(input$selected_measurement)
    data <- map_data()
    current_col <- input$selected_measurement
    display_name <- MEASUREMENT_DISPLAY_NAMES[current_col]
    
    if (nrow(data) > 0) {
      valid_data <- data[!is.na(data[[current_col]]), ]
      if (nrow(valid_data) > 0) {
        min_val <- min(valid_data[[current_col]], na.rm = TRUE)
        max_val <- max(valid_data[[current_col]], na.rm = TRUE)
        
        if(grepl("\\$", display_name)) {
          value_range <- paste0("$", format(min_val, big.mark = ","), " - $", format(max_val, big.mark = ","))
        } else {
          value_range <- paste0(format(min_val, big.mark = ","), " - ", format(max_val, big.mark = ","))
        }
        
        paste0("Locations displayed: ", nrow(valid_data),
               "\n", display_name, " range: ", value_range)
      } else {
        "No valid data for current selection"
      }
    } else {
      "No data matches current filters"
    }
  })
  
  # Display performance information
  output$performance_info <- renderText({
    total_cars <- length(unique(car_data[[CAR_ID_COL]]))
    total_rows <- nrow(car_data)
    total_measurements <- length(MEASUREMENT_COLS)
    
    info_lines <- c()
    
    if(total_cars > MAX_CARS_FOR_DROPDOWN) {
      info_lines <- c(info_lines, paste("Using server-side selectize for", total_cars, "cars"))
    }
    
    if(total_rows > MAX_LOCATIONS_FOR_SLIDER) {
      info_lines <- c(info_lines, "Value slider disabled for large dataset")
    }
    
    base_info <- paste("Dataset:", format(total_rows, big.mark = ","), "rows,", 
                      total_cars, "cars,", total_measurements, "measurements")
    
    if(length(info_lines) > 0) {
      paste(base_info, "|", paste(info_lines, collapse = "; "))
    } else {
      base_info
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
  cat("\n\nUsing column mappings:")
  cat("\n  Car location column:", CAR_LOCATION_COL)
  cat("\n  Car ID column:", CAR_ID_COL)
  cat("\n  Measurement columns:", paste(MEASUREMENT_COLS, collapse = ", "))
  cat("\n  Shapefile location column:", SHAPEFILE_LOCATION_COL)
  
  cat("\n\nData summary:")
  cat("\n  Unique cars:", length(unique(car_data[[CAR_ID_COL]])))
  cat("\n  Unique locations in car data:", length(unique(car_data[[CAR_LOCATION_COL]])))
  cat("\n  Unique locations in shapefile:", nrow(shapefile_data))
  cat("\n  Total car-location combinations:", nrow(car_data))
  cat("\n  Available measurements:", length(MEASUREMENT_COLS))
  
  # Check if measurement columns exist in the data
  missing_cols <- setdiff(MEASUREMENT_COLS, colnames(car_data))
  if(length(missing_cols) > 0) {
    cat("\n\nERROR - Missing measurement columns in car_data:")
    print(missing_cols)
  }
  
  # Show summary stats for each measurement
  cat("\n\nMeasurement column summaries:")
  for(col in intersect(MEASUREMENT_COLS, colnames(car_data))) {
    col_data <- car_data[[col]]
    cat("\n  ", col, ":")
    cat(" min =", round(min(col_data, na.rm = TRUE), 2),
        ", max =", round(max(col_data, na.rm = TRUE), 2),
        ", NA count =", sum(is.na(col_data)))
  }
  
  # Check for matching locations
  car_locations <- unique(car_data[[CAR_LOCATION_COL]])
  shape_locations <- unique(shapefile_data[[SHAPEFILE_LOCATION_COL]])
  
  missing_in_shapefile <- setdiff(car_locations, shape_locations)
  missing_in_car_data <- setdiff(shape_locations, car_locations)
  
  if(length(missing_in_shapefile) > 0) {
    cat("\n\nWARNING - Locations in car data but not in shapefile:")
    print(head(missing_in_shapefile, 10))  # Show first 10
    if(length(missing_in_shapefile) > 10) {
      cat("... and", length(missing_in_shapefile) - 10, "more")
    }
  }
  
  if(length(missing_in_car_data) > 0) {
    cat("\n\nINFO - Locations in shapefile but not in car data:")
    print(head(missing_in_car_data, 10))  # Show first 10
    if(length(missing_in_car_data) > 10) {
      cat("... and", length(missing_in_car_data) - 10, "more")
    }
  }
  
  cat("\n\nPerformance settings will be applied:")
  if(length(unique(car_data[[CAR_ID_COL]])) > MAX_CARS_FOR_DROPDOWN) {
    cat("\n  - Server-side selectize will be used (>", MAX_CARS_FOR_DROPDOWN, "cars)")
  }
  if(nrow(car_data) > MAX_LOCATIONS_FOR_SLIDER) {
    cat("\n  - Value range slider will be disabled (>", MAX_LOCATIONS_FOR_SLIDER, "rows)")
  }
  
  cat("\n\nDefault measurement:", DEFAULT_MEASUREMENT)
  cat("\nDisplay names configured for:", length(MEASUREMENT_DISPLAY_NAMES), "measurements")
}
