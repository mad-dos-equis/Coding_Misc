# Optimized structure for the Shiny app

library(shiny)
library(plotly)
library(sf)
library(dplyr)
library(data.table)  # For faster joins
library(memoise)     # For caching

# Configuration
DEBUG <- FALSE
CACHE_ENABLED <- TRUE

# ============= INITIALIZATION (Run Once) =============

# Initialize logging
log_message <- function(msg, level = "INFO") {
  if (DEBUG || level %in% c("ERROR", "WARNING")) {
    cat(sprintf("[%s] %s: %s\n", Sys.time(), level, msg))
  }
}

# Load and validate data once at startup
initialize_app_data <- function() {
  log_message("Initializing application data")
  
  # Load data (replace with your actual data loading)
  tryCatch({
    # Example: car_data <- fread("car_data.csv")
    # Example: shapefile_data <- st_read("shapefile.shp")
    
    # For now, using the existing approach
    car_data <- get(CAR_DATA_NAME, envir = .GlobalEnv)
    shapefile_data <- get(SHAPEFILE_DATA_NAME, envir = .GlobalEnv)
    
  }, error = function(e) {
    log_message(sprintf("Failed to load data: %s", e$message), "ERROR")
    stop("Data loading failed. Please ensure car_data and shapefile_data exist.")
  })
  
  # Transform CRS once
  if (!is.na(st_crs(shapefile_data)$input) && 
      st_crs(shapefile_data)$input != "EPSG:4326") {
    log_message("Transforming shapefile to WGS84")
    shapefile_data <- st_transform(shapefile_data, 4326)
  }
  
  # Validate shapefile geometries
  invalid_geoms <- !st_is_valid(shapefile_data)
  if (any(invalid_geoms)) {
    log_message(sprintf("Fixing %d invalid geometries", sum(invalid_geoms)), "WARNING")
    shapefile_data <- st_make_valid(shapefile_data)
  }
  
  # Validate required columns
  required_car_cols <- c(CAR_LOCATION_COL, CAR_ID_COL, MEASUREMENT_COLS)
  missing_cols <- setdiff(required_car_cols, colnames(car_data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns in car_data: %s", 
                 paste(missing_cols, collapse = ", ")))
  }
  
  if (!SHAPEFILE_LOCATION_COL %in% colnames(shapefile_data)) {
    stop(sprintf("Missing location column in shapefile: %s", SHAPEFILE_LOCATION_COL))
  }
  
  # Pre-calculate measurement ranges for slider optimization
  measurement_ranges <- lapply(MEASUREMENT_COLS, function(col) {
    values <- car_data[[col]][!is.na(car_data[[col]])]
    if (length(values) > 0) {
      list(min = min(values), max = max(values), exists = TRUE)
    } else {
      list(min = 0, max = 100, exists = FALSE)
    }
  })
  names(measurement_ranges) <- MEASUREMENT_COLS
  
  # Check location matching
  car_locations <- unique(car_data[[CAR_LOCATION_COL]])
  shape_locations <- unique(shapefile_data[[SHAPEFILE_LOCATION_COL]])
  matching_locations <- intersect(car_locations, shape_locations)
  
  if (length(matching_locations) == 0) {
    log_message("No exact location matches found. Trying case-insensitive matching.", "WARNING")
    
    # Create mapping table for case-insensitive matches
    car_data[[CAR_LOCATION_COL]] <- trimws(car_data[[CAR_LOCATION_COL]])
    shapefile_data[[SHAPEFILE_LOCATION_COL]] <- trimws(shapefile_data[[SHAPEFILE_LOCATION_COL]])
    
    # You might want to implement fuzzy matching here
  }
  
  match_rate <- length(matching_locations) / length(car_locations) * 100
  log_message(sprintf("Location match rate: %.1f%% (%d/%d)", 
                      match_rate, length(matching_locations), length(car_locations)))
  
  return(list(
    car_data = car_data,
    shapefile_data = shapefile_data,
    measurement_ranges = measurement_ranges,
    unique_cars = unique(car_data[[CAR_ID_COL]]),
    match_rate = match_rate
  ))
}

# Memoized (cached) data preparation function
if (CACHE_ENABLED) {
  prepare_map_data_cached <- memoise(function(shapefile_data, car_data, selected_car) {
    log_message(sprintf("Preparing data for car: %s (not cached)", selected_car))
    
    filtered_data <- car_data[car_data[[CAR_ID_COL]] == selected_car, ]
    
    if (nrow(filtered_data) == 0) {
      log_message(sprintf("No data found for car: %s", selected_car), "WARNING")
      result <- shapefile_data
      for(col in MEASUREMENT_COLS) {
        result[[col]] <- NA_real_
      }
      return(result)
    }
    
    # Use data.table for faster joins if available
    if (requireNamespace("data.table", quietly = TRUE)) {
      dt_shapefile <- as.data.table(shapefile_data)
      dt_filtered <- as.data.table(filtered_data)
      
      setkeyv(dt_shapefile, SHAPEFILE_LOCATION_COL)
      setkeyv(dt_filtered, CAR_LOCATION_COL)
      
      merged <- dt_filtered[dt_shapefile, on = c(CAR_LOCATION_COL = SHAPEFILE_LOCATION_COL)]
      merged_sf <- st_as_sf(merged)
    } else {
      # Fallback to dplyr
      join_vars <- setNames(CAR_LOCATION_COL, SHAPEFILE_LOCATION_COL)
      merged_sf <- shapefile_data %>%
        left_join(filtered_data, by = join_vars)
    }
    
    return(merged_sf)
  })
} else {
  prepare_map_data_cached <- prepare_map_data
}

# ============= OPTIMIZED UI =============
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .selectize-control { max-height: 400px; overflow-y: auto; }
      .shiny-notification { position: fixed; top: 10px; right: 10px; }
    "))
  ),
  
  titlePanel("Interactive Car Value Map"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_measurement",
                  "Select Measurement:",
                  choices = MEASUREMENT_DISPLAY_NAMES,
                  selected = DEFAULT_MEASUREMENT),
      
      selectizeInput("selected_car",
                     "Select Car:",
                     choices = NULL,
                     options = list(
                       placeholder = "Type to search cars...",
                       maxOptions = 50,
                       searchField = 'value',
                       loadThrottle = 300
                     )),
      
      conditionalPanel(
        condition = "output.show_slider == true",
        uiOutput("value_range_ui")
      ),
      
      selectInput("color_scheme",
                  "Color Scheme:",
                  choices = c("viridis", "plasma", "inferno", "magma", "cividis"),
                  selected = "viridis"),
      
      checkboxInput("show_borders",
                    "Show Location Borders",
                    value = TRUE),
      
      hr(),
      
      verbatimTextOutput("info_text")
    ),
    
    mainPanel(
      plotlyOutput("plotly_map", height = "600px"),
      conditionalPanel(
        condition = "output.show_debug == true",
        verbatimTextOutput("debug_text")
      )
    )
  )
)

# ============= OPTIMIZED SERVER =============
server <- function(input, output, session) {
  
  # Initialize data once
  app_data <- initialize_app_data()
  
  # Set up car selection with server-side processing
  updateSelectizeInput(session, "selected_car",
                       choices = app_data$unique_cars,
                       selected = app_data$unique_cars[1],
                       server = TRUE)
  
  # Reactive values for performance monitoring
  perf_stats <- reactiveValues(
    last_render_time = NULL,
    total_renders = 0
  )
  
  # Show slider only for reasonable data sizes
  output$show_slider <- reactive({
    nrow(app_data$car_data) <= MAX_LOCATIONS_FOR_SLIDER
  })
  outputOptions(output, "show_slider", suspendWhenHidden = FALSE)
  
  # Dynamic slider UI (only created when needed)
  output$value_range_ui <- renderUI({
    req(input$selected_measurement)
    
    range_info <- app_data$measurement_ranges[[input$selected_measurement]]
    if (!range_info$exists) return(NULL)
    
    sliderInput("value_range",
                paste(MEASUREMENT_DISPLAY_NAMES[input$selected_measurement], "Range:"),
                min = range_info$min,
                max = range_info$max,
                value = c(range_info$min, range_info$max),
                step = max(1, round((range_info$max - range_info$min) / 1000)))
  })
  
  # Separate reactive for base data (only depends on car selection)
  base_map_data <- reactive({
    req(input$selected_car)
    prepare_map_data_cached(app_data$shapefile_data, app_data$car_data, input$selected_car)
  })
  
  # Filtered data (adds measurement filtering)
  filtered_map_data <- reactive({
    data <- base_map_data()
    req(input$selected_measurement)
    
    # Apply value range filter if available
    if (!is.null(input$value_range)) {
      current_col <- input$selected_measurement
      if (current_col %in% colnames(data)) {
        data <- data %>%
          filter(is.na(.data[[current_col]]) |
                 (.data[[current_col]] >= input$value_range[1] &
                  .data[[current_col]] <= input$value_range[2]))
      }
    }
    
    # Pre-calculate hover text to avoid issues in ggplot
    if (input$selected_measurement %in% colnames(data)) {
      data$hover_text <- paste0(
        "Location: ", data[[SHAPEFILE_LOCATION_COL]],
        "<br>Car: ", input$selected_car,
        "<br>", MEASUREMENT_DISPLAY_NAMES[input$selected_measurement], ": ",
        format(round(data[[input$selected_measurement]], 2), big.mark = ",")
      )
    }
    
    data
  })
  
  # Optimized plot rendering
  output$plotly_map <- renderPlotly({
    start_time <- Sys.time()
    
    data <- filtered_map_data()
    req(input$selected_measurement)
    
    current_col <- input$selected_measurement
    display_name <- MEASUREMENT_DISPLAY_NAMES[current_col]
    
    if (!current_col %in% colnames(data)) {
      return(plotly_empty() %>%
        layout(title = "Selected measurement not available"))
    }
    
    valid_data_count <- sum(!is.na(data[[current_col]]))
    
    if (nrow(data) == 0 || valid_data_count == 0) {
      return(plotly_empty() %>%
        layout(title = "No data available for current selection"))
    }
    
    # Create plot
    p <- ggplot(data) +
      geom_sf(aes(fill = .data[[current_col]], text = hover_text),
              color = if(input$show_borders) "white" else NA,
              size = if(input$show_borders) 0.3 else 0) +
      scale_fill_viridis_c(name = display_name,
                           na.value = "grey90",
                           option = input$color_scheme) +
      theme_void() +
      labs(title = paste(display_name, "for", input$selected_car))
    
    plotly_obj <- ggplotly(p, tooltip = "text") %>%
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d"),
             displaylogo = FALSE) %>%
      layout(hoverlabel = list(bgcolor = "white"),
             margin = list(l = 0, r = 50, t = 50, b = 0))
    
    # Update performance stats
    perf_stats$last_render_time <- difftime(Sys.time(), start_time, units = "secs")
    perf_stats$total_renders <- perf_stats$total_renders + 1
    
    plotly_obj
  })
  
  # Info display
  output$info_text <- renderText({
    req(input$selected_measurement)
    data <- filtered_map_data()
    current_col <- input$selected_measurement
    
    valid_count <- sum(!is.na(data[[current_col]]))
    total_locations <- nrow(data)
    
    info_lines <- c(
      sprintf("Showing: %s for %s", 
              MEASUREMENT_DISPLAY_NAMES[current_col], 
              input$selected_car),
      sprintf("Locations: %d (with data: %d)", total_locations, valid_count),
      sprintf("Match rate: %.1f%%", app_data$match_rate)
    )
    
    if (!is.null(perf_stats$last_render_time)) {
      info_lines <- c(info_lines,
                     sprintf("Last render: %.2fs", perf_stats$last_render_time))
    }
    
    paste(info_lines, collapse = "\n")
  })
  
  # Debug output (only if DEBUG = TRUE)
  output$show_debug <- reactive({ DEBUG })
  outputOptions(output, "show_debug", suspendWhenHidden = FALSE)
  
  output$debug_text <- renderPrint({
    if (DEBUG) {
      list(
        selected_car = input$selected_car,
        selected_measurement = input$selected_measurement,
        data_rows = nrow(filtered_map_data()),
        render_count = perf_stats$total_renders,
        last_render_time = perf_stats$last_render_time
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
