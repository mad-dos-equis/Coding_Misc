# ================================================================================
# PARAMETERIZED SHINY MAP APPLICATION
# ================================================================================
# This script creates an interactive choropleth map visualization.
# Simply update the CONFIG section below to work with your data.
# ================================================================================

library(shiny)
library(plotly)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)

# ================================================================================
# CONFIGURATION SECTION - CUSTOMIZE THIS FOR YOUR DATA
# ================================================================================

CONFIG <- list(
  
  # ------------------------------------
  # DATA SOURCE CONFIGURATION
  # ------------------------------------
  data_sources = list(
    # Option 1: Load from files
    # car_data = list(
    #   type = "csv",
    #   path = "data/car_data.csv",
    #   encoding = "UTF-8"
    # ),
    # shapefile = list(
    #   type = "shapefile", 
    #   path = "data/regions.shp"
    # ),
    
    # Option 2: Load from R objects (must exist in environment)
    car_data = list(
      type = "object",
      name = "car_data"  # Name of object in environment
    ),
    shapefile = list(
      type = "object",
      name = "shapefile_data"
    )
    
    # Option 3: Load from database
    # car_data = list(
    #   type = "database",
    #   connection_string = "Driver={SQL Server};Server=...;Database=...;",
    #   query = "SELECT * FROM car_metrics"
    # )
  ),
  
  # ------------------------------------
  # COLUMN MAPPING
  # ------------------------------------
  columns = list(
    # Car dataset columns
    car = list(
      location = "location_id",     # Column linking to shapefile locations
      entity_id = "car_id",         # Column with entity identifiers (e.g., car_id)
      measurements = c(             # Numeric columns to visualize
        "value",
        "price", 
        "mileage",
        "age",
        "maintenance_cost"
      )
    ),
    
    # Shapefile columns
    shapefile = list(
      location = "location_name",   # Column linking to car data locations
      geometry = "geometry"         # Geometry column (usually auto-detected)
    )
  ),
  
  # ------------------------------------
  # DISPLAY CONFIGURATION
  # ------------------------------------
  display = list(
    # Application title
    app_title = "Interactive Car Value Map",
    
    # Entity label (singular and plural)
    entity_label = list(
      singular = "Car",
      plural = "Cars"
    ),
    
    # Measurement display names and formatting
    measurement_labels = list(
      "value" = list(
        label = "Car Value",
        prefix = "$",
        suffix = "",
        digits = 0
      ),
      "price" = list(
        label = "Purchase Price",
        prefix = "$",
        suffix = "",
        digits = 0
      ),
      "mileage" = list(
        label = "Mileage",
        prefix = "",
        suffix = " miles",
        digits = 0
      ),
      "age" = list(
        label = "Age",
        prefix = "",
        suffix = " years",
        digits = 1
      ),
      "maintenance_cost" = list(
        label = "Maintenance Cost",
        prefix = "$",
        suffix = "",
        digits = 0
      )
    ),
    
    # Default measurement on startup
    default_measurement = "value",
    
    # Color schemes available
    color_schemes = c("viridis", "plasma", "inferno", "magma", "cividis"),
    default_color_scheme = "viridis",
    
    # Map configuration
    show_borders_default = TRUE,
    na_color = "grey90",
    border_color = "white",
    border_width = 0.3
  ),
  
  # ------------------------------------
  # PERFORMANCE SETTINGS
  # ------------------------------------
  performance = list(
    # Maximum entities before using server-side selectize
    max_entities_dropdown = 1000,
    
    # Maximum rows before disabling range slider
    max_rows_slider = 10000,
    
    # Enable caching for data operations
    enable_cache = TRUE,
    
    # Maximum options to show in dropdown at once
    dropdown_max_options = 50,
    
    # Search throttle in milliseconds
    search_throttle = 300
  ),
  
  # ------------------------------------
  # ADVANCED SETTINGS
  # ------------------------------------
  advanced = list(
    # Enable debug mode (shows additional information)
    debug_mode = FALSE,
    
    # Coordinate reference system for map
    target_crs = 4326,  # WGS84 for web mapping
    
    # Validate geometries on load
    validate_geometries = TRUE,
    
    # Case-sensitive location matching
    case_sensitive_matching = FALSE,
    
    # Fuzzy matching threshold (0-1, or NULL to disable)
    fuzzy_match_threshold = NULL,
    
    # Log file path (NULL to disable file logging)
    log_file = NULL
  )
)

# ================================================================================
# APPLICATION CODE - NO CHANGES NEEDED BELOW THIS LINE
# ================================================================================

# Validation function for configuration
validate_config <- function(config) {
  errors <- character()
  
  # Check required sections exist
  required_sections <- c("data_sources", "columns", "display", "performance", "advanced")
  missing_sections <- setdiff(required_sections, names(config))
  if (length(missing_sections) > 0) {
    errors <- c(errors, paste("Missing config sections:", paste(missing_sections, collapse = ", ")))
  }
  
  # Check data sources
  if (!all(c("car_data", "shapefile") %in% names(config$data_sources))) {
    errors <- c(errors, "Both 'car_data' and 'shapefile' must be specified in data_sources")
  }
  
  # Check column mappings
  if (!all(c("car", "shapefile") %in% names(config$columns))) {
    errors <- c(errors, "Both 'car' and 'shapefile' must be specified in columns")
  }
  
  # Check car columns
  if (!all(c("location", "entity_id", "measurements") %in% names(config$columns$car))) {
    errors <- c(errors, "Car columns must include: location, entity_id, measurements")
  }
  
  # Check default measurement exists
  if (!config$display$default_measurement %in% config$columns$car$measurements) {
    errors <- c(errors, "Default measurement must be one of the specified measurements")
  }
  
  if (length(errors) > 0) {
    stop(paste("Configuration errors:\n", paste(errors, collapse = "\n")))
  }
  
  return(TRUE)
}

# Data loading function based on configuration
load_data_from_config <- function(source_config) {
  if (source_config$type == "csv") {
    return(read.csv(source_config$path, 
                    encoding = source_config$encoding %||% "UTF-8",
                    stringsAsFactors = FALSE))
    
  } else if (source_config$type == "object") {
    if (!exists(source_config$name, envir = .GlobalEnv)) {
      stop(paste("Object", source_config$name, "not found in environment"))
    }
    return(get(source_config$name, envir = .GlobalEnv))
    
  } else if (source_config$type == "shapefile") {
    return(sf::st_read(source_config$path, quiet = TRUE))
    
  } else if (source_config$type == "database") {
    con <- DBI::dbConnect(odbc::odbc(), .connection_string = source_config$connection_string)
    data <- DBI::dbGetQuery(con, source_config$query)
    DBI::dbDisconnect(con)
    return(data)
    
  } else {
    stop(paste("Unknown data source type:", source_config$type))
  }
}

# Logging function
log_message <- function(msg, level = "INFO", config = CONFIG) {
  if (config$advanced$debug_mode || level %in% c("ERROR", "WARNING")) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    log_entry <- sprintf("[%s] %s: %s", timestamp, level, msg)
    
    # Console output
    cat(log_entry, "\n")
    
    # File output if configured
    if (!is.null(config$advanced$log_file)) {
      try(cat(log_entry, "\n", file = config$advanced$log_file, append = TRUE), 
          silent = TRUE)
    }
  }
}

# Format measurement value based on configuration
format_measurement <- function(value, measurement_name, config = CONFIG) {
  if (is.na(value)) return("N/A")
  
  format_config <- config$display$measurement_labels[[measurement_name]]
  if (is.null(format_config)) {
    # Fallback if no formatting specified
    return(format(round(value, 2), big.mark = ","))
  }
  
  formatted <- format(round(value, format_config$digits %||% 2), 
                     big.mark = ",", 
                     scientific = FALSE)
  
  paste0(format_config$prefix %||% "", 
         formatted, 
         format_config$suffix %||% "")
}

# Get display label for measurement
get_measurement_label <- function(measurement_name, config = CONFIG) {
  label_config <- config$display$measurement_labels[[measurement_name]]
  if (!is.null(label_config) && !is.null(label_config$label)) {
    return(label_config$label)
  }
  # Fallback: capitalize and replace underscores
  return(gsub("_", " ", tools::toTitleCase(measurement_name)))
}

# Initialize application data
initialize_app_data <- function(config = CONFIG) {
  log_message("Initializing application data", "INFO", config)
  
  # Validate configuration
  validate_config(config)
  
  # Load data
  log_message("Loading car data", "INFO", config)
  car_data <- load_data_from_config(config$data_sources$car_data)
  
  log_message("Loading shapefile data", "INFO", config)
  shapefile_data <- load_data_from_config(config$data_sources$shapefile)
  
  # Ensure shapefile is an sf object
  if (!inherits(shapefile_data, "sf")) {
    stop("Shapefile data must be an sf object")
  }
  
  # Transform CRS if needed
  current_crs <- st_crs(shapefile_data)$input
  if (!is.na(current_crs) && current_crs != paste0("EPSG:", config$advanced$target_crs)) {
    log_message(sprintf("Transforming CRS from %s to EPSG:%d", 
                       current_crs, config$advanced$target_crs), "INFO", config)
    shapefile_data <- st_transform(shapefile_data, config$advanced$target_crs)
  }
  
  # Validate geometries if configured
  if (config$advanced$validate_geometries) {
    invalid_geoms <- !st_is_valid(shapefile_data)
    if (any(invalid_geoms)) {
      log_message(sprintf("Fixing %d invalid geometries", sum(invalid_geoms)), 
                 "WARNING", config)
      shapefile_data <- st_make_valid(shapefile_data)
    }
  }
  
  # Validate required columns exist
  car_cols <- config$columns$car
  required_car_cols <- c(car_cols$location, car_cols$entity_id, car_cols$measurements)
  missing_car_cols <- setdiff(required_car_cols, colnames(car_data))
  
  if (length(missing_car_cols) > 0) {
    stop(sprintf("Missing columns in car data: %s\nAvailable columns: %s",
                paste(missing_car_cols, collapse = ", "),
                paste(colnames(car_data), collapse = ", ")))
  }
  
  shape_cols <- config$columns$shapefile
  if (!shape_cols$location %in% colnames(shapefile_data)) {
    stop(sprintf("Missing location column '%s' in shapefile. Available: %s",
                shape_cols$location,
                paste(colnames(shapefile_data), collapse = ", ")))
  }
  
  # Handle case-sensitive matching
  if (!config$advanced$case_sensitive_matching) {
    car_data[[car_cols$location]] <- trimws(tolower(car_data[[car_cols$location]]))
    shapefile_data[[shape_cols$location]] <- trimws(tolower(shapefile_data[[shape_cols$location]]))
  }
  
  # Check location matching
  car_locations <- unique(car_data[[car_cols$location]])
  shape_locations <- unique(shapefile_data[[shape_cols$location]])
  matching_locations <- intersect(car_locations, shape_locations)
  match_rate <- length(matching_locations) / length(car_locations) * 100
  
  log_message(sprintf("Location match rate: %.1f%% (%d/%d)", 
                     match_rate, length(matching_locations), length(car_locations)),
                     if(match_rate < 50) "WARNING" else "INFO", config)
  
  # Pre-calculate measurement ranges
  measurement_ranges <- lapply(car_cols$measurements, function(col) {
    values <- car_data[[col]][!is.na(car_data[[col]])]
    if (length(values) > 0) {
      list(
        min = min(values),
        max = max(values),
        mean = mean(values),
        median = median(values),
        exists = TRUE
      )
    } else {
      list(min = 0, max = 100, mean = 50, median = 50, exists = FALSE)
    }
  })
  names(measurement_ranges) <- car_cols$measurements
  
  # Get unique entities
  unique_entities <- unique(car_data[[car_cols$entity_id]])
  
  log_message(sprintf("Data loaded: %d entities, %d locations, %d measurements",
                     length(unique_entities),
                     nrow(shapefile_data),
                     length(car_cols$measurements)), "INFO", config)
  
  return(list(
    car_data = car_data,
    shapefile_data = shapefile_data,
    measurement_ranges = measurement_ranges,
    unique_entities = unique_entities,
    match_rate = match_rate,
    config = config
  ))
}

# Prepare map data function
prepare_map_data <- function(shapefile_data, car_data, selected_entity, config = CONFIG) {
  car_cols <- config$columns$car
  shape_cols <- config$columns$shapefile
  
  # Filter for selected entity
  filtered_data <- car_data[car_data[[car_cols$entity_id]] == selected_entity, ]
  
  if (nrow(filtered_data) == 0) {
    log_message(sprintf("No data found for entity: %s", selected_entity), 
               "WARNING", config)
    result <- shapefile_data
    for(col in car_cols$measurements) {
      result[[col]] <- NA_real_
    }
    return(result)
  }
  
  # Select only needed columns to avoid conflicts
  columns_to_keep <- c(car_cols$location, car_cols$measurements)
  filtered_clean <- filtered_data[, columns_to_keep, drop = FALSE]
  
  # Rename location column to match shapefile
  names(filtered_clean)[names(filtered_clean) == car_cols$location] <- shape_cols$location
  
  # Merge with shapefile
  merged_data <- merge(shapefile_data, filtered_clean, 
                      by = shape_cols$location, 
                      all.x = TRUE)
  
  return(merged_data)
}

# Build UI from configuration
build_ui <- function(config = CONFIG) {
  fluidPage(
    tags$head(
      tags$style(HTML("
        .selectize-control { max-height: 400px; overflow-y: auto; }
        .measurement-info { 
          background: #f5f5f5; 
          padding: 10px; 
          border-radius: 5px; 
          margin-top: 10px;
        }
      "))
    ),
    
    titlePanel(config$display$app_title),
    
    sidebarLayout(
      sidebarPanel(
        h4("Settings"),
        
        # Measurement selection
        selectInput(
          "selected_measurement",
          "Select Measurement:",
          choices = setNames(
            config$columns$car$measurements,
            sapply(config$columns$car$measurements, get_measurement_label, config = config)
          ),
          selected = config$display$default_measurement
        ),
        
        # Entity selection
        selectizeInput(
          "selected_entity",
          paste("Select", config$display$entity_label$singular, ":"),
          choices = NULL,
          options = list(
            placeholder = paste("Type to search", 
                              tolower(config$display$entity_label$plural), "..."),
            maxOptions = config$performance$dropdown_max_options,
            searchField = 'value',
            loadThrottle = config$performance$search_throttle
          )
        ),
        
        # Conditional range filter
        conditionalPanel(
          condition = "output.show_slider == true",
          uiOutput("value_range_ui")
        ),
        
        hr(),
        
        h4("Display Options"),
        
        # Color scheme selection
        selectInput(
          "color_scheme",
          "Color Scheme:",
          choices = config$display$color_schemes,
          selected = config$display$default_color_scheme
        ),
        
        # Border toggle
        checkboxInput(
          "show_borders",
          "Show Location Borders",
          value = config$display$show_borders_default
        ),
        
        hr(),
        
        # Information display
        div(class = "measurement-info",
          h5("Current Selection"),
          verbatimTextOutput("info_text")
        ),
        
        # Debug panel
        conditionalPanel(
          condition = paste0("'", config$advanced$debug_mode, "' == 'TRUE'"),
          hr(),
          h5("Debug Information"),
          verbatimTextOutput("debug_text")
        )
      ),
      
      mainPanel(
        plotlyOutput("map_plot", height = "600px"),
        
        # Data summary below map
        conditionalPanel(
          condition = "output.has_data == true",
          div(
            style = "margin-top: 20px;",
            h4("Data Summary"),
            tableOutput("summary_table")
          )
        )
      )
    )
  )
}

# Build server from configuration
build_server <- function(config = CONFIG) {
  function(input, output, session) {
    
    # Initialize data
    app_data <- initialize_app_data(config)
    
    # Performance tracking
    perf_stats <- reactiveValues(
      last_render_time = NULL,
      total_renders = 0
    )
    
    # Update entity selection
    updateSelectizeInput(
      session, 
      "selected_entity",
      choices = app_data$unique_entities,
      selected = app_data$unique_entities[1],
      server = length(app_data$unique_entities) > config$performance$max_entities_dropdown
    )
    
    # Determine if slider should be shown
    output$show_slider <- reactive({
      nrow(app_data$car_data) <= config$performance$max_rows_slider
    })
    outputOptions(output, "show_slider", suspendWhenHidden = FALSE)
    
    # Dynamic slider UI
    output$value_range_ui <- renderUI({
      req(input$selected_measurement)
      
      range_info <- app_data$measurement_ranges[[input$selected_measurement]]
      if (!range_info$exists) return(NULL)
      
      label <- paste(get_measurement_label(input$selected_measurement, config), "Range:")
      
      sliderInput(
        "value_range",
        label,
        min = range_info$min,
        max = range_info$max,
        value = c(range_info$min, range_info$max),
        step = max(1, (range_info$max - range_info$min) / 1000)
      )
    })
    
    # Prepare map data
    map_data <- reactive({
      req(input$selected_entity, input$selected_measurement)
      
      data <- prepare_map_data(
        app_data$shapefile_data, 
        app_data$car_data, 
        input$selected_entity,
        config
      )
      
      # Apply value filter if available
      if (!is.null(input$value_range)) {
        current_col <- input$selected_measurement
        if (current_col %in% colnames(data)) {
          data <- data %>%
            filter(is.na(.data[[current_col]]) |
                   (.data[[current_col]] >= input$value_range[1] &
                    .data[[current_col]] <= input$value_range[2]))
        }
      }
      
      # Add hover text
      if (input$selected_measurement %in% colnames(data)) {
        data$hover_text <- paste0(
          "Location: ", data[[config$columns$shapefile$location]],
          "<br>", config$display$entity_label$singular, ": ", input$selected_entity,
          "<br>", get_measurement_label(input$selected_measurement, config), ": ",
          sapply(data[[input$selected_measurement]], format_measurement, 
                measurement_name = input$selected_measurement, config = config)
        )
      }
      
      data
    })
    
    # Check if we have data
    output$has_data <- reactive({
      data <- map_data()
      sum(!is.na(data[[input$selected_measurement]])) > 0
    })
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)
    
    # Render map
    output$map_plot <- renderPlotly({
      start_time <- Sys.time()
      
      data <- map_data()
      req(input$selected_measurement)
      
      current_col <- input$selected_measurement
      display_name <- get_measurement_label(current_col, config)
      
      if (!current_col %in% colnames(data)) {
        return(plotly_empty() %>%
          layout(title = "Selected measurement not available"))
      }
      
      valid_count <- sum(!is.na(data[[current_col]]))
      
      if (nrow(data) == 0 || valid_count == 0) {
        return(plotly_empty() %>%
          layout(title = "No data available for current selection"))
      }
      
      # Create plot
      p <- ggplot(data) +
        geom_sf(
          aes(fill = .data[[current_col]], text = hover_text),
          color = if(input$show_borders) config$display$border_color else NA,
          size = if(input$show_borders) config$display$border_width else 0
        ) +
        scale_fill_viridis_c(
          name = display_name,
          na.value = config$display$na_color,
          option = input$color_scheme
        ) +
        theme_void() +
        labs(title = paste(display_name, "for", input$selected_entity))
      
      plotly_obj <- ggplotly(p, tooltip = "text") %>%
        config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d"),
          displaylogo = FALSE
        ) %>%
        layout(
          hoverlabel = list(bgcolor = "white"),
          margin = list(l = 0, r = 50, t = 50, b = 0)
        )
      
      # Track performance
      perf_stats$last_render_time <- difftime(Sys.time(), start_time, units = "secs")
      perf_stats$total_renders <- perf_stats$total_renders + 1
      
      plotly_obj
    })
    
    # Information text
    output$info_text <- renderText({
      req(input$selected_measurement)
      data <- map_data()
      current_col <- input$selected_measurement
      
      valid_count <- sum(!is.na(data[[current_col]]))
      total_locations <- nrow(data)
      
      range_info <- app_data$measurement_ranges[[current_col]]
      
      info_lines <- c(
        sprintf("%s: %s", 
                config$display$entity_label$singular, 
                input$selected_entity),
        sprintf("Measurement: %s", 
                get_measurement_label(current_col, config)),
        sprintf("Locations: %d total, %d with data", 
                total_locations, valid_count),
        sprintf("Range: %s to %s",
                format_measurement(range_info$min, current_col, config),
                format_measurement(range_info$max, current_col, config))
      )
      
      if (config$advanced$debug_mode && !is.null(perf_stats$last_render_time)) {
        info_lines <- c(info_lines,
                       sprintf("Render time: %.2fs", perf_stats$last_render_time))
      }
      
      paste(info_lines, collapse = "\n")
    })
    
    # Summary table
    output$summary_table <- renderTable({
      data <- map_data()
      current_col <- input$selected_measurement
      
      if (sum(!is.na(data[[current_col]])) == 0) {
        return(NULL)
      }
      
      values <- data[[current_col]][!is.na(data[[current_col]])]
      
      summary_df <- data.frame(
        Statistic = c("Count", "Min", "Max", "Mean", "Median", "Std Dev"),
        Value = c(
          length(values),
          format_measurement(min(values), current_col, config),
          format_measurement(max(values), current_col, config),
          format_measurement(mean(values), current_col, config),
          format_measurement(median(values), current_col, config),
          format_measurement(sd(values), current_col, config)
        )
      )
      
      summary_df
    }, striped = TRUE, hover = TRUE)
    
    # Debug text
    output$debug_text <- renderPrint({
      if (config$advanced$debug_mode) {
        list(
          entity = input$selected_entity,
          measurement = input$selected_measurement,
          data_rows = nrow(map_data()),
          valid_values = sum(!is.na(map_data()[[input$selected_measurement]])),
          renders = perf_stats$total_renders,
          last_render = perf_stats$last_render_time,
          match_rate = paste0(round(app_data$match_rate, 1), "%")
        )
      }
    })
  }
}

# ================================================================================
# RUN APPLICATION
# ================================================================================

# Create and run the Shiny app
shinyApp(
  ui = build_ui(CONFIG),
  server = build_server(CONFIG)
)

# ================================================================================
# UTILITY FUNCTIONS FOR TESTING
# ================================================================================

# Test configuration without running app
test_config <- function(config = CONFIG) {
  cat("Testing configuration...\n")
  
  tryCatch({
    validate_config(config)
    cat("✓ Configuration structure is valid\n")
    
    # Try to initialize data
    app_data <- initialize_app_data(config)
    cat("✓ Data loaded successfully\n")
    
    cat(sprintf("  - %d %s found\n", 
                length(app_data$unique_entities),
                tolower(config$display$entity_label$plural)))
    cat(sprintf("  - %d locations in shapefile\n", 
                nrow(app_data$shapefile_data)))
    cat(sprintf("  - %d measurements configured\n", 
                length(config$columns$car$measurements)))
    cat(sprintf("  - %.1f%% location match rate\n", 
                app_data$match_rate))
    
    # Test a sample merge
    test_entity <- app_data$unique_entities[1]
    test_data <- prepare_map_data(
      app_data$shapefile_data,
      app_data$car_data,
      test_entity,
      config
    )
    
    valid_counts <- sapply(config$columns$car$measurements, function(m) {
      sum(!is.na(test_data[[m]]))
    })
    
    cat(sprintf("\nTest merge for %s '%s':\n", 
                tolower(config$display$entity_label$singular),
                test_entity))
    for (m in names(valid_counts)) {
      cat(sprintf("  - %s: %d valid values\n", m, valid_counts[m]))
    }
    
    cat("\n✓ Configuration test passed!\n")
    return(TRUE)
    
  }, error = function(e) {
    cat(sprintf("\n✗ Configuration test failed: %s\n", e$message))
    return(FALSE)
  })
}

# Generate example configuration file
generate_example_config <- function(filename = "map_config_example.R") {
  cat("
# Example configuration file for parameterized Shiny map
# Save this as a separate file and source it before running the app

CONFIG <- list(
  data_sources = list(
    car_data = list(
      type = 'csv',
      path = 'path/to/your/data.csv'
    ),
    shapefile = list(
      type = 'shapefile',
      path = 'path/to/your/shapefile.shp'
    )
  ),
  
  columns = list(
    car = list(
      location = 'location_column',
      entity_id = 'id_column',
      measurements = c('measure1', 'measure2', 'measure3')
    ),
    shapefile = list(
      location = 'location_name'
    )
  ),
  
  display = list(
    app_title = 'My Custom Map',
    entity_label = list(singular = 'Item', plural = 'Items'),
    measurement_labels = list(
      'measure1' = list(label = 'Measure 1', prefix = '$', suffix = '', digits = 0),
      'measure2' = list(label = 'Measure 2', prefix = '', suffix = ' units', digits = 2)
    ),
    default_measurement = 'measure1',
    color_schemes = c('viridis', 'plasma'),
    default_color_scheme = 'viridis',
    show_borders_default = TRUE,
    na_color = 'grey90',
    border_color = 'white',
    border_width = 0.3
  ),
  
  performance = list(
    max_entities_dropdown = 1000,
    max_rows_slider = 10000,
    enable_cache = TRUE,
    dropdown_max_options = 50,
    search_throttle = 300
  ),
  
  advanced = list(
    debug_mode = FALSE,
    target_crs = 4326,
    validate_geometries = TRUE,
    case_sensitive_matching = FALSE,
    fuzzy_match_threshold = NULL,
    log_file = NULL
  )
)
", file = filename)
  cat(sprintf("Example configuration saved to '%s'\n", filename))
}

# ================================================================================
# ALTERNATIVE USAGE PATTERNS
# ================================================================================

# Pattern 1: Load config from external file
# source("my_config.R")
# shinyApp(ui = build_ui(CONFIG), server = build_server(CONFIG))

# Pattern 2: Create multiple configs for different datasets
# CONFIG_CARS <- list(...)
# CONFIG_SALES <- list(...)
# shinyApp(ui = build_ui(CONFIG_CARS), server = build_server(CONFIG_CARS))

# Pattern 3: Override specific config values
# CONFIG$display$app_title <- "My Custom Title"
# CONFIG$advanced$debug_mode <- TRUE
# shinyApp(ui = build_ui(CONFIG), server = build_server(CONFIG))

# Pattern 4: Test before running
# if (test_config(CONFIG)) {
#   shinyApp(ui = build_ui(CONFIG), server = build_server(CONFIG))
# }
