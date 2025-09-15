# Load required packages
library(tidyverse)
library(seasonal)
library(sf)
library(rnaturalearth)
library(viridis)
library(scales)
library(lubridate)

# Check X-13 installation
checkX13()  # Follow instructions if not installed

# -----------------------------
# STEP 1: CREATE SAMPLE DATA
# -----------------------------

# Create sample import data for multiple countries
set.seed(123)  # For reproducibility

# Define countries and their base import levels
countries <- data.frame(
  PTN_ISO = c("CHN", "MEX", "CAN", "JPN", "DEU", "KOR", "VNM", "IND", 
              "GBR", "ITA", "FRA", "THA", "MYS", "BRA", "NLD", "CHE",
              "IDN", "SGP", "PHL", "AUS"),
  base_imports = c(40000, 35000, 30000, 12000, 10000, 8000, 7500, 7000,
                   6000, 5500, 5000, 4500, 4000, 3500, 3000, 2800,
                   2500, 2300, 2000, 1800) * 1000000  # Convert to dollars
)

# Generate monthly data from 2019-2025
dates <- seq.Date(from = as.Date("2019-01-01"), 
                  to = as.Date("2025-08-01"), 
                  by = "month")

# Create full dataset
df <- expand.grid(
  date = dates,
  PTN_ISO = countries$PTN_ISO,
  stringsAsFactors = FALSE
) %>%
  left_join(countries, by = "PTN_ISO") %>%
  mutate(
    YEAR = year(date),
    MONTH = month(date),
    RPT_ISO = "USA",
    # Add seasonal pattern (higher imports in Oct-Nov for holidays)
    seasonal_factor = case_when(
      MONTH %in% c(10, 11) ~ 1.3,
      MONTH %in% c(9, 12) ~ 1.15,
      MONTH %in% c(1, 2) ~ 0.85,
      MONTH %in% c(6, 7) ~ 0.95,
      TRUE ~ 1.0
    ),
    # Add some growth over time
    growth_factor = 1 + (YEAR - 2019) * 0.02,
    # Add COVID shock
    covid_factor = case_when(
      YEAR == 2020 & MONTH %in% c(3, 4, 5) ~ 0.7,
      YEAR == 2020 & MONTH %in% c(6, 7, 8) ~ 0.85,
      TRUE ~ 1.0
    ),
    # Add 2025 tariff effects for some countries
    tariff_factor = case_when(
      PTN_ISO == "CHN" & YEAR == 2025 & MONTH %in% c(5, 6) ~ 1.4,  # Pull-forward
      PTN_ISO == "CHN" & YEAR == 2025 & MONTH %in% c(7, 8) ~ 0.8,  # Post-tariff drop
      PTN_ISO == "MEX" & YEAR == 2025 & MONTH >= 6 ~ 1.1,  # Substitution benefit
      TRUE ~ 1.0
    ),
    # Calculate final import value with some random noise
    VALUE = base_imports * seasonal_factor * growth_factor * covid_factor * tariff_factor * 
            runif(n(), 0.9, 1.1)
  ) %>%
  select(RPT_ISO, PTN_ISO, YEAR, MONTH, VALUE)

# View sample of the data
cat("Sample of generated data:\n")
df %>% 
  filter(PTN_ISO == "CHN", YEAR == 2025, MONTH <= 8) %>% 
  print()

# -----------------------------
# STEP 2: PREPARE DATA FOR ANALYSIS
# -----------------------------

# Filter for USA as reporter and create date column
df_prepared <- df %>%
  filter(RPT_ISO == "USA") %>%
  mutate(date = as.Date(paste(YEAR, MONTH, "01", sep = "-"))) %>%
  rename(
    country = PTN_ISO,
    imports = VALUE
  ) %>%
  select(date, country, imports) %>%
  arrange(country, date)

# Check data range
cat("\n========== DATA SUMMARY ==========\n")
cat("Data range:", as.character(min(df_prepared$date)), "to", as.character(max(df_prepared$date)), "\n")
cat("Number of unique exporters:", n_distinct(df_prepared$country), "\n")
cat("Total observations:", nrow(df_prepared), "\n")

# Show import levels by country for 2024
cat("\n2024 Total Imports by Country (Jan-Aug):\n")
df_prepared %>%
  filter(year(date) == 2024, month(date) <= 8) %>%
  group_by(country) %>%
  summarise(total_imports_2024 = sum(imports)/1e9, .groups = "drop") %>%
  arrange(desc(total_imports_2024)) %>%
  mutate(total_imports_2024 = round(total_imports_2024, 2)) %>%
  print(n = 10)

# -----------------------------
# STEP 3: SEASONAL ADJUSTMENT FUNCTION
# -----------------------------

adjust_country <- function(country_df, country_name) {
  
  # Check if we have enough data (at least 3 years = 36 months)
  if(nrow(country_df) < 36) {
    message(paste("Insufficient data for", country_name, "- using original values"))
    country_df$imports_sa <- country_df$imports
    country_df$seasonal_factor <- 1
    country_df$adjustment_method <- "none"
    return(country_df)
  }
  
  # Convert to ts object
  ts_data <- ts(country_df$imports, 
                start = c(year(min(country_df$date)), 
                         month(min(country_df$date))), 
                frequency = 12)
  
  # Try seasonal adjustment
  result <- tryCatch({
    # Run X-13ARIMA-SEATS
    sa_model <- seas(ts_data,
                     transform.function = "auto",
                     regression.aictest = c("td", "easter"),
                     outlier = NULL,
                     forecast.maxlead = 0)
    
    # Extract adjusted series
    country_df$imports_sa <- as.numeric(final(sa_model))
    country_df$seasonal_factor <- as.numeric(original(sa_model)/final(sa_model))
    country_df$adjustment_method <- "x13"
    
    # Print diagnostics for major trading partners
    if(mean(country_df$imports, na.rm = TRUE) > 10000000000) {  # $10B threshold
      qs_stat <- sa_model$qsori
      message(paste(country_name, "- QS statistic:", round(qs_stat, 3)))
    }
    
    return(country_df)
    
  }, error = function(e) {
    # If X-13 fails, try simple STL as fallback
    tryCatch({
      stl_decomp <- stl(ts_data, s.window = "periodic", robust = TRUE)
      country_df$imports_sa <- as.numeric(seasadj(stl_decomp))
      country_df$seasonal_factor <- as.numeric(ts_data/seasadj(stl_decomp))
      country_df$adjustment_method <- "stl"
      message(paste(country_name, "- Used STL decomposition"))
      return(country_df)
      
    }, error = function(e2) {
      # If both fail, return original
      message(paste(country_name, "- No seasonal adjustment applied"))
      country_df$imports_sa <- country_df$imports
      country_df$seasonal_factor <- 1
      country_df$adjustment_method <- "none"
      return(country_df)
    })
  })
  
  return(result)
}

# -----------------------------
# STEP 4: APPLY SEASONAL ADJUSTMENT
# -----------------------------

cat("\n========== APPLYING SEASONAL ADJUSTMENT ==========\n")

df_adjusted <- df_prepared %>%
  arrange(country, date) %>%
  group_by(country) %>%
  group_modify(~ adjust_country(.x, .y$country)) %>%
  ungroup()

# Check adjustment summary
adjustment_summary <- df_adjusted %>%
  group_by(country, adjustment_method) %>%
  summarise(n = n(), .groups = "drop") %>%
  spread(adjustment_method, n, fill = 0)

cat("\nSeasonal Adjustment Methods Used:\n")
print(adjustment_summary)

# -----------------------------
# STEP 5: CALCULATE YTD CHANGES
# -----------------------------

ytd_changes <- df_adjusted %>%
  filter(month(date) <= 8) %>%
  mutate(year = year(date)) %>%
  filter(year %in% c(2024, 2025)) %>%
  group_by(country, year) %>%
  summarise(
    imports_total = sum(imports, na.rm = TRUE),
    imports_sa_total = sum(imports_sa, na.rm = TRUE),
    months_available = n(),
    .groups = "drop"
  ) %>%
  filter(months_available == 8) %>%
  pivot_wider(
    names_from = year, 
    values_from = c(imports_total, imports_sa_total, months_available),
    names_prefix = "y"
  ) %>%
  mutate(
    raw_change_pct = (imports_total_y2025 / imports_total_y2024 - 1) * 100,
    sa_change_pct = (imports_sa_total_y2025 / imports_sa_total_y2024 - 1) * 100,
    sa_change_abs = imports_sa_total_y2025 - imports_sa_total_y2024
  ) %>%
  mutate(
    sa_change_pct = case_when(
      is.infinite(sa_change_pct) ~ NA_real_,
      imports_sa_total_y2024 == 0 ~ NA_real_,
      TRUE ~ sa_change_pct
    )
  )

# View results
cat("\n========== TOP CHANGES (2024 to 2025) ==========\n")
cat("\nTop 10 Increases (Seasonally Adjusted):\n")
ytd_changes %>%
  arrange(desc(sa_change_pct)) %>%
  select(country, sa_change_pct, imports_sa_total_y2024, imports_sa_total_y2025) %>%
  mutate(across(starts_with("imports"), ~round(./1e9, 2))) %>%
  mutate(sa_change_pct = round(sa_change_pct, 1)) %>%
  head(10) %>%
  print()

cat("\nTop 10 Decreases (Seasonally Adjusted):\n")
ytd_changes %>%
  arrange(sa_change_pct) %>%
  select(country, sa_change_pct, imports_sa_total_y2024, imports_sa_total_y2025) %>%
  mutate(across(starts_with("imports"), ~round(./1e9, 2))) %>%
  mutate(sa_change_pct = round(sa_change_pct, 1)) %>%
  head(10) %>%
  print()

# -----------------------------
# STEP 6: CREATE CHOROPLETH MAP
# -----------------------------

# Get world map data with ISO codes
world <- ne_countries(scale = "medium", returnclass = "sf")

# Join data with map using ISO codes
map_data <- world %>%
  left_join(ytd_changes, by = c("iso_a3" = "country"))

# Calculate color scale limits
lower_limit <- quantile(ytd_changes$sa_change_pct, 0.05, na.rm = TRUE)
upper_limit <- quantile(ytd_changes$sa_change_pct, 0.95, na.rm = TRUE)
max_abs <- max(abs(c(lower_limit, upper_limit)))
color_limits <- c(-max_abs, max_abs)

# Create the map
map_plot <- ggplot(data = map_data) +
  geom_sf(aes(fill = sa_change_pct), color = "white", size = 0.1) +
  scale_fill_gradient2(
    low = "#d73027",      # Red for decreases
    mid = "#ffffbf",      # Yellow for no change
    high = "#1a9850",     # Green for increases
    midpoint = 0,
    limits = color_limits,
    oob = squish,
    name = "Change (%)",
    na.value = "grey80",
    breaks = pretty_breaks(n = 5)
  ) +
  labs(
    title = "Change in U.S. Imports by Country (2024 to 2025)",
    subtitle = "Seasonally Adjusted, January-August Year-to-Date Comparison",
    caption = paste0("Note: Gray indicates no data. Colors capped at ", 
                    round(color_limits[1], 1), "% and ", 
                    round(color_limits[2], 1), "% for visibility.\n",
                    "Sample data with simulated tariff effects for demonstration.")
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2.5, "cm"),
    legend.key.height = unit(0.4, "cm"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  coord_sf(crs = st_crs(4326))

print(map_plot)

# Save outputs
ggsave("import_changes_map_sample.png", map_plot, width = 14, height = 8, dpi = 300)

# -----------------------------
# STEP 7: SUMMARY STATISTICS
# -----------------------------

summary_stats <- ytd_changes %>%
  summarise(
    n_countries = n(),
    median_change = median(sa_change_pct, na.rm = TRUE),
    mean_change = weighted.mean(sa_change_pct, imports_sa_total_y2024, na.rm = TRUE),
    countries_increasing = sum(sa_change_pct > 0, na.rm = TRUE),
    countries_decreasing = sum(sa_change_pct < 0, na.rm = TRUE),
    total_imports_2024 = sum(imports_sa_total_y2024, na.rm = TRUE),
    total_imports_2025 = sum(imports_sa_total_y2025, na.rm = TRUE),
    overall_change = (total_imports_2025 / total_imports_2024 - 1) * 100
  )

cat("\n========== SUMMARY STATISTICS ==========\n")
cat(sprintf("Countries analyzed: %d\n", summary_stats$n_countries))
cat(sprintf("Median change: %.1f%%\n", summary_stats$median_change))
cat(sprintf("Trade-weighted mean change: %.1f%%\n", summary_stats$mean_change))
cat(sprintf("Countries with increases: %d\n", summary_stats$countries_increasing))
cat(sprintf("Countries with decreases: %d\n", summary_stats$countries_decreasing))
cat(sprintf("Total imports Jan-Aug 2024: $%.2f billion\n", 
            summary_stats$total_imports_2024/1e9))
cat(sprintf("Total imports Jan-Aug 2025: $%.2f billion\n", 
            summary_stats$total_imports_2025/1e9))
cat(sprintf("Overall change: %.1f%%\n", summary_stats$overall_change))
cat("=========================================\n")
