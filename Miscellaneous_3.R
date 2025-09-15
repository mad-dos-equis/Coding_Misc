# Load required packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(viridis)
library(scales)
library(lubridate)

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

# Generate monthly data for 2024-2025 (simplified - just need these two years)
dates <- seq.Date(from = as.Date("2024-01-01"), 
                  to = as.Date("2025-08-01"), 
                  by = "month")

# Create dataset
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
    # Add 2025 tariff effects for some countries
    tariff_factor = case_when(
      PTN_ISO == "CHN" & YEAR == 2025 & MONTH %in% c(5, 6) ~ 1.4,  # Pull-forward
      PTN_ISO == "CHN" & YEAR == 2025 & MONTH %in% c(7, 8) ~ 0.8,  # Post-tariff drop
      PTN_ISO == "MEX" & YEAR == 2025 & MONTH >= 6 ~ 1.1,  # Substitution benefit
      PTN_ISO == "VNM" & YEAR == 2025 & MONTH >= 5 ~ 1.15,  # Substitution benefit
      TRUE ~ 1.0
    ),
    # Add some general growth
    growth_factor = ifelse(YEAR == 2025, 1.03, 1.0),
    # Calculate import value with some random noise
    VALUE = base_imports * tariff_factor * growth_factor * runif(n(), 0.9, 1.1)
  ) %>%
  select(RPT_ISO, PTN_ISO, YEAR, MONTH, VALUE)

# View sample of the data
cat("Sample of generated data (China 2025):\n")
df %>% 
  filter(PTN_ISO == "CHN", YEAR == 2025, MONTH <= 8) %>% 
  print()

# -----------------------------
# STEP 2: CALCULATE YTD CHANGES
# -----------------------------

ytd_changes <- df %>%
  filter(
    RPT_ISO == "USA",
    MONTH <= 8  # Jan-Aug only
  ) %>%
  group_by(PTN_ISO, YEAR) %>%
  summarise(
    ytd_imports = sum(VALUE, na.rm = TRUE),
    months_count = n(),  # Verify we have all 8 months
    .groups = "drop"
  ) %>%
  # Only keep countries with complete data for both years
  group_by(PTN_ISO) %>%
  filter(all(c(2024, 2025) %in% YEAR) & all(months_count == 8)) %>%
  ungroup() %>%
  # Pivot to get 2024 and 2025 in separate columns
  select(-months_count) %>%
  pivot_wider(
    names_from = YEAR,
    values_from = ytd_imports,
    names_prefix = "imports_"
  ) %>%
  # Calculate percent and absolute changes
  mutate(
    change_pct = (imports_2025 / imports_2024 - 1) * 100,
    change_abs = imports_2025 - imports_2024
  ) %>%
  # Rename for consistency
  rename(country = PTN_ISO)

# View results
cat("\n========== TOP CHANGES (YTD 2024 to 2025) ==========\n")
cat("\nTop 10 Increases:\n")
ytd_changes %>%
  arrange(desc(change_pct)) %>%
  select(country, change_pct, imports_2024, imports_2025) %>%
  mutate(
    imports_2024 = round(imports_2024/1e9, 2),
    imports_2025 = round(imports_2025/1e9, 2),
    change_pct = round(change_pct, 1)
  ) %>%
  head(10) %>%
  print()

cat("\nTop 10 Decreases:\n")
ytd_changes %>%
  arrange(change_pct) %>%
  select(country, change_pct, imports_2024, imports_2025) %>%
  mutate(
    imports_2024 = round(imports_2024/1e9, 2),
    imports_2025 = round(imports_2025/1e9, 2),
    change_pct = round(change_pct, 1)
  ) %>%
  head(10) %>%
  print()

# -----------------------------
# STEP 3: CREATE CHOROPLETH MAP
# -----------------------------

# Get world map data with ISO codes
world <- ne_countries(scale = "medium", returnclass = "sf")

# Join data with map using ISO codes
map_data <- world %>%
  left_join(ytd_changes, by = c("iso_a3" = "country"))

# Calculate color scale limits based on data distribution
lower_limit <- quantile(ytd_changes$change_pct, 0.05, na.rm = TRUE)
upper_limit <- quantile(ytd_changes$change_pct, 0.95, na.rm = TRUE)
max_abs <- max(abs(c(lower_limit, upper_limit)))
color_limits <- c(-max_abs, max_abs)

# Create the map
map_plot <- ggplot(data = map_data) +
  geom_sf(aes(fill = change_pct), color = "white", size = 0.1) +
  scale_fill_gradient2(
    low = "#d73027",      # Red for decreases
    mid = "#ffffbf",      # Yellow for no change
    high = "#1a9850",     # Green for increases
    midpoint = 0,
    limits = color_limits,
    oob = squish,
    name = "YTD Change (%)",
    na.value = "grey80",
    breaks = pretty_breaks(n = 5)
  ) +
  labs(
    title = "Year-to-Date Change in U.S. Imports by Country",
    subtitle = "January-August 2025 vs January-August 2024",
    caption = paste0("Note: Gray indicates no data. ",
                    "Colors capped at ", round(color_limits[1], 1), "% and ", 
                    round(color_limits[2], 1), "% for visibility.")
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2.5, "cm"),
    legend.key.height = unit(0.4, "cm"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 8, hjust = 0)
  ) +
  coord_sf(crs = st_crs(4326))

# Display the map
print(map_plot)

# Save the map
ggsave("ytd_import_changes_2024_2025.png", map_plot, 
       width = 14, height = 8, dpi = 300)

# -----------------------------
# STEP 4: EXPORT RESULTS TABLE
# -----------------------------

# Create comprehensive summary table with country names
export_table <- ytd_changes %>%
  # Add country names by joining with world data
  left_join(world %>% 
            st_drop_geometry() %>% 
            select(iso_a3, name), 
            by = c("country" = "iso_a3")) %>%
  mutate(
    # Add trade size categories
    trade_category = case_when(
      imports_2024 > 10000000000 ~ "Major (>$10B)",
      imports_2024 > 1000000000 ~ "Large ($1-10B)",
      imports_2024 > 100000000 ~ "Medium ($100M-1B)",
      TRUE ~ "Small (<$100M)"
    )
  ) %>%
  select(
    `ISO Code` = country,
    `Country Name` = name,
    `Trade Category` = trade_category,
    `2024 Imports (Jan-Aug)` = imports_2024,
    `2025 Imports (Jan-Aug)` = imports_2025,
    `Change (%)` = change_pct,
    `Change ($)` = change_abs
  ) %>%
  arrange(desc(`Change (%)`))

# Save to CSV
write_csv(export_table, "ytd_import_changes_2024_2025.csv")

# -----------------------------
# STEP 5: SUMMARY STATISTICS
# -----------------------------

summary_stats <- ytd_changes %>%
  summarise(
    n_countries = n(),
    median_change = median(change_pct, na.rm = TRUE),
    mean_change = weighted.mean(change_pct, imports_2024, na.rm = TRUE),
    countries_increasing = sum(change_pct > 0, na.rm = TRUE),
    countries_decreasing = sum(change_pct < 0, na.rm = TRUE),
    total_imports_2024 = sum(imports_2024, na.rm = TRUE),
    total_imports_2025 = sum(imports_2025, na.rm = TRUE),
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

# Check for missing ISO matches (for debugging)
missing_countries <- ytd_changes %>%
  anti_join(world, by = c("country" = "iso_a3")) %>%
  select(country, imports_2024) %>%
  arrange(desc(imports_2024))

if(nrow(missing_countries) > 0) {
  cat("\nWarning: The following ISO codes did not match the map data:\n")
  print(missing_countries)
}
