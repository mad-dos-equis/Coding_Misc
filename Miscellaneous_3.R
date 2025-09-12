# Box Plots for Percentile Visualization in R

# Load required libraries
library(ggplot2)
library(dplyr)

# Generate sample data
set.seed(123)
data <- data.frame(
  group = rep(c("A", "B", "C"), each = 100),
  values = c(rnorm(100, mean = 50, sd = 10),
             rnorm(100, mean = 60, sd = 15),
             rnorm(100, mean = 55, sd = 8))
)

# Method 1: Basic box plot with base R
boxplot(values ~ group, data = data,
        main = "Basic Box Plot Showing Percentiles",
        xlab = "Group", ylab = "Values",
        col = c("lightblue", "lightgreen", "lightcoral"))

# Add custom percentile labels
text(x = 1:3, y = par("usr")[4], 
     labels = c("25th, 50th, 75th percentiles shown"), 
     pos = 1, cex = 0.8)

# Method 2: Enhanced ggplot2 version
p1 <- ggplot(data, aes(x = group, y = values, fill = group)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Box Plot with ggplot2",
       subtitle = "Box boundaries show 25th, 50th, and 75th percentiles",
       x = "Group", y = "Values") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

print(p1)

# Method 3: Customized box plot with specific percentiles
p2 <- ggplot(data, aes(x = group, y = values)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +  # Add whisker caps
  geom_boxplot(aes(fill = group), 
               alpha = 0.7,
               outlier.color = "red",
               outlier.size = 2) +
  stat_summary(fun = mean, geom = "point", 
               shape = 23, size = 3, fill = "white") +  # Add mean
  labs(title = "Detailed Box Plot with Mean and Outliers",
       subtitle = "Whiskers extend to 1.5*IQR, outliers shown in red",
       x = "Group", y = "Values") +
  theme_classic() +
  scale_fill_manual(values = c("A" = "#FF9999", "B" = "#66B2FF", "C" = "#99FF99"))

print(p2)

# Method 4: Box plot with custom percentiles
# Function to calculate custom percentiles
custom_percentiles <- function(x) {
  data.frame(
    ymin = quantile(x, 0.05),   # 5th percentile
    lower = quantile(x, 0.25),  # 25th percentile
    middle = quantile(x, 0.50), # 50th percentile (median)
    upper = quantile(x, 0.75),  # 75th percentile
    ymax = quantile(x, 0.95)    # 95th percentile
  )
}

p3 <- ggplot(data, aes(x = group, y = values, fill = group)) +
  stat_summary(fun.data = custom_percentiles, 
               geom = "boxplot", alpha = 0.7) +
  labs(title = "Custom Percentile Box Plot",
       subtitle = "5th, 25th, 50th, 75th, and 95th percentiles",
       x = "Group", y = "Values") +
  theme_minimal() +
  scale_fill_viridis_d()

print(p3)

# Method 5: Multiple box plots with faceting
# Create data with more groups
extended_data <- data.frame(
  category = rep(c("Type1", "Type2"), each = 300),
  group = rep(rep(c("A", "B", "C"), each = 100), 2),
  values = c(data$values, data$values + rnorm(300, 0, 5))
)

p4 <- ggplot(extended_data, aes(x = group, y = values, fill = group)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ category) +
  labs(title = "Faceted Box Plots for Comparison",
       x = "Group", y = "Values") +
  theme_bw() +
  scale_fill_brewer(palette = "Pastel1")

print(p4)

# Method 6: Horizontal box plot
p5 <- ggplot(data, aes(x = group, y = values, fill = group)) +
  geom_boxplot(alpha = 0.7) +
  coord_flip() +  # Flip coordinates for horizontal boxes
  labs(title = "Horizontal Box Plot",
       x = "Group", y = "Values") +
  theme_minimal()

print(p5)

# Method 7: Box plot with jittered points
p6 <- ggplot(data, aes(x = group, y = values, fill = group)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +  # Hide outliers from boxplot
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.8) +  # Add all points
  labs(title = "Box Plot with Individual Data Points",
       subtitle = "Shows distribution shape along with percentiles",
       x = "Group", y = "Values") +
  theme_minimal()

print(p6)

# Print summary statistics to understand the percentiles
cat("\nSummary Statistics by Group:\n")
summary_stats <- data %>%
  group_by(group) %>%
  summarise(
    Min = min(values),
    Q1 = quantile(values, 0.25),
    Median = median(values),
    Q3 = quantile(values, 0.75),
    Max = max(values),
    IQR = IQR(values),
    .groups = 'drop'
  )

print(summary_stats)

# Calculate specific percentiles
cat("\nDetailed Percentiles by Group:\n")
detailed_percentiles <- data %>%
  group_by(group) %>%
  summarise(
    P5 = quantile(values, 0.05),
    P10 = quantile(values, 0.10),
    P25 = quantile(values, 0.25),
    P50 = quantile(values, 0.50),
    P75 = quantile(values, 0.75),
    P90 = quantile(values, 0.90),
    P95 = quantile(values, 0.95),
    .groups = 'drop'
  )

print(detailed_percentiles)
