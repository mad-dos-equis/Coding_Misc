# Distribution Plots for Percentile Visualization in R

# Load required libraries
library(ggplot2)

# Generate sample data
set.seed(123)
values <- rnorm(200, mean = 50, sd = 10)

# Create data frame for ggplot
df <- data.frame(values = values)

# Method 1: Histogram with percentile lines
p1 <- ggplot(df, aes(x = values)) +
  geom_histogram(bins = 30, fill = "lightblue", alpha = 0.7, color = "black") +
  geom_vline(aes(xintercept = quantile(values, 0.25)), 
             color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = quantile(values, 0.50)), 
             color = "red", linetype = "solid", size = 1.2) +
  geom_vline(aes(xintercept = quantile(values, 0.75)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Histogram with Percentile Lines",
       subtitle = "Dashed lines: 25th & 75th percentiles, Solid line: 50th percentile (median)",
       x = "Values", y = "Frequency") +
  theme_minimal()

print(p1)

# Method 2: Density plot with percentiles
p2 <- ggplot(df, aes(x = values)) +
  geom_density(fill = "lightgreen", alpha = 0.7, color = "darkgreen") +
  geom_vline(aes(xintercept = quantile(values, 0.10)), 
             color = "blue", linetype = "dotted") +
  geom_vline(aes(xintercept = quantile(values, 0.25)), 
             color = "blue", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(values, 0.50)), 
             color = "blue", linetype = "solid", size = 1.2) +
  geom_vline(aes(xintercept = quantile(values, 0.75)), 
             color = "blue", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(values, 0.90)), 
             color = "blue", linetype = "dotted") +
  labs(title = "Density Plot with Percentiles",
       subtitle = "10th, 25th, 50th, 75th, and 90th percentiles shown",
       x = "Values", y = "Density") +
  theme_minimal()

print(p2)

# Method 3: Histogram with base R
hist(values, 
     breaks = 30, 
     col = "lightcoral", 
     border = "black",
     main = "Histogram with Percentile Lines (Base R)",
     xlab = "Values", 
     ylab = "Frequency")

# Add percentile lines
abline(v = quantile(values, 0.25), col = "red", lty = 2, lwd = 2)
abline(v = quantile(values, 0.50), col = "red", lty = 1, lwd = 3)
abline(v = quantile(values, 0.75), col = "red", lty = 2, lwd = 2)

# Add legend
legend("topright", 
       legend = c("25th percentile", "50th percentile", "75th percentile"),
       col = "red", lty = c(2, 1, 2), lwd = c(2, 3, 2))

# Method 4: Empirical Cumulative Distribution Function (ECDF)
p3 <- ggplot(df, aes(x = values)) +
  stat_ecdf(geom = "step", color = "purple", size = 1.2) +
  geom_hline(yintercept = c(0.25, 0.50, 0.75), 
             color = "red", linetype = "dashed", alpha = 0.7) +
  geom_vline(aes(xintercept = quantile(values, 0.25)), 
             color = "red", linetype = "dashed", alpha = 0.7) +
  geom_vline(aes(xintercept = quantile(values, 0.50)), 
             color = "red", linetype = "dashed", alpha = 0.7) +
  geom_vline(aes(xintercept = quantile(values, 0.75)), 
             color = "red", linetype = "dashed", alpha = 0.7) +
  labs(title = "Empirical Cumulative Distribution Function",
       subtitle = "Y-axis shows percentiles directly (0.25 = 25th percentile)",
       x = "Values", y = "Cumulative Probability") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format())

print(p3)

# Method 5: Violin plot (single distribution)
p4 <- ggplot(df, aes(x = "", y = values)) +
  geom_violin(fill = "lightyellow", alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.8) +
  stat_summary(fun = mean, geom = "point", 
               shape = 23, size = 3, fill = "red") +
  labs(title = "Violin Plot with Box Plot Overlay",
       subtitle = "Shows full distribution shape plus percentiles",
       x = "", y = "Values") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

print(p4)

# Method 6: Q-Q Plot (comparing to normal distribution)
p5 <- ggplot(df, aes(sample = values)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot vs Normal Distribution",
       subtitle = "Points on red line indicate normal distribution",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

print(p5)

# Method 7: Density plot with filled percentile regions
# Calculate percentiles
p25 <- quantile(values, 0.25)
p75 <- quantile(values, 0.75)

p6 <- ggplot(df, aes(x = values)) +
  geom_density(alpha = 0.3) +
  geom_density(data = subset(df, values >= p25 & values <= p75),
               fill = "red", alpha = 0.5) +
  geom_vline(aes(xintercept = quantile(values, 0.50)), 
             color = "darkred", linetype = "solid", size = 1.2) +
  labs(title = "Density Plot with IQR Highlighted",
       subtitle = "Red area shows interquartile range (25th to 75th percentile)",
       x = "Values", y = "Density") +
  theme_minimal()

print(p6)

# Method 8: Strip chart (dot plot)
stripchart(values, 
           method = "stack", 
           pch = 19, 
           col = "blue",
           main = "Strip Chart Showing All Data Points",
           xlab = "Values")

# Add percentile lines
abline(v = quantile(values, c(0.25, 0.50, 0.75)), 
       col = "red", lty = 2, lwd = 2)

# Print summary statistics
cat("\nSummary Statistics:\n")
summary_stats <- data.frame(
  Min = min(values),
  Q1 = quantile(values, 0.25),
  Median = median(values),
  Mean = mean(values),
  Q3 = quantile(values, 0.75),
  Max = max(values),
  SD = sd(values)
)

print(summary_stats)

# Calculate various percentiles
cat("\nPercentiles:\n")
percentiles <- quantile(values, probs = seq(0.1, 0.9, 0.1))
print(percentiles)
