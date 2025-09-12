# Simple Percentile Table for a Single Variable in R

# Generate sample data (replace with your actual data)
set.seed(123)
values <- rnorm(200, mean = 50, sd = 10)

# Method 1: Basic percentile table
percentiles <- quantile(values, probs = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95))
print(percentiles)

# Method 2: More comprehensive percentile table
percentile_table <- data.frame(
  Percentile = c("5th", "10th", "25th", "50th (Median)", "75th", "90th", "95th"),
  Value = quantile(values, probs = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95))
)

print(percentile_table)

# Method 3: Deciles (every 10th percentile)
deciles <- quantile(values, probs = seq(0.1, 0.9, 0.1))
decile_table <- data.frame(
  Percentile = paste0(seq(10, 90, 10), "th"),
  Value = deciles
)

print("Decile Table:")
print(decile_table)

# Method 4: Custom percentiles with min, median, mean, and max
custom_probs <- c(0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99)
custom_table <- data.frame(
  Percentile = c("Min", paste0(custom_probs[1:2] * 100, "th"), 
                 paste0(custom_probs[3] * 100, "th"), 
                 "Median", "Mean",
                 paste0(custom_probs[5:7] * 100, "th"), "Max"),
  Value = c(min(values), quantile(values, probs = custom_probs[1:2]),
            quantile(values, probs = custom_probs[3]),
            median(values), mean(values),
            quantile(values, probs = custom_probs[5:7]), max(values))
)

print("Custom Percentile Table:")
print(custom_table)

# Method 5: Add basic summary statistics
summary_table <- data.frame(
  Statistic = c("Min", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"),
  Value = c(min(values), quantile(values, 0.25), median(values), 
            mean(values), quantile(values, 0.75), max(values))
)

print("Summary Statistics:")
print(summary_table)
