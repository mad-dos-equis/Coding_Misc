################################################################################
# DATA-DRIVEN TARIFFED CATEGORY DETECTION (APPROACH 1: CLUSTERING)
#
# - Uses coverage = dutiable_value / customs_value
# - Uses tariff_dutiable = calculated_duties / dutiable_value
# - Uses k-means on (coverage, tariff_dutiable) to find a "tariffed" cluster
# - Derives c_star and r_star from the lower edge (10th percentile) of that cluster
################################################################################

#===============================================================================
# SECTION 0: SETUP
#===============================================================================

# Load required packages
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

# Weighted quantile helper (avoids extra dependencies)
wtd_quantile <- function(x, weights = NULL, probs = 0.5, na.rm = TRUE) {
  if (na.rm) {
    keep <- !is.na(x) & !is.na(weights)
    if (!is.null(weights)) {
      x <- x[keep]
      weights <- weights[keep]
    } else {
      x <- x[!is.na(x)]
    }
  }
  if (is.null(weights)) {
    return(stats::quantile(x, probs = probs, na.rm = FALSE, type = 7))
  }
  # sort by x
  o <- order(x)
  x_sorted <- x[o]
  w_sorted <- weights[o]
  w_cum <- cumsum(w_sorted) / sum(w_sorted)
  sapply(probs, function(p) {
    idx <- which(w_cum >= p)[1]
    x_sorted[idx]
  })
}

#===============================================================================
# SECTION 1: SYNTHETIC HS-LEVEL DATA (EXAMPLE)
#===============================================================================

set.seed(123)

n_codes <- 400

# Create fake HS-10 codes
hs_codes <- sprintf("%010d", sample(1e9:1.5e9, n_codes))

# We'll simulate three "types" of HS codes:
# 1) Mostly duty-free: low coverage, low rate
# 2) Mixed: medium coverage, modest rate
# 3) Heavily tariffed: high coverage, higher rate

type <- sample(c("duty_freeish", "mixed", "heavily_tariffed"),
               size = n_codes,
               replace = TRUE,
               prob = c(0.4, 0.3, 0.3))

synthetic_df <- tibble(
  hs_code = hs_codes,
  type    = type,
  customs_value = runif(n_codes, min = 1e5, max = 5e7) # overall import value
) %>%
  rowwise() %>%
  mutate(
    # Simulate coverage and tariff rates depending on "type"
    true_coverage = case_when(
      type == "duty_freeish"      ~ runif(1, 0.00, 0.35),
      type == "mixed"             ~ runif(1, 0.20, 0.75),
      type == "heavily_tariffed"  ~ runif(1, 0.60, 1.00)
    ),
    true_tariff_dutiable = case_when(
      type == "duty_freeish"      ~ runif(1, 0.000, 0.010), # ~0–1%
      type == "mixed"             ~ runif(1, 0.005, 0.025), # 0.5–2.5%
      type == "heavily_tariffed"  ~ runif(1, 0.020, 0.080)  # 2–8%
    ),
    dutiable_value = true_coverage * customs_value,
    calculated_duties = true_tariff_dutiable * dutiable_value
  ) %>%
  ungroup()

# For demonstration, this synthetic_df plays the role of your August 2025 extract:
df_aug25_raw <- synthetic_df

#===============================================================================
# SECTION 2: COMPUTE COVERAGE, TARIFF_DUTIABLE, AND ETR
#===============================================================================

df_aug25 <- df_aug25_raw %>%
  mutate(
    coverage = if_else(customs_value > 0,
                       dutiable_value / customs_value,
                       NA_real_),
    tariff_dutiable = if_else(dutiable_value > 0,
                              calculated_duties / dutiable_value,
                              NA_real_),
    ETR = if_else(customs_value > 0,
                  calculated_duties / customs_value,
                  NA_real_)
  )

# Sanity check (synthetic case: these should be close to true_coverage/true_tariff_dutiable)
df_aug25 %>%
  select(hs_code, type, coverage, tariff_dutiable, ETR) %>%
  head()

#===============================================================================
# SECTION 3: PREP DATA FOR CLUSTERING
#===============================================================================

df_use <- df_aug25 %>%
  filter(
    !is.na(coverage),
    !is.na(tariff_dutiable),
    customs_value > 0
  )

# Optional: drop very small flows to reduce noise from tiny trade lines
min_value_threshold <- quantile(df_use$customs_value, 0.10, na.rm = TRUE)
df_use <- df_use %>%
  filter(customs_value >= min_value_threshold)

# Data matrix for clustering
X <- df_use %>%
  select(coverage, tariff_dutiable) %>%
  as.matrix()

# Scale variables
X_scaled <- scale(X)

#===============================================================================
# SECTION 4: K-MEANS CLUSTERING TO FIND "TARIFFED" GROUP
#===============================================================================

set.seed(123)
k <- 3  # you can experiment with 2–4, but 3 is a good starting point

km <- kmeans(X_scaled, centers = k)

df_use <- df_use %>%
  mutate(cluster = km$cluster)

# Summarize clusters (import-value weighted)
cluster_summary <- df_use %>%
  group_by(cluster) %>%
  summarise(
    mean_cov  = weighted.mean(coverage, customs_value),
    mean_rate = weighted.mean(tariff_dutiable, customs_value),
    total_value = sum(customs_value),
    n_codes   = n(),
    .groups   = "drop"
  )

cat("Cluster summary:\n")
print(cluster_summary)

# Choose the "tariffed" cluster: highest (mean_cov + mean_rate)
tariffed_cluster <- cluster_summary %>%
  arrange(desc(mean_cov + mean_rate)) %>%
  slice(1) %>%
  pull(cluster)

cat("\nChosen 'tariffed' cluster:", tariffed_cluster, "\n\n")

#===============================================================================
# SECTION 5: DATA-DRIVEN c_star AND r_star
#===============================================================================

tariffed_df <- df_use %>%
  filter(cluster == tariffed_cluster)

# Compute value-weighted 10th percentile thresholds
c_star <- wtd_quantile(
  tariffed_df$coverage,
  weights = tariffed_df$customs_value,
  probs = 0.10
)

r_star <- wtd_quantile(
  tariffed_df$tariff_dutiable,
  weights = tariffed_df$customs_value,
  probs = 0.10
)

cat("Data-driven thresholds:\n")
cat("  c_star (coverage threshold):        ", round(c_star, 4), "\n")
cat("  r_star (tariff_dutiable threshold): ", round(r_star, 4), "\n\n")

#===============================================================================
# SECTION 6: APPLY THE RULE TO ALL HS CODES
#===============================================================================

df_aug25_flagged <- df_aug25 %>%
  mutate(
    coverage = if_else(customs_value > 0,
                       dutiable_value / customs_value,
                       NA_real_),
    tariff_dutiable = if_else(dutiable_value > 0,
                              calculated_duties / dutiable_value,
                              NA_real_),
    tariffed_flag = coverage >= c_star & tariff_dutiable >= r_star
  )

cat("Tariffed flag summary (by count of HS codes):\n")
print(table(df_aug25_flagged$tariffed_flag, useNA = "ifany"))

cat("\nTariffed flag summary (by import value):\n")
print(df_aug25_flagged %>%
        group_by(tariffed_flag) %>%
        summarise(
          total_value = sum(customs_value, na.rm = TRUE),
          share_of_value = total_value / sum(df_aug25_flagged$customs_value, na.rm = TRUE),
          n_codes = n(),
          .groups = "drop"
        ))

#===============================================================================
# SECTION 7: OPTIONAL — BACK OUT AN ETR THRESHOLD & PLOT
#===============================================================================

# ETR threshold from the "tariffed" cluster (e.g., value-weighted 10th percentile)
etr_star <- wtd_quantile(
  tariffed_df$ETR,
  weights = tariffed_df$customs_value,
  probs = 0.10
)

cat("\nDerived ETR threshold (etr_star):", round(etr_star, 4), "\n")

# Compare ETR-based flag vs coverage+rate-based flag
df_aug25_flagged <- df_aug25_flagged %>%
  mutate(
    tariffed_flag_ETR = ETR >= etr_star
  )

cat("\nCross-tab: rule-based vs ETR-based flags:\n")
print(table(rule = df_aug25_flagged$tariffed_flag,
            ETR_flag = df_aug25_flagged$tariffed_flag_ETR,
            useNA = "ifany"))

# Optional plot of clusters in synthetic example
ggplot(df_use, aes(x = coverage, y = tariff_dutiable,
                   color = factor(cluster))) +
  geom_point(alpha = 0.6) +
  geom_vline(xintercept = c_star, linetype = "dashed") +
  geom_hline(yintercept = r_star, linetype = "dashed") +
  labs(
    title = "Coverage vs Tariff on Dutiable Value (Synthetic HS Data)",
    subtitle = paste("Dashed lines show c_star and r_star; tariffed cluster =", tariffed_cluster),
    x = "Coverage (dutiable_value / customs_value)",
    y = "Tariff on dutiable value (duties / dutiable_value)",
    color = "Cluster"
  ) +
  theme_minimal()
