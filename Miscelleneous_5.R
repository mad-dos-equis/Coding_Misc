# ══════════════════════════════════════════════════════════════════════════════
# Section 122 Customs Duties Revenue Projection
# Sources: US Treasury customs duties (MTS); HTS-10 trade-weighted ETR
# Method: Partial equilibrium projection with 2D sensitivity grid
#         over import demand elasticity (ε) and pass-through (φ)
# Baseline: July 2025 (single month)
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(scales)

# ── 0. Parameters ─────────────────────────────────────────────────────────────

TAU_NEW <- 0.15   # Section 122 target rate (statutory cap)

# Pass-through scenarios
# Amiti, Redding & Weinstein (2019, JEP):     full pass-through ~ 1.0
# Intermediate exporter absorption:            partial ~ 0.70
# Cavallo, Cavallo & Rigobon (2021, AER):      incomplete ~ 0.45
phi_scenarios <- tribble(
  ~phi_label,                                               ~phi,
  "Full pass-through \u2014 Amiti et al. (2019)",           1.00,
  "Partial pass-through \u2014 exporter absorption",        0.70,
  "Incomplete pass-through \u2014 Cavallo et al. (2021)",   0.45
)

# Import demand elasticity scenarios
# Boehm, Levchenko & Pandalai-Nayar (2023, AER): short-run ~ -0.5 to -1.0
# Imbs & Mejean (2015, ReStud):                   aggregate ~ -1.25
# Hooper, Johnson & Marquez (2000, Princeton IES): aggregate ~ -1.5
# Broda & Weinstein (2006, QJE):                  aggregate proxy ~ -2.0
elasticity_scenarios <- tribble(
  ~epsilon_label,                                            ~epsilon,
  "Naive (\u03b5 = 0)",                                      0.00,
  "Boehm et al. (2023) \u2014 short-run low",               -0.50,
  "Boehm et al. (2023) \u2014 short-run high",              -1.00,
  "Imbs & Mejean (2015)",                                    -1.25,
  "Hooper, Johnson & Marquez (2000)",                        -1.50,
  "Broda & Weinstein (2006)",                                -2.00
)

# Primary scenarios for HTS-10 decomposition
# Short-run elasticity (Boehm et al.) is appropriate for 150-day window;
# full pass-through (Amiti et al.) is the conservative revenue lower bound
EPSILON_PRIMARY <- -0.75
PHI_PRIMARY     <-  1.00

# Section 122 exempt chapters (2-digit HTS)
# These chapters are treated as having zero marginal rate shock throughout
# all projections, decompositions, and sensitivity analyses.
# Adjust as the proclamation text and legal interpretation develops.
# Current candidates based on standard Section 122 carve-out categories:
#   27 = Mineral fuels and oils (energy)
#   30 = Pharmaceutical products
# Add further chapters as strings, e.g. c("27", "30", "31")
EXEMPT_CHAPTERS <- c("27", "30")

# ── 1. Load and filter HTS-10 data ───────────────────────────────────────────
# July 2025 selected as baseline month. Rationale:
#   - Post-Liberation Day: most trading partners at 10% IEEPA baseline
#   - Post-Geneva (May 14): China at ~30%, avoiding the within-month
#     145%/30% discontinuity present in May 2025
#   - 232 structure (50% steel/aluminum since June 3; autos at 25%;
#     copper and lumber in place) closely matches the regime Section 122
#     layers on top of today
#   - No within-month structural breaks

hts10_raw <- read_csv("your_hts10_data.csv")   # replace with your path

hts10_base <- hts10_raw |>
  rename_with(tolower) |>
  rename(
    import_value = customs_value,
    duties       = calculated_duties,
    tau_eff      = etr
  ) |>
  filter(
    year == 2025,
    month == 7,
    !is.na(import_value),
    !is.na(duties),
    !is.na(tau_eff),
    import_value >= 0,
    tau_eff      >= 0
  ) |>
  mutate(
    chapter = str_pad(str_sub(hts, 1, 2), 2, pad = "0"),
    exempt  = chapter %in% EXEMPT_CHAPTERS
  )

# ── 2. Aggregate baseline inputs ──────────────────────────────────────────────

R_baseline   <- sum(hts10_base$duties)
M_baseline   <- sum(hts10_base$import_value)
tau_baseline <- R_baseline / M_baseline   # bottom-up trade-weighted ETR

cat("── Baseline Summary (July 2025) ──────────────────────────────\n")
cat("Total customs duties:        ", dollar(R_baseline),             "\n")
cat("Total dutiable import value: ", dollar(M_baseline),             "\n")
cat("Implied trade-weighted ETR:  ", percent(tau_baseline, 0.01),    "\n\n")

# ── 3. Projection function ────────────────────────────────────────────────────

project_revenue <- function(epsilon, phi, tau_old, tau_new, R_old) {
  M_old       <- R_old / tau_old
  price_shock <- phi * (tau_new - tau_old) / (1 + tau_old)
  M_new       <- M_old * (1 + epsilon * price_shock)
  R_new       <- tau_new * M_new
  tibble(
    R_projected      = R_new,
    delta_R_abs      = R_new - R_old,
    delta_R_pct      = (R_new / R_old - 1) * 100,
    revenue_multiple = R_new / R_old,
    revenue_150day   = R_new * 5
  )
}

# ── 4. Headline projection: 2D sensitivity grid (ε × φ) ──────────────────────

grid <- cross_join(elasticity_scenarios, phi_scenarios) |>
  mutate(project_revenue(epsilon, phi, tau_baseline, TAU_NEW, R_baseline))

cat("── 2D Sensitivity Grid: Projected Monthly Revenue ────────────\n")
print(
  grid |>
    select(epsilon_label, phi_label, R_projected, delta_R_abs,
           delta_R_pct, revenue_multiple) |>
    mutate(
      across(c(R_projected, delta_R_abs), dollar),
      delta_R_pct      = round(delta_R_pct, 1),
      revenue_multiple = round(revenue_multiple, 3)
    )
)
cat("\n")

# Wide revenue matrix: elasticity (rows) × pass-through (cols)
revenue_matrix <- grid |>
  select(epsilon_label, phi_label, R_projected) |>
  mutate(R_projected = dollar(R_projected)) |>
  pivot_wider(names_from = phi_label, values_from = R_projected)

cat("── Revenue Matrix ─────────────────────────────────────────────\n")
print(revenue_matrix)
cat("\n")

# ── 5. HTS-10 line-level projection (primary ε and φ) ────────────────────────

hts10_projected <- hts10_base |>
  mutate(
    bucket = case_when(
      exempt                           ~ "Exempt \u2014 Section 122 carve-out",
      tau_eff == 0                     ~ "Zero-rated",
      tau_eff >= TAU_NEW               ~ "At/above 15% \u2014 no Section 122 bite",
      tau_eff > 0 & tau_eff < TAU_NEW  ~ "Below 15% \u2014 Section 122 adds margin"
    ),
    delta_tau         = if_else(exempt, 0, pmax(TAU_NEW - tau_eff, 0)),
    price_shock       = PHI_PRIMARY * (delta_tau / (1 + tau_eff)),
    import_value_proj = import_value * (1 + EPSILON_PRIMARY * price_shock),
    tau_eff_proj      = tau_eff + delta_tau,
    duties_proj       = import_value_proj * tau_eff_proj,
    delta_duties      = duties_proj - duties
  )

# ── 6. Bucket decomposition ───────────────────────────────────────────────────

decomp_bucket <- hts10_projected |>
  group_by(bucket) |>
  summarise(
    n_lines           = n(),
    import_value_base = sum(import_value),
    import_value_proj = sum(import_value_proj),
    duties_baseline   = sum(duties),
    duties_projected  = sum(duties_proj),
    delta_duties      = sum(delta_duties),
    share_import_base = sum(import_value) / M_baseline,
    .groups = "drop"
  ) |>
  mutate(
    total_gain    = sum(delta_duties[delta_duties > 0]),
    share_of_gain = if_else(delta_duties > 0, delta_duties / total_gain, NA_real_)
  ) |>
  select(-total_gain)

cat("── Bucket Decomposition ──────────────────────────────────────\n")
print(decomp_bucket)
cat("\n")

# ── 7. Chapter-level breakdown (within "below 15%" bucket) ───────────────────

decomp_chapter <- hts10_projected |>
  filter(bucket == "Below 15% \u2014 Section 122 adds margin") |>
  group_by(chapter) |>
  summarise(
    n_lines           = n(),
    import_value_base = sum(import_value),
    duties_baseline   = sum(duties),
    duties_projected  = sum(duties_proj),
    delta_duties      = sum(delta_duties),
    .groups = "drop"
  ) |>
  mutate(share_of_gain = delta_duties / sum(delta_duties)) |>
  arrange(desc(delta_duties))

cat("── Top 20 HTS Chapters Driving Revenue Gain ──────────────────\n")
print(decomp_chapter |> slice_head(n = 20))
cat("\n")

# ── 8. Bucket decomposition sensitivity over ε × φ ───────────────────────────

bucket_sensitivity <- cross_join(elasticity_scenarios, phi_scenarios) |>
  mutate(
    results = map2(epsilon, phi, function(eps, ph) {
      hts10_base |>
        mutate(
          bucket      = case_when(
            exempt                          ~ "exempt",
            tau_eff == 0                    ~ "zero",
            tau_eff >= TAU_NEW              ~ "above",
            tau_eff > 0 & tau_eff < TAU_NEW ~ "below"
          ),
          delta_tau         = if_else(exempt, 0, pmax(TAU_NEW - tau_eff, 0)),
          price_shock       = ph * (delta_tau / (1 + tau_eff)),
          import_value_proj = import_value * (1 + eps * price_shock),
          duties_proj       = import_value_proj * (tau_eff + delta_tau),
          delta_duties      = duties_proj - duties
        ) |>
        summarise(
          total_delta_duties = sum(delta_duties),
          below_delta        = sum(delta_duties[bucket == "below"]),
          above_delta        = sum(delta_duties[bucket == "above"]),
          exempt_delta       = sum(delta_duties[bucket == "exempt"])
        )
    })
  ) |>
  unnest(results)

cat("── Bucket Gain Sensitivity (\u03b5 \u00d7 \u03c6) ─────────────────────────────\n")
print(bucket_sensitivity)
cat("\n")

# ── 9. Illustrative long-run revenue scenarios ────────────────────────────────
# Section 122 is capped at 150 days. The table below is explicitly illustrative:
# it projects cumulative revenue under different assumptions about what authority
# (if any) replaces Section 122 after expiry. These are NOT forecasts; they are
# sensitivity scenarios for planning purposes only.
#
# Regime assumptions:
#   A. Section 122 lapses after 150 days, no replacement → 0 incremental revenue
#   B. Section 122 renewed/replaced at equivalent rate for full 10 years
#   C. Partial replacement: equivalent authority in place for 50% of the decade
#      (reflecting likely legal/political attrition over time)
#
# All scenarios use the primary ε and φ. Monthly projection scaled by number
# of months active in each scenario.

R_primary <- grid |>
  filter(
    epsilon == EPSILON_PRIMARY,
    phi     == PHI_PRIMARY
  ) |>
  pull(R_projected)

delta_R_monthly <- R_primary - R_baseline   # incremental monthly gain

scenario_table <- tibble(
  scenario = c(
    "A. Section 122 lapses (150 days only)",
    "B. Full replacement — 10 years at equivalent rate",
    "C. Partial replacement — 50% of decade"
  ),
  months_active    = c(5, 120, 60),
  assumption       = c(
    "No successor authority; tariff reverts to 232-only baseline after day 150",
    "Illustrative upper bound: Section 122 renewed or replaced by equivalent authority continuously",
    "Illustrative midpoint: successor authority faces legal/political attrition, in effect ~half the decade"
  )
) |>
  mutate(
    cumulative_delta_revenue = delta_R_monthly * months_active,
    cumulative_total_revenue = R_primary * months_active
  )

cat("── Illustrative Long-Run Revenue Scenarios (PLANNING ONLY) ───\n")
cat("Primary parameters: \u03b5 =", EPSILON_PRIMARY, "(Boehm et al. 2023),",
    "\u03c6 =", PHI_PRIMARY, "(Amiti et al. 2019)\n")
cat("Monthly incremental gain over baseline: ", dollar(delta_R_monthly), "\n\n")
print(
  scenario_table |>
    select(scenario, months_active, cumulative_delta_revenue,
           cumulative_total_revenue, assumption) |>
    mutate(
      across(c(cumulative_delta_revenue, cumulative_total_revenue), dollar)
    )
)
cat("\nNOTE: Scenarios B and C are purely illustrative. Section 122 expires\n")
cat("after 150 days absent congressional extension. Any successor authority\n")
cat("would require independent legal basis (e.g., Section 232 investigations\n")
cat("currently underway for pharmaceuticals, semiconductors, etc.).\n\n")

# ── 10. Visualizations ───────────────────────────────────────────────────────

# 10a. Heatmap: revenue multiple across ε × φ
p_heatmap <- grid |>
  mutate(
    epsilon_label = fct_reorder(epsilon_label, epsilon),
    phi_label     = fct_reorder(phi_label, phi)
  ) |>
  ggplot(aes(x = phi_label, y = epsilon_label, fill = revenue_multiple)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = percent(revenue_multiple, accuracy = 0.1)),
            size = 3.5, color = "white", fontface = "bold") +
  scale_fill_gradient(
    low    = "#5B9BD5",
    high   = "#1F4E79",
    name   = "R\u2081\u2085 / R\u2081\u2080",
    labels = percent
  ) +
  labs(
    title    = "Revenue Multiple: 15% Section 122 vs. July 2025 Baseline",
    subtitle = "Sensitivity to import demand elasticity (\u03b5) and pass-through (\u03c6)",
    x        = "Pass-Through (\u03c6)",
    y        = "Import Demand Elasticity (\u03b5)",
    caption  = paste0(
      "Baseline: July 2025. ",
      "Sources: Amiti et al. (2019, JEP); Cavallo et al. (2021, AER);\n",
      "Boehm et al. (2023, AER); Hooper, Johnson & Marquez (2000); ",
      "Imbs & Mejean (2015, ReStud); Broda & Weinstein (2006, QJE)."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x     = element_text(angle = 15, hjust = 1),
    plot.caption    = element_text(size = 8, color = "gray40"),
    legend.position = "right"
  )

# 10b. Bucket decomposition bar chart (primary ε and φ)
p_bucket <- decomp_bucket |>
  ggplot(aes(
    x    = fct_reorder(bucket, delta_duties),
    y    = delta_duties / 1e6,
    fill = bucket
  )) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  coord_flip() +
  scale_fill_manual(values = c(
    "Below 15% \u2014 Section 122 adds margin" = "#1F4E79",
    "At/above 15% \u2014 no Section 122 bite"  = "gray70",
    "Zero-rated"                               = "gray90",
    "Exempt \u2014 Section 122 carve-out"      = "#D4A017"
  )) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title    = "Projected Duty Change by Tariff Bucket",
    subtitle = "Section 122 at 15% vs. July 2025 effective rates",
    x        = NULL,
    y        = "Change in Monthly Duties (USD Millions)",
    caption  = paste0(
      "\u03b5 = ", EPSILON_PRIMARY, " (Boehm et al. 2023, short-run midpoint); ",
      "\u03c6 = ", PHI_PRIMARY, " (Amiti et al. 2019). Baseline: July 2025."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(size = 8, color = "gray40"))

# 10c. Top chapters driving the gain
p_chapter <- decomp_chapter |>
  slice_head(n = 15) |>
  ggplot(aes(
    x = fct_reorder(chapter, delta_duties),
    y = delta_duties / 1e6
  )) +
  geom_col(fill = "#1F4E79") +
  coord_flip() +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title    = "Top HTS Chapters Driving Section 122 Revenue Gain",
    subtitle = "Lines with July 2025 ETR below 15% only",
    x        = "HTS Chapter",
    y        = "Projected Duty Gain (USD Millions)",
    caption  = paste0(
      "\u03b5 = ", EPSILON_PRIMARY, " (Boehm et al. 2023); ",
      "\u03c6 = ", PHI_PRIMARY, " (Amiti et al. 2019). Baseline: July 2025."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(size = 8, color = "gray40"))

# 10d. Faceted bar: bucket gain sensitivity across φ scenarios
#     (primary elasticity, varying phi)
p_bucket_phi <- phi_scenarios |>
  mutate(
    bucket_data = map(phi, function(ph) {
      hts10_base |>
        mutate(
          bucket      = case_when(
            exempt                          ~ "Exempt",
            tau_eff == 0                    ~ "Zero-rated",
            tau_eff >= TAU_NEW              ~ "At/above 15%",
            tau_eff > 0 & tau_eff < TAU_NEW ~ "Below 15%"
          ),
          delta_tau         = if_else(exempt, 0, pmax(TAU_NEW - tau_eff, 0)),
          price_shock       = ph * (delta_tau / (1 + tau_eff)),
          import_value_proj = import_value * (1 + EPSILON_PRIMARY * price_shock),
          duties_proj       = import_value_proj * (tau_eff + delta_tau),
          delta_duties      = duties_proj - duties
        ) |>
        group_by(bucket) |>
        summarise(delta_duties = sum(delta_duties), .groups = "drop")
    })
  ) |>
  unnest(bucket_data) |>
  ggplot(aes(
    x    = fct_reorder(bucket, delta_duties),
    y    = delta_duties / 1e6,
    fill = bucket
  )) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~phi_label, ncol = 1) +
  scale_fill_manual(values = c(
    "Below 15%"    = "#1F4E79",
    "At/above 15%" = "gray70",
    "Zero-rated"   = "gray90",
    "Exempt"       = "#D4A017"
  )) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title    = "Bucket Revenue Gain by Pass-Through Scenario",
    subtitle = paste0(
      "\u03b5 = ", EPSILON_PRIMARY,
      " (Boehm et al. 2023, short-run midpoint). Baseline: July 2025."
    ),
    x = NULL,
    y = "Change in Monthly Duties (USD Millions)"
  ) +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

# ── 11. Print plots ───────────────────────────────────────────────────────────

print(p_heatmap)
print(p_bucket)
print(p_chapter)
print(p_bucket_phi)
