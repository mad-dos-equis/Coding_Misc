# ══════════════════════════════════════════════════════════════════════════════
# Section 122 Customs Duties Revenue Projection — Extensions
# ══════════════════════════════════════════════════════════════════════════════
# Source after running the main projection script. Assumes the following
# objects exist in the environment:
#   hts10_base, hts10_projected, grid, decomp_bucket, decomp_chapter,
#   bucket_sensitivity, scenario_table, revenue_matrix,
#   elasticity_scenarios, phi_scenarios,
#   R_baseline, M_baseline, tau_baseline,
#   TAU_NEW, EPSILON_PRIMARY, PHI_PRIMARY, EXEMPT_CHAPTERS
#
# Additions:
#   A. Baseline summary table
#   B. Rate effect vs. volume effect (Harberger) decomposition
#   C. Concentration metrics for revenue gain
#   D. Extensive-margin scenario for zero-rated lines
#   E. Foregone revenue from Section 122 exemptions / carve-outs
#   F. HTS-4 breakdown within top revenue-gaining chapters
#      (with trade-weighted + simple-average ETR; marginal rate columns)
#   G. Annualized and 10-year projections with full ε × φ sensitivity
#   H. Visualizations for new outputs
#   I. Export: CSV collection + optional multi-sheet xlsx workbook
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(scales)

export_dir <- "output_tables"
dir.create(export_dir, showWarnings = FALSE, recursive = TRUE)

fmt_dollar <- function(x) dollar(x, accuracy = 1)
fmt_pct    <- function(x) percent(x, accuracy = 0.01)


# ══════════════════════════════════════════════════════════════════════════════
# A. Baseline Summary Table
# ══════════════════════════════════════════════════════════════════════════════
# One-row context table: anchors every downstream number.

n_exempt_lines <- sum(hts10_base$exempt)
n_zero_lines   <- sum(hts10_base$tau_eff == 0 & !hts10_base$exempt)
n_below_lines  <- sum(hts10_base$tau_eff > 0 & hts10_base$tau_eff < TAU_NEW & !hts10_base$exempt)
n_above_lines  <- sum(hts10_base$tau_eff >= TAU_NEW & !hts10_base$exempt)

baseline_summary <- tibble(
  baseline_month            = "July 2025",
  total_hts10_lines         = nrow(hts10_base),
  n_exempt_lines            = n_exempt_lines,
  n_zero_rated_lines        = n_zero_lines,
  n_below_15_lines          = n_below_lines,
  n_at_above_15_lines       = n_above_lines,
  total_import_value        = M_baseline,
  total_duties              = R_baseline,
  trade_weighted_etr        = tau_baseline,
  simple_avg_etr            = mean(hts10_base$tau_eff),
  median_etr                = median(hts10_base$tau_eff),
  import_value_exempt       = sum(hts10_base$import_value[hts10_base$exempt]),
  import_value_zero_rated   = sum(hts10_base$import_value[hts10_base$tau_eff == 0 & !hts10_base$exempt]),
  import_value_below_15     = sum(hts10_base$import_value[hts10_base$tau_eff > 0 & hts10_base$tau_eff < TAU_NEW & !hts10_base$exempt]),
  import_value_at_above_15  = sum(hts10_base$import_value[hts10_base$tau_eff >= TAU_NEW & !hts10_base$exempt]),
  exempt_chapters           = paste(EXEMPT_CHAPTERS, collapse = ", "),
  section_122_target_rate   = TAU_NEW,
  epsilon_primary           = EPSILON_PRIMARY,
  phi_primary               = PHI_PRIMARY
)

cat("══ A. Baseline Summary (July 2025) ═══════════════════════════\n")
cat("Total HTS-10 lines:       ", comma(nrow(hts10_base)), "\n")
cat("  Exempt:                 ", comma(n_exempt_lines), "\n")
cat("  Zero-rated:             ", comma(n_zero_lines), "\n")
cat("  Below 15%:              ", comma(n_below_lines), "\n")
cat("  At/above 15%:           ", comma(n_above_lines), "\n")
cat("Total import value:       ", fmt_dollar(M_baseline), "\n")
cat("Total duties:             ", fmt_dollar(R_baseline), "\n")
cat("Trade-weighted ETR:       ", fmt_pct(tau_baseline), "\n")
cat("Simple-average ETR:       ", fmt_pct(mean(hts10_base$tau_eff)), "\n\n")


# ══════════════════════════════════════════════════════════════════════════════
# B. Rate Effect vs. Volume Effect (Harberger Decomposition)
# ══════════════════════════════════════════════════════════════════════════════
# Total ΔR = rate_effect + volume_effect
#   rate_effect   = (τ_new − τ_old) × M_old       [mechanical gain from higher rate]
#   volume_effect = τ_new × (M_new − M_old)        [revenue loss from import contraction]
# These sum exactly to ΔR = τ_new × M_new − τ_old × M_old.

hts10_harberger <- hts10_projected |>
  mutate(
    rate_effect   = delta_tau * import_value,
    volume_effect = tau_eff_proj * (import_value_proj - import_value),
    # Verify: rate_effect + volume_effect should equal delta_duties
    check         = abs((rate_effect + volume_effect) - delta_duties)
  )

# Sanity check
max_decomp_error <- max(hts10_harberger$check)
if (max_decomp_error > 1) {
  warning("Harberger decomposition error exceeds $1: ", fmt_dollar(max_decomp_error))
}

harberger_bucket <- hts10_harberger |>
  group_by(bucket) |>
  summarise(
    rate_effect       = sum(rate_effect),
    volume_effect     = sum(volume_effect),
    net_delta_duties  = sum(delta_duties),
    pct_rate_effect   = sum(rate_effect) / sum(delta_duties),
    pct_volume_effect = sum(volume_effect) / sum(delta_duties),
    .groups = "drop"
  )

harberger_chapter <- hts10_harberger |>
  filter(bucket == "Below 15% \u2014 Section 122 adds margin") |>
  group_by(chapter) |>
  summarise(
    rate_effect       = sum(rate_effect),
    volume_effect     = sum(volume_effect),
    net_delta_duties  = sum(delta_duties),
    pct_rate_effect   = rate_effect / net_delta_duties,
    pct_volume_effect = volume_effect / net_delta_duties,
    .groups = "drop"
  ) |>
  arrange(desc(net_delta_duties))

harberger_total <- hts10_harberger |>
  summarise(
    rate_effect      = sum(rate_effect),
    volume_effect    = sum(volume_effect),
    net_delta_duties = sum(delta_duties)
  )

cat("══ B. Harberger Decomposition: Rate vs. Volume Effect ════════\n")
cat("Primary parameters: ε =", EPSILON_PRIMARY, ", φ =", PHI_PRIMARY, "\n\n")
cat("Aggregate:\n")
cat("  Gross rate effect:     ", fmt_dollar(harberger_total$rate_effect),
    " (", round(harberger_total$rate_effect / harberger_total$net_delta_duties * 100, 1), "%)\n", sep = "")
cat("  Volume offset:         ", fmt_dollar(harberger_total$volume_effect),
    " (", round(harberger_total$volume_effect / harberger_total$net_delta_duties * 100, 1), "%)\n", sep = "")
cat("  Net revenue gain:      ", fmt_dollar(harberger_total$net_delta_duties), "\n\n")
cat("By bucket:\n")
print(harberger_bucket)
cat("\nBy chapter (below 15% bucket, top 15):\n")
print(harberger_chapter |> slice_head(n = 15))
cat("\n")


# ══════════════════════════════════════════════════════════════════════════════
# C. Concentration Metrics
# ══════════════════════════════════════════════════════════════════════════════

total_gain <- sum(decomp_chapter$delta_duties)

concentration <- decomp_chapter |>
  mutate(
    cumulative_gain  = cumsum(delta_duties),
    cumulative_share = cumulative_gain / total_gain
  )

top5_share  <- concentration |> slice_head(n = 5)  |> pull(cumulative_share) |> last()
top10_share <- concentration |> slice_head(n = 10) |> pull(cumulative_share) |> last()

# Herfindahl at chapter level (on delta_duties shares)
hhi_chapter <- decomp_chapter |>
  mutate(share = delta_duties / total_gain) |>
  summarise(hhi = sum(share^2)) |>
  pull(hhi)

concentration_summary <- tibble(
  metric                     = c("Top 5 chapters share", "Top 10 chapters share",
                                  "Chapter-level HHI", "Number of chapters with gain"),
  value                      = c(top5_share, top10_share, hhi_chapter,
                                  sum(decomp_chapter$delta_duties > 0)),
  formatted                  = c(fmt_pct(top5_share), fmt_pct(top10_share),
                                  round(hhi_chapter, 4),
                                  sum(decomp_chapter$delta_duties > 0))
)

cat("══ C. Revenue Gain Concentration ═════════════════════════════\n")
cat("Top 5 chapters:  ", fmt_pct(top5_share), "of projected gain\n")
cat("Top 10 chapters: ", fmt_pct(top10_share), "of projected gain\n")
cat("Chapter-level HHI:", round(hhi_chapter, 4), "\n")
cat("Chapters with positive gain:", sum(decomp_chapter$delta_duties > 0), "\n\n")


# ══════════════════════════════════════════════════════════════════════════════
# D. Extensive-Margin Scenario for Zero-Rated Lines
# ══════════════════════════════════════════════════════════════════════════════
# The behavioral response to 0% → 15% is plausibly larger than 5% → 15%.
# Model this with a separate elasticity multiplier for zero-rated lines.
# Default: 1.5× the primary elasticity for zero-rated products.
# This is a scenario, not a correction — reported alongside the standard run.

ZERO_RATE_ELAST_MULTIPLIER <- 1.5

hts10_extensive <- hts10_base |>
  mutate(
    bucket = case_when(
      exempt                           ~ "Exempt \u2014 Section 122 carve-out",
      tau_eff == 0                     ~ "Zero-rated",
      tau_eff >= TAU_NEW               ~ "At/above 15% \u2014 no Section 122 bite",
      tau_eff > 0 & tau_eff < TAU_NEW  ~ "Below 15% \u2014 Section 122 adds margin"
    ),
    is_zero_rated     = (tau_eff == 0 & !exempt),
    epsilon_applied   = if_else(is_zero_rated,
                                EPSILON_PRIMARY * ZERO_RATE_ELAST_MULTIPLIER,
                                EPSILON_PRIMARY),
    delta_tau         = if_else(exempt, 0, pmax(TAU_NEW - tau_eff, 0)),
    price_shock       = PHI_PRIMARY * (delta_tau / (1 + tau_eff)),
    import_value_proj = import_value * (1 + epsilon_applied * price_shock),
    tau_eff_proj      = tau_eff + delta_tau,
    duties_proj       = import_value_proj * tau_eff_proj,
    delta_duties      = duties_proj - duties
  )

extensive_bucket <- hts10_extensive |>
  group_by(bucket) |>
  summarise(
    n_lines           = n(),
    import_value_base = sum(import_value),
    import_value_proj = sum(import_value_proj),
    duties_baseline   = sum(duties),
    duties_projected  = sum(duties_proj),
    delta_duties      = sum(delta_duties),
    .groups = "drop"
  )

extensive_total <- sum(hts10_extensive$duties_proj)
standard_total  <- sum(hts10_projected$duties_proj)

extensive_comparison <- tibble(
  scenario                = c("Standard (uniform ε)", "Extensive-margin (1.5× ε for zero-rated)"),
  epsilon_zero_rated      = c(EPSILON_PRIMARY, EPSILON_PRIMARY * ZERO_RATE_ELAST_MULTIPLIER),
  epsilon_other           = c(EPSILON_PRIMARY, EPSILON_PRIMARY),
  monthly_projected       = c(standard_total, extensive_total),
  monthly_delta           = c(standard_total - R_baseline, extensive_total - R_baseline),
  pct_change_from_base    = c((standard_total / R_baseline - 1) * 100,
                               (extensive_total / R_baseline - 1) * 100)
)

cat("══ D. Extensive-Margin Scenario for Zero-Rated Lines ═════════\n")
cat("Multiplier for zero-rated ε:", ZERO_RATE_ELAST_MULTIPLIER, "×\n")
cat("ε (zero-rated):", EPSILON_PRIMARY * ZERO_RATE_ELAST_MULTIPLIER,
    "  ε (other):", EPSILON_PRIMARY, "\n\n")
cat("Comparison:\n")
print(extensive_comparison |>
        mutate(across(c(monthly_projected, monthly_delta), fmt_dollar),
               pct_change_from_base = round(pct_change_from_base, 1)))
cat("\nBucket detail (extensive-margin scenario):\n")
print(extensive_bucket)
cat("\n")


# ══════════════════════════════════════════════════════════════════════════════
# E. Foregone Revenue from Section 122 Exemptions / Carve-Outs
# ══════════════════════════════════════════════════════════════════════════════

exempt_counterfactual <- hts10_base |>
  filter(exempt) |>
  mutate(
    delta_tau_cf     = pmax(TAU_NEW - tau_eff, 0),
    price_shock_cf   = PHI_PRIMARY * (delta_tau_cf / (1 + tau_eff)),
    import_value_cf  = import_value * (1 + EPSILON_PRIMARY * price_shock_cf),
    tau_eff_cf       = tau_eff + delta_tau_cf,
    duties_cf        = import_value_cf * tau_eff_cf,
    foregone_duties  = duties_cf - duties
  )

foregone_by_chapter <- exempt_counterfactual |>
  group_by(chapter) |>
  summarise(
    n_lines             = n(),
    import_value_base   = sum(import_value),
    duties_baseline     = sum(duties),
    duties_counterfact  = sum(duties_cf),
    foregone_revenue    = sum(foregone_duties),
    tw_avg_etr          = weighted.mean(tau_eff, import_value),
    simple_avg_etr      = mean(tau_eff),
    share_below_15      = sum(import_value[tau_eff < TAU_NEW]) / sum(import_value),
    .groups = "drop"
  ) |>
  arrange(desc(foregone_revenue))

foregone_total <- sum(foregone_by_chapter$foregone_revenue)

cat("══ E. Foregone Revenue from Section 122 Exemptions ═══════════\n")
cat("Exempt chapters:", paste(EXEMPT_CHAPTERS, collapse = ", "), "\n")
cat("Monthly foregone revenue:    ", fmt_dollar(foregone_total), "\n")
cat("Annualized foregone:         ", fmt_dollar(foregone_total * 12), "\n")
cat("150-day foregone:            ", fmt_dollar(foregone_total * 5), "\n\n")
print(foregone_by_chapter)
cat("\n")

# Sensitivity grid for foregone revenue
foregone_sensitivity <- cross_join(elasticity_scenarios, phi_scenarios) |>
  mutate(
    foregone = map2_dbl(epsilon, phi, function(eps, ph) {
      hts10_base |>
        filter(exempt) |>
        mutate(
          delta_tau_cf    = pmax(TAU_NEW - tau_eff, 0),
          price_shock_cf  = ph * (delta_tau_cf / (1 + tau_eff)),
          import_value_cf = import_value * (1 + eps * price_shock_cf),
          duties_cf       = import_value_cf * (tau_eff + delta_tau_cf),
          foregone        = duties_cf - duties
        ) |>
        pull(foregone) |>
        sum()
    }),
    foregone_annual = foregone * 12,
    foregone_150day = foregone * 5
  )

foregone_matrix <- foregone_sensitivity |>
  select(epsilon_label, phi_label, foregone) |>
  pivot_wider(names_from = phi_label, values_from = foregone)

cat("Foregone revenue sensitivity (ε × φ), monthly:\n")
print(foregone_matrix |>
        mutate(across(-epsilon_label, fmt_dollar)))
cat("\n")


# ══════════════════════════════════════════════════════════════════════════════
# F. HTS-4 Breakdown Within Top Revenue-Gaining Chapters
# ══════════════════════════════════════════════════════════════════════════════
# Includes: trade-weighted ETR, simple-average ETR, delta_tau (marginal
# rate increase), and Harberger decomposition at HTS-4 level.

TOP_N_CHAPTERS <- 10

top_chapters <- decomp_chapter |>
  slice_head(n = TOP_N_CHAPTERS) |>
  pull(chapter)

hts4_decomp <- hts10_harberger |>
  filter(
    bucket == "Below 15% \u2014 Section 122 adds margin",
    chapter %in% top_chapters
  ) |>
  mutate(hts4 = str_sub(hts, 1, 4)) |>
  group_by(chapter, hts4) |>
  summarise(
    n_lines           = n(),
    import_value_base = sum(import_value),
    import_value_proj = sum(import_value_proj),
    duties_baseline   = sum(duties),
    duties_projected  = sum(duties_proj),
    delta_duties      = sum(delta_duties),
    rate_effect       = sum(rate_effect),
    volume_effect     = sum(volume_effect),
    tw_avg_etr_pre    = weighted.mean(tau_eff, import_value),
    tw_avg_etr_post   = weighted.mean(tau_eff_proj, import_value_proj),
    simple_avg_etr_pre  = mean(tau_eff),
    simple_avg_etr_post = mean(tau_eff_proj),
    tw_avg_delta_tau    = weighted.mean(delta_tau, import_value),
    simple_avg_delta_tau = mean(delta_tau),
    .groups = "drop"
  ) |>
  mutate(
    share_of_chapter_gain = delta_duties / sum(delta_duties),
    pct_volume_offset     = if_else(rate_effect != 0,
                                     abs(volume_effect) / rate_effect * 100,
                                     NA_real_)
  ) |>
  arrange(chapter, desc(delta_duties))

cat("══ F. HTS-4 Breakdown: Top", TOP_N_CHAPTERS, "Chapters ══════════════════\n")
cat("Lines where July 2025 ETR < 15% (Section 122 adds margin)\n")
cat("Includes Harberger decomposition and dual ETR measures\n\n")

for (ch in top_chapters) {
  ch_data <- hts4_decomp |> filter(chapter == ch) |> slice_head(n = 10)
  ch_gain <- sum(ch_data$delta_duties)
  cat("Chapter ", ch, " — Top HTS-4 codes (total chapter gain: ",
      fmt_dollar(ch_gain), "):\n", sep = "")
  print(ch_data |>
          select(hts4, n_lines, import_value_base, delta_duties,
                 rate_effect, volume_effect,
                 tw_avg_etr_pre, tw_avg_etr_post, tw_avg_delta_tau))
  cat("\n")
}

# Top 30 HTS-4 codes overall
hts4_top30 <- hts4_decomp |>
  arrange(desc(delta_duties)) |>
  slice_head(n = 30) |>
  mutate(cumulative_share = cumsum(delta_duties) / sum(hts4_decomp$delta_duties))

cat("── Top 30 HTS-4 Codes Overall by Revenue Gain ────────────────\n")
print(hts4_top30 |>
        select(chapter, hts4, n_lines, import_value_base, delta_duties,
               rate_effect, volume_effect,
               tw_avg_etr_pre, tw_avg_etr_post, cumulative_share))
cat("\n")


# ══════════════════════════════════════════════════════════════════════════════
# G. Annualized and 10-Year Projections with Full ε × φ Sensitivity
# ══════════════════════════════════════════════════════════════════════════════

horizons <- tribble(
  ~horizon_label,                                         ~months,
  "150-day (Section 122 statutory window)",                    5,
  "Annual (12-month projection)",                             12,
  "Partial replacement (50% of decade)",                      60,
  "Full replacement (10-year illustrative upper bound)",     120
)

longrun_grid <- cross_join(grid, horizons) |>
  mutate(
    cumulative_projected = R_projected * months,
    cumulative_baseline  = R_baseline * months,
    cumulative_delta     = delta_R_abs * months
  ) |>
  select(
    epsilon_label, epsilon,
    phi_label, phi,
    horizon_label, months,
    monthly_projected = R_projected,
    monthly_delta     = delta_R_abs,
    cumulative_projected,
    cumulative_baseline,
    cumulative_delta
  )

longrun_summary <- longrun_grid |>
  group_by(horizon_label, months) |>
  summarise(
    min_cumulative_delta = min(cumulative_delta),
    max_cumulative_delta = max(cumulative_delta),
    min_cumulative_total = min(cumulative_projected),
    max_cumulative_total = max(cumulative_projected),
    .groups = "drop"
  ) |>
  arrange(months)

cat("══ G. Long-Run Projections ═══════════════════════════════════\n")
cat("NOTE: Horizons beyond 150 days are purely illustrative.\n")
cat("Section 122 expires after 150 days absent congressional action.\n\n")
cat("Cumulative revenue ranges across all ε × φ combinations:\n")
print(longrun_summary |>
        mutate(across(starts_with("min_") | starts_with("max_"), fmt_dollar)))
cat("\n")

# Wide matrices per horizon
horizon_matrices <- list()
for (h in unique(longrun_grid$horizon_label)) {
  mat <- longrun_grid |>
    filter(horizon_label == h) |>
    select(epsilon_label, phi_label, cumulative_projected) |>
    pivot_wider(names_from = phi_label, values_from = cumulative_projected)
  horizon_matrices[[h]] <- mat
}


# ══════════════════════════════════════════════════════════════════════════════
# H. Additional Visualizations
# ══════════════════════════════════════════════════════════════════════════════

# H1. Foregone revenue by exempt chapter
p_foregone <- foregone_by_chapter |>
  ggplot(aes(
    x = fct_reorder(chapter, foregone_revenue),
    y = foregone_revenue / 1e6
  )) +
  geom_col(fill = "#D4A017") +
  coord_flip() +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title    = "Monthly Foregone Revenue from Section 122 Exemptions",
    subtitle = paste0(
      "Counterfactual: exempt chapters (",
      paste(EXEMPT_CHAPTERS, collapse = ", "),
      ") subjected to 15% floor"
    ),
    x       = "HTS Chapter",
    y       = "Foregone Monthly Duties (USD Millions)",
    caption = paste0(
      "\u03b5 = ", EPSILON_PRIMARY, "; \u03c6 = ", PHI_PRIMARY,
      ". Baseline: July 2025."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(size = 8, color = "gray40"))

# H2. Harberger decomposition — stacked bar by chapter
harberger_plot_data <- harberger_chapter |>
  slice_head(n = 15) |>
  select(chapter, rate_effect, volume_effect) |>
  pivot_longer(cols = c(rate_effect, volume_effect),
               names_to = "component", values_to = "value") |>
  mutate(
    component = case_when(
      component == "rate_effect"   ~ "Rate effect (mechanical gain)",
      component == "volume_effect" ~ "Volume effect (demand offset)"
    )
  )

p_harberger <- harberger_plot_data |>
  ggplot(aes(
    x    = fct_reorder(chapter, value, .fun = sum),
    y    = value / 1e6,
    fill = component
  )) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c(
    "Rate effect (mechanical gain)"  = "#1F4E79",
    "Volume effect (demand offset)"  = "#C0392B"
  )) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title    = "Harberger Decomposition: Rate vs. Volume Effect by Chapter",
    subtitle = "Top 15 chapters in the 'Below 15%' bucket",
    x        = "HTS Chapter",
    y        = "Monthly Duty Change (USD Millions)",
    fill     = NULL,
    caption  = paste0(
      "\u03b5 = ", EPSILON_PRIMARY, "; \u03c6 = ", PHI_PRIMARY,
      ". Rate effect = \u0394\u03c4 \u00d7 M_old; ",
      "Volume effect = \u03c4_new \u00d7 \u0394M. Baseline: July 2025."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.caption    = element_text(size = 8, color = "gray40")
  )

# H3. Extensive margin comparison — paired bar
p_extensive <- extensive_comparison |>
  ggplot(aes(
    x    = fct_rev(scenario),
    y    = monthly_delta / 1e9,
    fill = scenario
  )) +
  geom_col(show.legend = FALSE, width = 0.6) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Standard (uniform \u03b5)"                          = "#1F4E79",
    "Extensive-margin (1.5\u00d7 \u03b5 for zero-rated)" = "#5B9BD5"
  )) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title    = "Impact of Extensive-Margin Adjustment on Revenue Projection",
    subtitle = paste0(
      "Zero-rated lines: \u03b5 = ",
      EPSILON_PRIMARY * ZERO_RATE_ELAST_MULTIPLIER,
      " vs. uniform \u03b5 = ", EPSILON_PRIMARY
    ),
    x = NULL,
    y = "Monthly Incremental Revenue (USD Billions)",
    caption = paste0(
      "Zero-rated elasticity multiplier: ", ZERO_RATE_ELAST_MULTIPLIER,
      "\u00d7. \u03c6 = ", PHI_PRIMARY, ". Baseline: July 2025."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.caption = element_text(size = 8, color = "gray40"))

print(p_foregone)
print(p_harberger)
print(p_extensive)


# ══════════════════════════════════════════════════════════════════════════════
# I. Export: CSV + Optional Multi-Sheet xlsx
# ══════════════════════════════════════════════════════════════════════════════

# I1. CSV exports ─────────────────────────────────────────────────────────────

exports <- list(
  "01_baseline_summary"             = baseline_summary,
  "02_sensitivity_grid"             = grid |>
    mutate(R_projected_annual = R_projected * 12,
           delta_R_abs_annual = delta_R_abs * 12,
           revenue_10yr       = R_projected * 120),
  "03_revenue_matrix_wide"          = revenue_matrix |>
    left_join(elasticity_scenarios |> select(epsilon_label, epsilon),
              by = "epsilon_label"),
  "04_bucket_decomposition"         = decomp_bucket,
  "05_chapter_decomposition"        = decomp_chapter,
  "06_harberger_by_bucket"          = harberger_bucket,
  "07_harberger_by_chapter"         = harberger_chapter,
  "08_concentration_summary"        = concentration_summary,
  "09_concentration_cumulative"     = concentration,
  "10_extensive_margin_comparison"  = extensive_comparison,
  "11_extensive_margin_buckets"     = extensive_bucket,
  "12_foregone_by_chapter"          = foregone_by_chapter,
  "13_foregone_sensitivity"         = foregone_sensitivity,
  "14_foregone_matrix_wide"         = foregone_matrix,
  "15_bucket_sensitivity"           = bucket_sensitivity,
  "16_scenario_table"               = scenario_table,
  "17_hts4_top_chapters"            = hts4_decomp,
  "18_hts4_top30_overall"           = hts4_top30,
  "19_longrun_full_grid"            = longrun_grid,
  "20_longrun_summary"              = longrun_summary
)

# Write horizon matrices
for (h in names(horizon_matrices)) {
  safe_name <- str_replace_all(tolower(h), "[^a-z0-9]+", "_") |>
    str_remove("_$")
  key <- paste0("21_matrix_", safe_name)
  exports[[key]] <- horizon_matrices[[h]]
}

walk2(names(exports), exports, function(nm, df) {
  write_csv(df, file.path(export_dir, paste0(nm, ".csv")))
})

# I2. Multi-sheet xlsx (optional) ─────────────────────────────────────────────

if (requireNamespace("openxlsx", quietly = TRUE)) {
  library(openxlsx)

  wb <- createWorkbook()

  # Clean sheet names (max 31 chars for Excel)
  sheet_map <- list(
    "Baseline Summary"         = baseline_summary,
    "Sensitivity Grid"         = exports[["02_sensitivity_grid"]],
    "Revenue Matrix"           = exports[["03_revenue_matrix_wide"]],
    "Bucket Decomposition"     = decomp_bucket,
    "Chapter Decomposition"    = decomp_chapter,
    "Harberger by Bucket"      = harberger_bucket,
    "Harberger by Chapter"     = harberger_chapter,
    "Concentration"            = concentration_summary,
    "Extensive Margin"         = extensive_comparison,
    "Foregone by Chapter"      = foregone_by_chapter,
    "Foregone Sensitivity"     = foregone_sensitivity,
    "Bucket Sensitivity"       = bucket_sensitivity,
    "Scenario Table"           = scenario_table,
    "HTS-4 Top Chapters"      = hts4_decomp,
    "HTS-4 Top 30"             = hts4_top30,
    "Long-Run Grid"            = longrun_grid,
    "Long-Run Summary"         = longrun_summary
  )

  bold_style <- createStyle(textDecoration = "bold", halign = "center")

  for (nm in names(sheet_map)) {
    df <- sheet_map[[nm]]
    addWorksheet(wb, nm)
    writeData(wb, nm, df)
    setColWidths(wb, nm, cols = seq_len(ncol(df)), widths = "auto")
    addStyle(wb, nm, style = bold_style,
             rows = 1, cols = seq_len(ncol(df)), gridExpand = TRUE)
  }

  xlsx_path <- file.path(export_dir, "section122_projection_tables.xlsx")
  saveWorkbook(wb, xlsx_path, overwrite = TRUE)
  cat("Workbook saved:", xlsx_path, "\n\n")

} else {
  cat("{openxlsx} not installed — CSV exports only.\n")
  cat("Install with: install.packages('openxlsx')\n\n")
}

# I3. Manifest ────────────────────────────────────────────────────────────────

cat("══ Exported Tables ═══════════════════════════════════════════\n")
cat("Directory:", normalizePath(export_dir), "\n")
list.files(export_dir, pattern = "\\.(csv|xlsx)$") |>
  walk(~ cat("  ", .x, "\n"))
cat("\n")
