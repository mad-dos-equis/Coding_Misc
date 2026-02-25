# ══════════════════════════════════════════════════════════════════════════════
# Section 122 Customs Duties Revenue Projection
# ══════════════════════════════════════════════════════════════════════════════
# Sources: US Treasury customs duties (MTS); HTS-10 trade-weighted ETR
# Method:  Partial equilibrium projection with 2D sensitivity grid
#          over import demand elasticity (ε) and pass-through (φ)
# Baseline: July 2025 (single month)
#
# Contents:
#    0. Parameters
#    1. Load and filter HTS-10 data
#    2. Aggregate baseline inputs
#    3. Baseline summary table
#    4. Projection function
#    5. Headline projection: 2D sensitivity grid (ε × φ)
#    6. HTS-10 line-level projection (primary ε and φ)
#   6b. Aggregate projected ETR (primary ε and φ)
#   6c. Projected ETR sensitivity over ε × φ
#    7. Harberger decomposition: rate effect vs. volume effect
#    8. Bucket decomposition
#    9. Concentration metrics
#   10. Chapter-level breakdown
#   11. HTS-4 breakdown within top chapters
#   12. Extensive-margin scenario for zero-rated lines
#   13. Foregone revenue from Section 122 exemptions
#   14. Bucket decomposition sensitivity over ε × φ
#   15. Annualized and 10-year projections (full ε × φ × horizon)
#   16. Illustrative long-run revenue scenarios (primary ε and φ)
#   17. Visualizations
#   18. Export: CSV collection + optional multi-sheet xlsx workbook
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
#   27 = Mineral fuels and oils (energy)
#   30 = Pharmaceutical products
EXEMPT_CHAPTERS <- c("27", "30")

# Extensive-margin multiplier for zero-rated lines
# The behavioral response to 0% → 15% is plausibly larger than 5% → 15%.
# Applied as a scenario alongside the standard (uniform ε) projection.
ZERO_RATE_ELAST_MULTIPLIER <- 1.5

# Export directory
EXPORT_DIR <- "output_tables"

# Formatting helpers
fmt_dollar <- function(x) dollar(x, accuracy = 1)
fmt_pct    <- function(x) percent(x, accuracy = 0.01)


# ── 1. Load and filter HTS-10 data ───────────────────────────────────────────
# July 2025 selected as baseline month. Rationale:
#   - Post-Liberation Day: most trading partners at 10% IEEPA baseline
#   - Post-Geneva (May 14): China at ~30%, avoiding the within-month
#     145%/30% discontinuity present in May 2025
#   - 232 structure (50% steel/aluminum since June 3; autos at 25%;
#     copper and lumber in place) closely matches the regime Section 122
#     layers on top of today
#   - No within-month structural breaks

hts10_raw <- read_csv("your_hts10_data.csv")   # ← replace with your path

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

cat("══ Baseline Summary (July 2025) ══════════════════════════════\n")
cat("Total customs duties:        ", fmt_dollar(R_baseline),          "\n")
cat("Total dutiable import value: ", fmt_dollar(M_baseline),          "\n")
cat("Implied trade-weighted ETR:  ", fmt_pct(tau_baseline),           "\n\n")


# ── 3. Baseline summary table ────────────────────────────────────────────────
# One-row context table anchoring every downstream number.

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

cat("Total HTS-10 lines:       ", comma(nrow(hts10_base)), "\n")
cat("  Exempt:                 ", comma(n_exempt_lines), "\n")
cat("  Zero-rated:             ", comma(n_zero_lines), "\n")
cat("  Below 15%:              ", comma(n_below_lines), "\n")
cat("  At/above 15%:           ", comma(n_above_lines), "\n")
cat("Simple-average ETR:       ", fmt_pct(mean(hts10_base$tau_eff)), "\n")
cat("Median ETR:               ", fmt_pct(median(hts10_base$tau_eff)), "\n\n")


# ── 4. Projection function ───────────────────────────────────────────────────

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


# ── 5. Headline projection: 2D sensitivity grid (ε × φ) ──────────────────────

grid <- cross_join(elasticity_scenarios, phi_scenarios) |>
  mutate(project_revenue(epsilon, phi, tau_baseline, TAU_NEW, R_baseline))

cat("══ 2D Sensitivity Grid: Projected Monthly Revenue ════════════\n")
print(
  grid |>
    select(epsilon_label, phi_label, R_projected, delta_R_abs,
           delta_R_pct, revenue_multiple) |>
    mutate(
      across(c(R_projected, delta_R_abs), fmt_dollar),
      delta_R_pct      = round(delta_R_pct, 1),
      revenue_multiple = round(revenue_multiple, 3)
    )
)
cat("\n")

# Wide revenue matrix: elasticity (rows) × pass-through (cols)
revenue_matrix <- grid |>
  select(epsilon_label, phi_label, R_projected) |>
  mutate(R_projected = fmt_dollar(R_projected)) |>
  pivot_wider(names_from = phi_label, values_from = R_projected)

cat("══ Revenue Matrix ════════════════════════════════════════════\n")
print(revenue_matrix)
cat("\n")


# ── 6. HTS-10 line-level projection (primary ε and φ) ────────────────────────

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


# ── 6b. Aggregate projected ETR (primary ε and φ) ────────────────────────────
#
# WHY THIS MATTERS
# ────────────────
# The Section 122 statutory rate is 15%, but the actual effective tariff rate
# (ETR) the economy faces will differ — and potentially differ substantially —
# from 15%. Three forces drive the wedge:
#
#   1. Exempt chapters (energy, pharma) stay at their current rates, pulling
#      the aggregate ETR below the statutory floor.
#   2. Lines already at or above 15% are unaffected by Section 122, so their
#      existing (higher) rates push the aggregate ETR above 15%.
#   3. Behavioral response: importers reduce purchases of goods that become
#      more expensive. Lines hit hardest by the rate increase see the largest
#      import contractions, which shrinks their weight in the aggregate and
#      pulls the realized ETR below what a naive calculation would suggest.
#
# To capture this, we compute two versions of the projected ETR:
#
#   PAASCHE ETR (projected import weights)
#   ──────────────────────────────────────
#   For each HTS-10 line, we:
#     (a) compute the rate shock: δτ = max(0.15 − current ETR, 0),
#         zeroed out for exempt lines;
#     (b) translate that into a price shock, scaled by the pass-through
#         parameter φ;
#     (c) adjust import value downward using the demand elasticity ε
#         (lines with bigger rate shocks contract more); and
#     (d) apply the new effective rate to get projected duties.
#   The Paasche ETR is then: sum(projected duties) / sum(projected imports).
#   This is the rate the economy actually faces after behavioral adjustment.
#   It is the internally consistent ETR for revenue projections.
#
#   LASPEYRES ETR (baseline import weights)
#   ───────────────────────────────────────
#   Same new effective rates, but weighted by original (pre-shock) import
#   values — i.e., what the ETR would be if importers didn't change their
#   purchasing behavior at all. This is the mechanical/"sticker-price" ETR
#   and serves as an upper bound on the effective rate.
#
# The gap between Laspeyres and Paasche directly measures how much the
# behavioral demand response erodes the effective rate. This maps to the
# Harberger volume effect computed in Section 7.
# ──────────────────────────────────────────────────────────────────────────────

M_proj_total       <- sum(hts10_projected$import_value_proj)
R_proj_total       <- sum(hts10_projected$duties_proj)
tau_proj_paasche   <- R_proj_total / M_proj_total

# Laspeyres: new rates, baseline import weights
R_proj_laspeyres   <- sum(hts10_projected$tau_eff_proj * hts10_projected$import_value)
tau_proj_laspeyres <- R_proj_laspeyres / M_baseline

# By-bucket projected ETRs — shows where the aggregate ETR is coming from
etr_by_bucket <- hts10_projected |>
  group_by(bucket) |>
  summarise(
    import_value_base     = sum(import_value),
    import_value_proj     = sum(import_value_proj),
    duties_base           = sum(duties),
    duties_proj           = sum(duties_proj),
    tw_etr_base           = sum(duties) / sum(import_value),
    tw_etr_proj_paasche   = sum(duties_proj) / sum(import_value_proj),
    tw_etr_proj_laspeyres = sum(tau_eff_proj * import_value) / sum(import_value),
    .groups = "drop"
  )

# Summary table
etr_summary <- tibble(
  metric = c(
    "Baseline trade-weighted ETR",
    "Projected trade-weighted ETR (Paasche \u2014 projected weights)",
    "Projected trade-weighted ETR (Laspeyres \u2014 baseline weights)",
    "Statutory Section 122 rate",
    "ETR increase (Paasche)",
    "ETR increase (Laspeyres)",
    "Behavioral erosion of ETR (Laspeyres \u2212 Paasche)"
  ),
  value = c(
    tau_baseline,
    tau_proj_paasche,
    tau_proj_laspeyres,
    TAU_NEW,
    tau_proj_paasche - tau_baseline,
    tau_proj_laspeyres - tau_baseline,
    tau_proj_laspeyres - tau_proj_paasche
  ),
  formatted = c(
    fmt_pct(tau_baseline),
    fmt_pct(tau_proj_paasche),
    fmt_pct(tau_proj_laspeyres),
    fmt_pct(TAU_NEW),
    fmt_pct(tau_proj_paasche - tau_baseline),
    fmt_pct(tau_proj_laspeyres - tau_baseline),
    fmt_pct(tau_proj_laspeyres - tau_proj_paasche)
  )
)

cat("══ Aggregate Effective Tariff Rate Comparison ════════════════\n")
cat("\u03b5 =", EPSILON_PRIMARY, ", \u03c6 =", PHI_PRIMARY, "\n\n")
cat("The statutory Section 122 rate is 15%, but the actual effective\n")
cat("tariff rate (ETR) the economy faces will differ. Exempt chapters,\n")
cat("lines already above 15%, and behavioral import contraction all\n")
cat("drive a wedge between the statutory rate and the realized ETR.\n\n")
cat("Baseline trade-weighted ETR:        ", fmt_pct(tau_baseline), "\n")
cat("Projected ETR (Paasche weights):    ", fmt_pct(tau_proj_paasche), "\n")
cat("  \u2192 Uses projected (post-shock) import values as weights.\n")
cat("    This is the rate the economy actually faces.\n")
cat("Projected ETR (Laspeyres weights):  ", fmt_pct(tau_proj_laspeyres), "\n")
cat("  \u2192 Uses baseline import values as weights (no behavioral adjustment).\n")
cat("    This is the mechanical upper bound on the effective rate.\n")
cat("Statutory Section 122 rate:         ", fmt_pct(TAU_NEW), "\n")
cat("ETR increase (Paasche):             ", fmt_pct(tau_proj_paasche - tau_baseline), "\n")
cat("ETR increase (Laspeyres):           ", fmt_pct(tau_proj_laspeyres - tau_baseline), "\n")
cat("Behavioral erosion (Lasp. \u2212 Paa.): ", fmt_pct(tau_proj_laspeyres - tau_proj_paasche), "\n")
cat("  \u2192 The demand response erodes the effective rate by this amount.\n")
cat("    This corresponds to the Harberger volume effect in Section 7.\n\n")
cat("By bucket:\n")
print(etr_by_bucket)
cat("\n")


# ── 6c. Projected ETR sensitivity over ε × φ ────────────────────────────────
#
# The aggregate ETR varies with assumptions about how much of the tariff
# increase is passed through to domestic prices (φ) and how strongly
# importers cut back purchases in response (ε). This section computes
# both Paasche and Laspeyres projected ETRs across the full grid.
#
# Key intuition:
#   - Higher |ε| (more elastic demand) → more import contraction on lines
#     with big rate shocks → lower Paasche ETR relative to Laspeyres.
#   - Higher φ (more pass-through) → larger price shocks → amplifies the
#     demand response → wider Paasche-Laspeyres gap.
#   - At ε = 0 (naive/no behavioral response), Paasche = Laspeyres.
#
# The Laspeyres ETR is invariant to ε (no behavioral adjustment by
# definition) but does vary with φ through the rate shock computation
# when τ_eff already reflects partial pass-through. In this script,
# however, δτ = max(0.15 − τ_eff, 0) is mechanical and independent
# of φ, so Laspeyres is constant across all scenarios. It appears in
# the grid for completeness and to make the Paasche gap easy to read.
# ──────────────────────────────────────────────────────────────────────────────

etr_sensitivity <- cross_join(elasticity_scenarios, phi_scenarios) |>
  mutate(
    etr_result = map2(epsilon, phi, function(eps, ph) {
      proj <- hts10_base |>
        mutate(
          delta_tau         = if_else(exempt, 0, pmax(TAU_NEW - tau_eff, 0)),
          price_shock       = ph * (delta_tau / (1 + tau_eff)),
          import_value_proj = import_value * (1 + eps * price_shock),
          tau_eff_proj      = tau_eff + delta_tau,
          duties_proj       = import_value_proj * tau_eff_proj
        )
      tibble(
        tw_etr_proj_paasche   = sum(proj$duties_proj) / sum(proj$import_value_proj),
        tw_etr_proj_laspeyres = sum(proj$tau_eff_proj * proj$import_value) / sum(proj$import_value),
        R_projected           = sum(proj$duties_proj),
        M_projected           = sum(proj$import_value_proj)
      )
    })
  ) |>
  unnest(etr_result)

# Wide matrices for easy reading
etr_matrix_paasche <- etr_sensitivity |>
  select(epsilon_label, phi_label, tw_etr_proj_paasche) |>
  pivot_wider(names_from = phi_label, values_from = tw_etr_proj_paasche)

etr_matrix_laspeyres <- etr_sensitivity |>
  select(epsilon_label, phi_label, tw_etr_proj_laspeyres) |>
  pivot_wider(names_from = phi_label, values_from = tw_etr_proj_laspeyres)

cat("══ Projected Trade-Weighted ETR Sensitivity ══════════════════\n")
cat("Baseline ETR: ", fmt_pct(tau_baseline), "\n\n")
cat("Paasche (projected import weights):\n")
cat("  Each cell shows the ETR after behavioral adjustment.\n")
cat("  Higher |\u03b5| and higher \u03c6 push this further below the Laspeyres.\n")
print(etr_matrix_paasche |> mutate(across(-epsilon_label, fmt_pct)))
cat("\nLaspeyres (baseline import weights):\n")
cat("  Mechanical ETR assuming no change in import quantities.\n")
cat("  Constant across \u03b5 and \u03c6 because \u03b4\u03c4 is independent of both.\n")
print(etr_matrix_laspeyres |> mutate(across(-epsilon_label, fmt_pct)))
cat("\n")


# ── 7. Harberger decomposition: rate effect vs. volume effect ─────────────────
# Total ΔR = rate_effect + volume_effect
#   rate_effect   = (τ_new − τ_old) × M_old       [mechanical gain from higher rate]
#   volume_effect = τ_new × (M_new − M_old)        [revenue loss from import contraction]

hts10_harberger <- hts10_projected |>
  mutate(
    rate_effect   = delta_tau * import_value,
    volume_effect = tau_eff_proj * (import_value_proj - import_value),
    check         = abs((rate_effect + volume_effect) - delta_duties)
  )

max_decomp_error <- max(hts10_harberger$check)
if (max_decomp_error > 1) {
  warning("Harberger decomposition error exceeds $1: ", fmt_dollar(max_decomp_error))
}

harberger_total <- hts10_harberger |>
  summarise(
    rate_effect      = sum(rate_effect),
    volume_effect    = sum(volume_effect),
    net_delta_duties = sum(delta_duties)
  )

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

cat("══ Harberger Decomposition: Rate vs. Volume Effect ═══════════\n")
cat("ε =", EPSILON_PRIMARY, ", φ =", PHI_PRIMARY, "\n\n")
cat("Aggregate:\n")
cat("  Gross rate effect:  ", fmt_dollar(harberger_total$rate_effect),
    " (", round(harberger_total$rate_effect / harberger_total$net_delta_duties * 100, 1), "%)\n", sep = "")
cat("  Volume offset:      ", fmt_dollar(harberger_total$volume_effect),
    " (", round(harberger_total$volume_effect / harberger_total$net_delta_duties * 100, 1), "%)\n", sep = "")
cat("  Net revenue gain:   ", fmt_dollar(harberger_total$net_delta_duties), "\n\n")
cat("By bucket:\n")
print(harberger_bucket)
cat("\nBy chapter (below 15% bucket, top 15):\n")
print(harberger_chapter |> slice_head(n = 15))
cat("\n")


# ── 8. Bucket decomposition ──────────────────────────────────────────────────

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

cat("══ Bucket Decomposition ══════════════════════════════════════\n")
print(decomp_bucket)
cat("\n")


# ── 9. Concentration metrics ─────────────────────────────────────────────────

# (Computed after chapter decomposition in Section 10, but logically grouped
#  here; the decomp_chapter object is created next.)


# ── 10. Chapter-level breakdown ──────────────────────────────────────────────

decomp_chapter <- hts10_projected |>
  filter(bucket == "Below 15% \u2014 Section 122 adds margin") |>
  mutate(import_value_proj = pmax(import_value_proj, 0)) |>
  group_by(chapter) |>
  summarise(
    n_lines             = n(),
    import_value_base   = sum(import_value),
    duties_baseline     = sum(duties),
    duties_projected    = sum(duties_proj),
    delta_duties        = sum(delta_duties),
    tw_avg_etr_pre      = weighted.mean(tau_eff, import_value),
    tw_avg_etr_post     = weighted.mean(tau_eff_proj, import_value),
    simple_avg_etr_pre  = mean(tau_eff),
    simple_avg_etr_post = mean(tau_eff_proj),
    tw_avg_delta_tau    = weighted.mean(delta_tau, import_value),
    simple_avg_delta_tau = mean(delta_tau),
    .groups = "drop"
  ) |>
  mutate(share_of_gain = delta_duties / sum(delta_duties)) |>
  arrange(desc(delta_duties))

cat("══ Top 20 HTS Chapters Driving Revenue Gain ═════════════════\n")
print(decomp_chapter |> slice_head(n = 20))
cat("\n")

# ── 9 (cont'd). Concentration metrics ───────────────────────────────────────

total_gain <- sum(decomp_chapter$delta_duties)

concentration <- decomp_chapter |>
  mutate(
    cumulative_gain  = cumsum(delta_duties),
    cumulative_share = cumulative_gain / total_gain
  )

top5_share  <- concentration |> slice_head(n = 5)  |> pull(cumulative_share) |> last()
top10_share <- concentration |> slice_head(n = 10) |> pull(cumulative_share) |> last()

hhi_chapter <- decomp_chapter |>
  mutate(share = delta_duties / total_gain) |>
  summarise(hhi = sum(share^2)) |>
  pull(hhi)

concentration_summary <- tibble(
  metric = c("Top 5 chapters share", "Top 10 chapters share",
             "Chapter-level HHI", "Number of chapters with gain"),
  value  = c(top5_share, top10_share, hhi_chapter,
             sum(decomp_chapter$delta_duties > 0)),
  formatted = c(fmt_pct(top5_share), fmt_pct(top10_share),
                round(hhi_chapter, 4),
                sum(decomp_chapter$delta_duties > 0))
)

cat("══ Revenue Gain Concentration ════════════════════════════════\n")
cat("Top 5 chapters:  ", fmt_pct(top5_share), "of projected gain\n")
cat("Top 10 chapters: ", fmt_pct(top10_share), "of projected gain\n")
cat("Chapter-level HHI:", round(hhi_chapter, 4), "\n")
cat("Chapters with positive gain:", sum(decomp_chapter$delta_duties > 0), "\n\n")


# ── 11. HTS-4 breakdown within top chapters ──────────────────────────────────
# Includes: trade-weighted and simple-average ETR (pre/post), marginal rate
# increase (delta_tau), and Harberger rate/volume decomposition at HTS-4.

TOP_N_CHAPTERS <- 10

top_chapters <- decomp_chapter |>
  slice_head(n = TOP_N_CHAPTERS) |>
  pull(chapter)

hts4_decomp <- hts10_harberger |>
  filter(
    bucket == "Below 15% \u2014 Section 122 adds margin",
    chapter %in% top_chapters
  ) |>
  mutate(
    hts4              = str_sub(hts, 1, 4),
    import_value_proj = pmax(import_value_proj, 0)   # clamp to prevent negatives
  ) |>
  group_by(chapter, hts4) |>
  summarise(
    n_lines              = n(),
    import_value_base    = sum(import_value),
    import_value_proj    = sum(import_value_proj),
    duties_baseline      = sum(duties),
    duties_projected     = sum(duties_proj),
    delta_duties         = sum(delta_duties),
    rate_effect          = sum(rate_effect),
    volume_effect        = sum(volume_effect),
    tw_avg_etr_pre       = weighted.mean(tau_eff, import_value),
    tw_avg_etr_post      = weighted.mean(tau_eff_proj, import_value),
    simple_avg_etr_pre   = mean(tau_eff),
    simple_avg_etr_post  = mean(tau_eff_proj),
    tw_avg_delta_tau     = weighted.mean(delta_tau, import_value),
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

cat("══ HTS-4 Breakdown: Top", TOP_N_CHAPTERS, "Chapters ════════════════════\n")
cat("Lines where July 2025 ETR < 15% (Section 122 adds margin)\n\n")

for (ch in top_chapters) {
  ch_data <- hts4_decomp |> filter(chapter == ch) |> slice_head(n = 10)
  ch_gain <- sum(ch_data$delta_duties)
  cat("Chapter ", ch, " \u2014 Top HTS-4 codes (chapter gain: ",
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

cat("══ Top 30 HTS-4 Codes Overall by Revenue Gain ═══════════════\n")
print(hts4_top30 |>
        select(chapter, hts4, n_lines, import_value_base, delta_duties,
               rate_effect, volume_effect,
               tw_avg_etr_pre, tw_avg_etr_post, cumulative_share))
cat("\n")


# ── 12. Extensive-margin scenario for zero-rated lines ────────────────────────

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
  scenario           = c("Standard (uniform \u03b5)",
                          "Extensive-margin (1.5\u00d7 \u03b5 for zero-rated)"),
  epsilon_zero_rated = c(EPSILON_PRIMARY,
                          EPSILON_PRIMARY * ZERO_RATE_ELAST_MULTIPLIER),
  epsilon_other      = c(EPSILON_PRIMARY, EPSILON_PRIMARY),
  monthly_projected  = c(standard_total, extensive_total),
  monthly_delta      = c(standard_total - R_baseline,
                          extensive_total - R_baseline),
  pct_change_vs_base = c((standard_total / R_baseline - 1) * 100,
                          (extensive_total / R_baseline - 1) * 100)
)

cat("══ Extensive-Margin Scenario for Zero-Rated Lines ════════════\n")
cat("Multiplier:", ZERO_RATE_ELAST_MULTIPLIER, "\u00d7\n")
cat("\u03b5 (zero-rated):", EPSILON_PRIMARY * ZERO_RATE_ELAST_MULTIPLIER,
    "  \u03b5 (other):", EPSILON_PRIMARY, "\n\n")
print(extensive_comparison |>
        mutate(across(c(monthly_projected, monthly_delta), fmt_dollar),
               pct_change_vs_base = round(pct_change_vs_base, 1)))
cat("\nBucket detail (extensive-margin scenario):\n")
print(extensive_bucket)
cat("\n")


# ── 13. Foregone revenue from Section 122 exemptions ─────────────────────────

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

cat("══ Foregone Revenue from Section 122 Exemptions ══════════════\n")
cat("Exempt chapters:", paste(EXEMPT_CHAPTERS, collapse = ", "), "\n")
cat("Monthly foregone revenue:    ", fmt_dollar(foregone_total), "\n")
cat("Annualized foregone:         ", fmt_dollar(foregone_total * 12), "\n")
cat("150-day foregone:            ", fmt_dollar(foregone_total * 5), "\n\n")
print(foregone_by_chapter)
cat("\n")

# Foregone revenue sensitivity across ε × φ
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

cat("Foregone revenue sensitivity (\u03b5 \u00d7 \u03c6), monthly:\n")
print(foregone_matrix |> mutate(across(-epsilon_label, fmt_dollar)))
cat("\n")


# ── 14. Bucket decomposition sensitivity over ε × φ ──────────────────────────

bucket_sensitivity <- cross_join(elasticity_scenarios, phi_scenarios) |>
  mutate(
    results = map2(epsilon, phi, function(eps, ph) {
      hts10_base |>
        mutate(
          bucket = case_when(
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
          exempt_delta       = sum(delta_duties[bucket == "exempt"]),
          zero_delta         = sum(delta_duties[bucket == "zero"])
        )
    })
  ) |>
  unnest(results)

cat("══ Bucket Gain Sensitivity (\u03b5 \u00d7 \u03c6) ══════════════════════════\n")
print(bucket_sensitivity)
cat("\n")


# ── 15. Annualized and 10-year projections (full ε × φ × horizon) ────────────

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

cat("══ Long-Run Projections (Full \u03b5 \u00d7 \u03c6 \u00d7 Horizon) ════════════\n")
cat("NOTE: Horizons beyond 150 days are purely illustrative.\n")
cat("Section 122 expires after 150 days absent congressional action.\n\n")
cat("Cumulative revenue ranges across all \u03b5 \u00d7 \u03c6 combinations:\n")
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


# ── 16. Illustrative long-run revenue scenarios (primary ε and φ) ─────────────
# Section 122 is capped at 150 days. The table below is explicitly illustrative.

R_primary       <- project_revenue(EPSILON_PRIMARY, PHI_PRIMARY,
                                   tau_baseline, TAU_NEW, R_baseline)$R_projected
delta_R_monthly <- R_primary - R_baseline

scenario_table <- tibble(
  scenario = c(
    "A. Section 122 lapses (150 days only)",
    "B. Full replacement \u2014 10 years at equivalent rate",
    "C. Partial replacement \u2014 50% of decade"
  ),
  months_active = c(5, 120, 60),
  assumption    = c(
    "No successor authority; tariff reverts to 232-only baseline after day 150",
    "Illustrative upper bound: Section 122 renewed or replaced by equivalent authority continuously",
    "Illustrative midpoint: successor authority faces legal/political attrition, in effect ~half the decade"
  )
) |>
  mutate(
    cumulative_delta_revenue = delta_R_monthly * months_active,
    cumulative_total_revenue = R_primary * months_active
  )

cat("══ Illustrative Long-Run Revenue Scenarios (PLANNING ONLY) ═══\n")
cat("\u03b5 =", EPSILON_PRIMARY, "(Boehm et al. 2023),",
    "\u03c6 =", PHI_PRIMARY, "(Amiti et al. 2019)\n")
cat("Monthly incremental gain over baseline: ", fmt_dollar(delta_R_monthly), "\n\n")
print(
  scenario_table |>
    select(scenario, months_active, cumulative_delta_revenue,
           cumulative_total_revenue, assumption) |>
    mutate(across(c(cumulative_delta_revenue, cumulative_total_revenue), fmt_dollar))
)
cat("\nNOTE: Scenarios B and C are purely illustrative. Section 122 expires\n")
cat("after 150 days absent congressional extension.\n\n")


# ══════════════════════════════════════════════════════════════════════════════
# 17. Visualizations
# ══════════════════════════════════════════════════════════════════════════════

# 17a. Heatmap: revenue multiple across ε × φ
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
    axis.text.x  = element_text(angle = 15, hjust = 1),
    plot.caption = element_text(size = 8, color = "gray40"),
    legend.position = "right"
  )

# 17b. Bucket decomposition bar chart (primary ε and φ)
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

# 17c. Top chapters driving the gain
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

# 17d. Faceted bar: bucket gain sensitivity across φ scenarios
p_bucket_phi <- phi_scenarios |>
  mutate(
    bucket_data = map(phi, function(ph) {
      hts10_base |>
        mutate(
          bucket = case_when(
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

# 17e. Foregone revenue by exempt chapter
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

# 17f. Harberger decomposition — stacked bar by chapter
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

# 17g. Extensive margin comparison — paired bar
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

# 17h. ETR heatmap: Paasche projected ETR across ε × φ
p_etr_heatmap <- etr_sensitivity |>
  mutate(
    epsilon_label = fct_reorder(epsilon_label, epsilon),
    phi_label     = fct_reorder(phi_label, phi)
  ) |>
  ggplot(aes(x = phi_label, y = epsilon_label, fill = tw_etr_proj_paasche)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = percent(tw_etr_proj_paasche, accuracy = 0.01)),
            size = 3.5, color = "white", fontface = "bold") +
  scale_fill_gradient(
    low    = "#5B9BD5",
    high   = "#1F4E79",
    name   = "Projected\nETR",
    labels = percent
  ) +
  geom_hline(yintercept = 0, color = "transparent") +
  labs(
    title    = "Projected Trade-Weighted ETR Under Section 122",
    subtitle = paste0(
      "Paasche (post-shock weights). Baseline ETR: ",
      fmt_pct(tau_baseline), ". Statutory rate: ", fmt_pct(TAU_NEW), "."
    ),
    x        = "Pass-Through (\u03c6)",
    y        = "Import Demand Elasticity (\u03b5)",
    caption  = paste0(
      "The Paasche ETR reflects behavioral adjustment: lines with larger\n",
      "rate shocks see bigger import contractions, reducing their weight\n",
      "in the aggregate. Higher |\u03b5| and \u03c6 widen the gap vs. the Laspeyres\n",
      "(no-adjustment) ETR of ", fmt_pct(tau_proj_laspeyres), "."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x  = element_text(angle = 15, hjust = 1),
    plot.caption  = element_text(size = 8, color = "gray40"),
    legend.position = "right"
  )

# ── Print all plots ──────────────────────────────────────────────────────────

print(p_heatmap)
print(p_bucket)
print(p_chapter)
print(p_bucket_phi)
print(p_foregone)
print(p_harberger)
print(p_extensive)
print(p_etr_heatmap)


# ══════════════════════════════════════════════════════════════════════════════
# 18. Export: CSV + Optional Multi-Sheet xlsx
# ══════════════════════════════════════════════════════════════════════════════

dir.create(EXPORT_DIR, showWarnings = FALSE, recursive = TRUE)

# ── 18a. CSV exports ─────────────────────────────────────────────────────────

exports <- list(
  "01_baseline_summary"            = baseline_summary,
  "02_sensitivity_grid"            = grid |>
    mutate(R_projected_annual = R_projected * 12,
           delta_R_abs_annual = delta_R_abs * 12,
           revenue_10yr       = R_projected * 120),
  "03_revenue_matrix_wide"         = grid |>
    select(epsilon_label, epsilon, phi_label, R_projected) |>
    pivot_wider(names_from = phi_label, values_from = R_projected),
  "04_bucket_decomposition"        = decomp_bucket,
  "05_chapter_decomposition"       = decomp_chapter,
  "06_harberger_by_bucket"         = harberger_bucket,
  "07_harberger_by_chapter"        = harberger_chapter,
  "08_concentration_summary"       = concentration_summary,
  "09_concentration_cumulative"    = concentration,
  "10_extensive_margin_comparison" = extensive_comparison,
  "11_extensive_margin_buckets"    = extensive_bucket,
  "12_foregone_by_chapter"         = foregone_by_chapter,
  "13_foregone_sensitivity"        = foregone_sensitivity,
  "14_foregone_matrix_wide"        = foregone_matrix,
  "15_bucket_sensitivity"          = bucket_sensitivity,
  "16_scenario_table"              = scenario_table,
  "17_hts4_top_chapters"           = hts4_decomp,
  "18_hts4_top30_overall"          = hts4_top30,
  "19_longrun_full_grid"           = longrun_grid,
  "20_longrun_summary"             = longrun_summary
)

# Horizon matrices (numbered 21_matrix_*)
for (h in names(horizon_matrices)) {
  safe_name <- str_replace_all(tolower(h), "[^a-z0-9]+", "_") |>
    str_remove("_$")
  exports[[paste0("21_matrix_", safe_name)]] <- horizon_matrices[[h]]
}

# ETR tables (numbered 22–26)
exports[["22_etr_summary"]]           <- etr_summary
exports[["23_etr_by_bucket"]]         <- etr_by_bucket
exports[["24_etr_sensitivity"]]       <- etr_sensitivity
exports[["25_etr_matrix_paasche"]]    <- etr_matrix_paasche
exports[["26_etr_matrix_laspeyres"]]  <- etr_matrix_laspeyres

walk2(names(exports), exports, function(nm, df) {
  write_csv(df, file.path(EXPORT_DIR, paste0(nm, ".csv")))
})

# ── 18b. Multi-sheet xlsx (optional) ─────────────────────────────────────────

if (requireNamespace("openxlsx", quietly = TRUE)) {
  library(openxlsx)

  wb <- createWorkbook()

  sheet_map <- list(
    "Baseline Summary"      = baseline_summary,
    "Sensitivity Grid"      = exports[["02_sensitivity_grid"]],
    "Revenue Matrix"        = exports[["03_revenue_matrix_wide"]],
    "Bucket Decomposition"  = decomp_bucket,
    "Chapter Decomposition" = decomp_chapter,
    "Harberger by Bucket"   = harberger_bucket,
    "Harberger by Chapter"  = harberger_chapter,
    "Concentration"         = concentration_summary,
    "ETR Summary"           = etr_summary,
    "ETR by Bucket"         = etr_by_bucket,
    "ETR Sensitivity"       = etr_sensitivity,
    "Extensive Margin"      = extensive_comparison,
    "Foregone by Chapter"   = foregone_by_chapter,
    "Foregone Sensitivity"  = foregone_sensitivity,
    "Bucket Sensitivity"    = bucket_sensitivity,
    "Scenario Table"        = scenario_table,
    "HTS-4 Top Chapters"   = hts4_decomp,
    "HTS-4 Top 30"          = hts4_top30,
    "Long-Run Grid"         = longrun_grid,
    "Long-Run Summary"      = longrun_summary
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

  xlsx_path <- file.path(EXPORT_DIR, "section122_projection_tables.xlsx")
  saveWorkbook(wb, xlsx_path, overwrite = TRUE)
  cat("Workbook saved:", xlsx_path, "\n\n")

} else {
  cat("{openxlsx} not installed \u2014 CSV exports only.\n")
  cat("Install with: install.packages('openxlsx')\n\n")
}

# ── 18c. Manifest ────────────────────────────────────────────────────────────

cat("══ Exported Tables ═══════════════════════════════════════════\n")
cat("Directory:", normalizePath(EXPORT_DIR), "\n")
list.files(EXPORT_DIR, pattern = "\\.(csv|xlsx)$") |>
  walk(~ cat("  ", .x, "\n"))
cat("\n")
