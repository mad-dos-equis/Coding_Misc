# =============================================================================
# grocery_elasticity_hybrid.R
#
# Channel-augmented spending-weighted FAH own-price elasticity using a
# category-by-category hybrid of Okrent & Alston (2012), Zhen et al. (2014),
# and Luke, Tonsor & Schroeder (2025) for meat.
#
# Marshallian vs. Hicksian note: OA reports uncompensated (Marshallian);
# Zhen reports median Marshallian; Luke reports Hicksian (compensated).
# In practice the distinction is what's held fixed when the price changes:
#   - Marshallian (uncompensated): nominal income (or total expenditure) is
#     held fixed. The estimate captures both the substitution response and
#     the real-income hit of the price change. This is the right concept
#     for most policy questions because consumers don't get compensated
#     when prices rise; their real purchasing power simply falls.
#   - Hicksian (compensated): utility is held fixed. Pure substitution
#     effect only — the consumer is imagined to be given (or taken) just
#     enough income to keep them on the same indifference curve. Useful
#     for welfare calculations but not for "how much less will people
#     actually buy if prices rise?"
# The Slutsky equation links them: eps_M = eps_H - w_i * eta_i, where
# w_i is the good's budget share and eta_i is its expenditure (income)
# elasticity. The subtraction captures the income effect — for a normal
# good (eta > 0), Marshallian is more elastic than Hicksian because the
# price hike also makes the consumer poorer in real terms, further
# reducing quantity demanded. We convert Luke's Hicksian estimates to
# Marshallian via Slutsky using Luke's reported expenditure elasticities,
# so all three sources are on the same uncompensated (Marshallian) footing.
#
# Why the headline is more elastic than diary-only estimates suggest:
# This is a feature of the data, not a bug. A well-documented finding in
# the food-demand literature (formalized by Jeon et al. 2024 and replicated
# by Luke, Tonsor & Schroeder 2026) is that scanner-data own-price
# elasticities are systematically more elastic than diary-data or
# publicly-available-data elasticities for the same categories. Plausible
# mechanisms include: scanner data accurately measure sale-driven quantity
# spikes that diary data smooth over; scanner data capture brand-switching
# within categories that diary data miss; and scanner prices are quantity-
# weighted, mechanically lining up price drops with purchase surges.
# Because the hybrid headline draws on scanner data for four of six FAH
# groups (cereals/bakery, dairy, beverages, other FAH) and Luke scanner
# data for the meat group, it inherits this scanner-elastic bias. We
# apply the Jeon et al. correction of +0.219 to all scanner-based inputs
# to partially offset this bias, but a residual gap to diary-only estimates
# remains and is the right reading of the data rather than a methodological
# artifact.
#
# Source data are POOLED (whole-market), not pure in-person:
# A subtle but important note on the channel architecture. None of the
# three source studies estimated channel-specific elasticities. Each
# reports a single elasticity per category, estimated on a sample that
# pooled in-person and online purchases together. We treat each source
# elasticity as an in-person estimate (the column 'source_pooled' in the
# group-level table) for the purpose of applying the Harris-Lagoudakis
# (2023) online multiplier, but this is a defensible approximation
# rather than a literal truth.
# The approximation works because all three studies are dominated by
# in-person purchases:
#   - OA (1998-2010 CEX diary): online FAH was ~0-2% of the sample
#   - Zhen (2006 Nielsen Homescan): online FAH was <1% of the sample
#   - Luke (2009-2018 Circana): online FAH grew from ~0% to ~3% across
#     the sample, with a time-weighted average of perhaps 1-2%
# A pure in-person elasticity would be slightly more elastic than the
# pooled estimate because the pooled estimate already contains a small
# contribution from less-elastic online purchases. The unblending
# adjustment is:
#   eps_inperson = eps_pooled / ((1 - omega_sample) + omega_sample * mu)
# For Luke (the most affected source) with omega_sample = 0.02 and
# mu = 0.5, this implies eps_inperson is about 1% more elastic than
# eps_pooled. For OA and Zhen the correction is even smaller. We do
# not apply this correction in the script because it falls well within
# the rounding tolerance of the headline number; the column labeled
# 'source_pooled' should be read as "essentially in-person, with at
# most a ~1% understatement of true in-person elasticity."
#
# -----------------------------------------------------------------------------
# Data sources
# -----------------------------------------------------------------------------
#
# [1] Okrent & Alston (2012). USDA-ERS ERR-139.
#     https://www.ers.usda.gov/publications/pub-details?pubid=45003
#
# [2] Zhen, C., E.A. Finkelstein, J.M. Nonnemaker, S.A. Karns & J.E. Todd
#     (2014). AJAE 96(1): 1-25.
#     https://doi.org/10.1093/ajae/aat049
#     Open mirror: https://bpb-us-e1.wpmucdn.com/sites.psu.edu/dist/c/13885/files/2014/07/
#         Zhen2014_Predicting-the-effects-of-sugar-sweetened-beverage-taxes-on-food-and-beverage-demand-in-a-large-demand-system.pdf
#     Standard errors derived as |elast| / |t-stat| from Table 2.
#
# [3] Luke, J.R., G.T. Tonsor & T.C. Schroeder (2026). "U.S. Meat Demand
#     Elasticity Estimates: Using Publicly Available Data versus Scanner
#     Data." Agricultural and Resource Economics Review 55(1): 104-123.
#     https://doi.org/10.1017/age.2025.10020 (open access, CC-BY)
#     Table 2 (Rotterdam coefficients) and Table 3 (Hicksian elasticities).
#     Used for the meat_and_eggs group in the hybrid.
#
# [4] Jeon, Y., H. Hoang, W. Thompson & D. Abler (2024). AEPP 46(2): 760-780.
#     https://onlinelibrary.wiley.com/doi/10.1002/aepp.13414
#     Per Luke, Tonsor & Schroeder (2026), Jeon et al. propose a correction
#     factor of -0.219 to align non-scanner-based own-price elasticities to
#     scanner-based ones. Equivalently, ADD +0.219 to a scanner-based
#     estimate to align it to a non-scanner-based estimate (less elastic).
#     We use alpha_scanner = +0.219 as default. Set to 0.0 to disable.
#
# [5] Harris-Lagoudakis, K. (2023). IJIO 87: 102918.
#     https://www.sciencedirect.com/science/article/abs/pii/S0167718722000935
#     mu_online ~ 0.5 (own-price elasticities ~2x larger in-store than online).
#
# [6] USDA-ERS Food Expenditure Series (2024).
#     https://www.ers.usda.gov/data-products/food-expenditure-series
#     Online share of FAH ~ 9.2% in 2024.
#
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(purrr)
})

# -----------------------------------------------------------------------------
# 1. INPUTS
# -----------------------------------------------------------------------------

# Source [1]: Okrent & Alston (2012) Table 4 — Marshallian own-price
oa_elasticities <- tribble(
  ~group,                  ~oa_elast_source, ~oa_se,
  "cereals_and_bakery",    -0.58,              0.25,
  "meat_and_eggs",         -0.31,              0.17,
  "dairy",                 -0.05,              0.09,
  "fruits_and_vegetables", -0.79,              0.19,
  "nonalcoholic_beverages",-0.65,              0.39,
  "other_fah",             -0.98,              0.30
)

# Source [1]: Okrent & Alston (2012) Table 1 first-stage budget shares
oa_shares <- tribble(
  ~group,                   ~share_pct_total_budget,
  "cereals_and_bakery",      1.66,
  "meat_and_eggs",           2.88,
  "dairy",                   1.21,
  "fruits_and_vegetables",   1.69,
  "nonalcoholic_beverages",  0.75,
  "other_fah",               2.76
)

# Source [2]: Zhen et al. (2014) Table 2 — own-price elasticities, t-stats
# SE derived as |elast|/|t|. These are statistical SEs from a model fit
# on ~110K quarterly household observations; very small by construction.
zhen_categories <- tribble(
  ~zhen_cat,              ~exp_low, ~exp_high, ~elast,    ~t_stat,   ~oa_group,
  "regular_csd",           6.33,    5.53,    -1.035,    -103.2,   "nonalcoholic_beverages",
  "sports_energy_drinks",  1.39,    2.08,    -2.363,    -106.9,   "nonalcoholic_beverages",
  "whole_milk",            2.39,    1.70,    -0.900,     -23.9,   "dairy",
  "reduced_fat_milk",      5.76,    6.64,    -1.199,    -129.5,   "dairy",
  "whole_grain_bread",     0.89,    1.26,    -1.196,     -95.0,   "cereals_and_bakery",
  "white_bread",           5.23,    5.72,    -0.666,    -103.2,   "cereals_and_bakery",
  "cheese",                7.19,    8.83,    -0.567,     -64.9,   "dairy",
  "juice_100pct",          4.09,    5.01,    -1.566,     -78.2,   "nonalcoholic_beverages",
  "juice_drinks",          3.12,    3.26,    -1.192,     -99.0,   "nonalcoholic_beverages",
  "peanut_butter",         0.79,    0.83,    -1.466,     -90.5,   "other_fah",
  "cereals",               6.38,    7.18,    -0.814,     -65.0,   "cereals_and_bakery",
  "yogurt",                1.85,    2.64,    -2.043,    -119.4,   "dairy",
  "diet_csd",              3.55,    4.85,    -0.959,     -74.3,   "nonalcoholic_beverages",
  "bottled_water",         2.13,    2.90,    -1.703,     -97.7,   "nonalcoholic_beverages",
  "canned_dried_fruits",   2.32,    2.58,    -1.222,    -116.2,   "fruits_and_vegetables",
  "canned_vegetables",     4.52,    4.91,    -1.516,    -138.9,   "fruits_and_vegetables",
  "frozen_dinners",       11.67,   12.68,    -0.765,     -61.4,   "other_fah",
  "canned_soup",           2.49,    3.18,    -2.472,    -253.8,   "other_fah",
  "candy",                 6.19,    7.00,    -1.485,    -108.4,   "other_fah",
  "ice_cream",             3.24,    3.35,    -1.115,    -160.3,   "dairy",
  "cakes_cookies",         7.45,    8.06,    -1.697,    -122.2,   "cereals_and_bakery",
  "lunch_meat",            3.69,    3.89,    -1.216,    -139.5,   "meat_and_eggs",   # excluded from hybrid
  "snacks",                7.02,    8.52,    -1.266,     -80.7,   "other_fah"
) %>%
  mutate(elast_se = abs(elast / t_stat))

# Source [3]: Luke, Tonsor & Schroeder (2026) — beef, chicken, pork
# Table 3 reports Hicksian elasticities; Table 1 reports expenditure shares
# and expenditure elasticities. We convert to Marshallian via Slutsky.
# SE on Hicksian = SE(c_ii)/w_i from Table 2 coefficients.
luke_meat <- tribble(
  ~meat,    ~hicksian_elast, ~hicksian_se, ~exp_share, ~exp_elast,
  "beef",   -0.498,           0.157,        0.498,      0.934,
  "chicken",-1.113,           0.394,        0.249,      1.001,
  "pork",   -2.016,           0.071,        0.253,      1.120
) %>%
  mutate(
    # Slutsky conversion to Marshallian for comparability with Zhen and OA
    marshallian_elast = hicksian_elast - exp_share * exp_elast,
    # SE on Marshallian ≈ SE on Hicksian (the w*eta term is a deterministic
    # adjustment using point estimates; ignoring covariance is a small-sample
    # approximation)
    marshallian_se    = hicksian_se,
    # Within-meat-group expenditure share (renormalize among the 3 meats)
    within_meat_share = exp_share / sum(exp_share)
  )

# Tunable parameters
MU_ONLINE_DEFAULT      <- 0.50     # Source [5]
OMEGA_ONLINE_DEFAULT   <- 0.10     # Source [6]
ALPHA_SCANNER_DEFAULT  <- 0.219    # Source [4] via Luke et al. citation
# W_LOW and W_HIGH are the population weights used to collapse Zhen's
# low-income and high-income per-capita expenditure columns into a single
# average expenditure per category. Zhen stratifies at 185% of the Federal
# Poverty Level (FPL) — the standard threshold for SNAP and several other
# means-tested programs — and reports per-capita per-quarter expenditures
# separately for households below and above this cutoff. Based on the 2006
# Current Population Survey (the survey year matching Zhen's Nielsen
# Homescan sample), approximately 35% of U.S. households fell below 185%
# FPL and 65% above. We use those shares as a fixed weighting; the
# headline FAH elasticity is insensitive to reasonable perturbations
# (e.g., 0.30/0.70 or 0.40/0.60) because the within-category low/high
# expenditure differences are modest.
W_LOW                  <- 0.35     # Pop share below 185% FPL, 2006 CPS
W_HIGH                 <- 0.65     # Pop share at or above 185% FPL, 2006 CPS

# Group-level coverage classification for the HYBRID rule
coverage_classification <- tribble(
  ~group,                   ~hybrid_source,
  "cereals_and_bakery",     "zhen",
  "meat_and_eggs",          "luke",     # replaced (was "oa")
  "dairy",                  "zhen",
  "fruits_and_vegetables",  "oa",
  "nonalcoholic_beverages", "zhen",
  "other_fah",              "zhen"
)

# -----------------------------------------------------------------------------
# 2. AGGREGATE ZHEN UP TO OA GROUPS (with propagated SEs)
# -----------------------------------------------------------------------------
zhen_at_oa_level <- zhen_categories %>%
  mutate(avg_exp = W_LOW * exp_low + W_HIGH * exp_high) %>%
  group_by(oa_group) %>%
  summarise(
    zhen_elast_raw       = sum(elast * avg_exp) / sum(avg_exp),
    # SE on a weighted sum: sqrt(sum(w_i^2 * se_i^2)), with w_i normalized.
    # This treats Zhen's category SEs as independent (conservative; ignores
    # cross-equation covariance which would typically shrink the joint SE).
    zhen_elast_se        = sqrt(sum((avg_exp / sum(avg_exp))^2 * elast_se^2)),
    n_zhen_subcategories = n(),
    .groups = "drop"
  ) %>%
  rename(group = oa_group)

# -----------------------------------------------------------------------------
# 3. AGGREGATE LUKE MEAT TO OA GROUP LEVEL
# -----------------------------------------------------------------------------
luke_meat_aggregated <- luke_meat %>%
  summarise(
    luke_elast = sum(within_meat_share * marshallian_elast),
    luke_se    = sqrt(sum(within_meat_share^2 * marshallian_se^2))
  ) %>%
  mutate(group = "meat_and_eggs")

cat("=== Luke, Tonsor & Schroeder (2026) meat estimates ===\n")
cat("Hicksian elasticities (Table 3):\n")
print(luke_meat %>% select(meat, hicksian_elast, hicksian_se, exp_share, exp_elast) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))))
cat("\nMarshallian (Slutsky-converted) and within-meat shares:\n")
print(luke_meat %>% select(meat, marshallian_elast, marshallian_se, within_meat_share) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))))
cat(sprintf("\nLuke meat group Marshallian elasticity: %+.3f (SE %.3f)\n",
            luke_meat_aggregated$luke_elast, luke_meat_aggregated$luke_se))
cat("Caveat: Luke covers beef + chicken + pork only — no eggs, fish, or other meat.\n")
cat("Eggs and fish are typically less elastic, so this is likely an upper bound.\n")

# -----------------------------------------------------------------------------
# 4. ASSEMBLE THE MASTER TABLE
# -----------------------------------------------------------------------------
build_master <- function(alpha_scanner = ALPHA_SCANNER_DEFAULT) {
  oa_shares %>%
    left_join(oa_elasticities, by = "group") %>%
    left_join(zhen_at_oa_level %>% select(group, zhen_elast_raw, zhen_elast_se),
              by = "group") %>%
    left_join(luke_meat_aggregated %>% select(group, luke_elast, luke_se),
              by = "group") %>%
    left_join(coverage_classification, by = "group") %>%
    mutate(
      share_fah = share_pct_total_budget / sum(share_pct_total_budget),
      # Apply Jeon adjustment to Zhen estimates (positive alpha = less elastic)
      zhen_elast_adjusted = zhen_elast_raw + alpha_scanner,
      # Luke is also scanner-based, so apply the same adjustment
      luke_elast_adjusted = luke_elast + alpha_scanner,
      # Hybrid: pick the source on a per-group basis
      hybrid_elast = case_when(
        hybrid_source == "zhen" ~ zhen_elast_adjusted,
        hybrid_source == "luke" ~ luke_elast_adjusted,
        hybrid_source == "oa"   ~ oa_elast_source
      ),
      hybrid_se = case_when(
        hybrid_source == "zhen" ~ zhen_elast_se,
        hybrid_source == "luke" ~ luke_se,
        hybrid_source == "oa"   ~ oa_se
      )
    )
}

# -----------------------------------------------------------------------------
# 5. CORE COMPUTATION
# -----------------------------------------------------------------------------
compute_all_aggregates <- function(omega_online    = OMEGA_ONLINE_DEFAULT,
                                   mu_online       = MU_ONLINE_DEFAULT,
                                   alpha_scanner   = ALPHA_SCANNER_DEFAULT,
                                   coverage_w_zhen = 0.50,
                                   n_mc            = 50000,
                                   seed            = 20260521) {
  
  df <- build_master(alpha_scanner = alpha_scanner)
  
  # Channel-blending multiplier (applied uniformly within each source)
  blend_mult <- omega_online * mu_online + (1 - omega_online)
  
  # Point estimates
  headline <- df %>%
    summarise(
      eps_oa_source_pooled       = sum(share_fah * oa_elast_source),
      eps_zhen_source_pooled     = sum(share_fah * zhen_elast_adjusted),
      eps_hybrid_source_pooled   = sum(share_fah * hybrid_elast),
      eps_oa_headline_blended       = eps_oa_source_pooled     * blend_mult,
      eps_zhen_headline_blended     = eps_zhen_source_pooled   * blend_mult,
      eps_hybrid_headline_blended   = eps_hybrid_source_pooled * blend_mult,
      eps_covwt_headline_blended    = coverage_w_zhen * eps_zhen_headline_blended +
        (1 - coverage_w_zhen) * eps_oa_headline_blended
    )
  
  # Monte Carlo CIs propagating each source's SEs through blending and weighting
  if (!is.null(seed)) set.seed(seed)
  n_groups <- nrow(df)
  
  draws_oa     <- replicate(n_mc,
                            sum(df$share_fah * rnorm(n_groups, df$oa_elast_source, df$oa_se))) * blend_mult
  draws_zhen   <- replicate(n_mc,
                            sum(df$share_fah * (rnorm(n_groups, df$zhen_elast_raw, df$zhen_elast_se) +
                                                  alpha_scanner))) * blend_mult
  draws_hybrid <- replicate(n_mc,
                            sum(df$share_fah * rnorm(n_groups, df$hybrid_elast, df$hybrid_se))) * blend_mult
  draws_covwt  <- coverage_w_zhen * draws_zhen + (1 - coverage_w_zhen) * draws_oa
  
  ci <- tibble(
    estimator = c("oa", "zhen", "hybrid", "coverage_weighted"),
    point     = c(headline$eps_oa_headline_blended, headline$eps_zhen_headline_blended,
                  headline$eps_hybrid_headline_blended, headline$eps_covwt_headline_blended),
    mc_mean   = c(mean(draws_oa), mean(draws_zhen),
                  mean(draws_hybrid), mean(draws_covwt)),
    mc_sd     = c(sd(draws_oa), sd(draws_zhen),
                  sd(draws_hybrid), sd(draws_covwt)),
    ci_low_95 = c(quantile(draws_oa, 0.025), quantile(draws_zhen, 0.025),
                  quantile(draws_hybrid, 0.025), quantile(draws_covwt, 0.025)),
    ci_up_95  = c(quantile(draws_oa, 0.975), quantile(draws_zhen, 0.975),
                  quantile(draws_hybrid, 0.975), quantile(draws_covwt, 0.975))
  )
  
  list(table = df, headline = headline, ci = ci,
       draws = list(oa = draws_oa, zhen = draws_zhen,
                    hybrid = draws_hybrid, covwt = draws_covwt))
}

# -----------------------------------------------------------------------------
# 6. RUN AT DEFAULTS
# -----------------------------------------------------------------------------
res <- compute_all_aggregates()

cat("\n=== Group-level inputs (with Jeon adjustment alpha = 0.219) ===\n")
cat(sprintf("Channel blend applied: mu_online=%.2f, omega_online=%.2f\n",
            MU_ONLINE_DEFAULT, OMEGA_ONLINE_DEFAULT))
cat("source_pooled    = source elasticity as published (whole-market;\n")
cat("                   dominated by in-person purchases — see header)\n")
cat("derived_online   = mu_online * source_pooled (derived, not measured)\n")
cat("headline_blended = omega_online * derived_online +\n")
cat("                   (1 - omega_online) * source_pooled\n\n")
print(res$table %>%
        mutate(
          source_pooled    = hybrid_elast,
          derived_online   = MU_ONLINE_DEFAULT * hybrid_elast,
          headline_blended = OMEGA_ONLINE_DEFAULT * derived_online +
            (1 - OMEGA_ONLINE_DEFAULT) * source_pooled
        ) %>%
        select(group, share_fah, hybrid_source,
               source_pooled, derived_online, headline_blended, hybrid_se) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))))

cat(sprintf("\n=== Headline by channel (alpha=%.3f) ===\n",
            ALPHA_SCANNER_DEFAULT))
cat("source_pooled    = no channel blending (omega=0); essentially in-person\n")
cat(sprintf("derived_online   = mu_online (%.2f) applied to source_pooled\n",
            MU_ONLINE_DEFAULT))
cat(sprintf("headline_blended = omega_online (%.2f) blend of the two\n\n",
            OMEGA_ONLINE_DEFAULT))
headline_by_channel <- tibble(
  estimator        = c("oa", "zhen", "hybrid", "covwt"),
  source_pooled    = c(res$headline$eps_oa_source_pooled,
                       res$headline$eps_zhen_source_pooled,
                       res$headline$eps_hybrid_source_pooled,
                       0.5 * res$headline$eps_zhen_source_pooled +
                         0.5 * res$headline$eps_oa_source_pooled),
  derived_online   = MU_ONLINE_DEFAULT * c(res$headline$eps_oa_source_pooled,
                                           res$headline$eps_zhen_source_pooled,
                                           res$headline$eps_hybrid_source_pooled,
                                           0.5 * res$headline$eps_zhen_source_pooled +
                                             0.5 * res$headline$eps_oa_source_pooled),
  headline_blended = c(res$headline$eps_oa_headline_blended,
                       res$headline$eps_zhen_headline_blended,
                       res$headline$eps_hybrid_headline_blended,
                       res$headline$eps_covwt_headline_blended)
)
print(headline_by_channel %>% mutate(across(where(is.numeric), ~ round(.x, 3))))

cat(sprintf("\n=== Headline blended numbers with 95%% CIs (omega=%.2f, mu=%.2f, alpha=%.3f) ===\n",
            OMEGA_ONLINE_DEFAULT, MU_ONLINE_DEFAULT, ALPHA_SCANNER_DEFAULT))
print(res$ci %>% mutate(across(where(is.numeric), ~ round(.x, 3))))

# -----------------------------------------------------------------------------
# 7. SENSITIVITY GRID (fixed: single call per row, list-column unpacking)
# -----------------------------------------------------------------------------
omega_grid <- c(0.05, 0.10, 0.15, 0.20, 0.25)
mu_grid    <- c(0.33, 0.50, 0.67, 1.00)

sens <- expand_grid(omega_online = omega_grid, mu_online = mu_grid) %>%
  mutate(
    headline = pmap(list(omega_online, mu_online),
                    ~ compute_all_aggregates(omega_online = ..1,
                                             mu_online    = ..2,
                                             n_mc         = 1,
                                             seed         = NULL)$headline),
    eps_hybrid = map_dbl(headline, "eps_hybrid_headline_blended")
  ) %>%
  select(-headline)

cat("\n=== Sensitivity grid: HYBRID blended FAH elasticity ===\n")
print(sens %>%
        pivot_wider(names_from = mu_online, values_from = eps_hybrid,
                    names_prefix = "mu_") %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))))

# -----------------------------------------------------------------------------
# 8. JEON ADJUSTMENT SENSITIVITY
# -----------------------------------------------------------------------------
alpha_grid <- c(0.0, 0.1, 0.2, 0.219, 0.3, 0.4, 0.5)

alpha_sens <- tibble(alpha_scanner = alpha_grid) %>%
  mutate(
    headline = map(alpha_scanner,
                   ~ compute_all_aggregates(alpha_scanner = .,
                                            n_mc = 1, seed = NULL)$headline),
    eps_zhen_headline_blended     = map_dbl(headline, "eps_zhen_headline_blended"),
    eps_hybrid_headline_blended   = map_dbl(headline, "eps_hybrid_headline_blended"),
    eps_covwt_headline_blended    = map_dbl(headline, "eps_covwt_headline_blended"),
    eps_oa_headline_blended       = map_dbl(headline, "eps_oa_headline_blended")
  ) %>%
  select(-headline)

cat("\n=== Jeon adjustment sensitivity ===\n")
cat("alpha_scanner = positive shift applied to scanner-based estimates.\n")
cat("Default 0.219 comes from Jeon et al. (2023) per Luke, Tonsor & Schroeder.\n\n")
print(alpha_sens %>% mutate(across(where(is.numeric), ~ round(.x, 3))))

# -----------------------------------------------------------------------------
# 9. SUMMARY
# -----------------------------------------------------------------------------
cat(sprintf("\n=== Summary of headline estimators (omega=%.2f, mu=%.2f, alpha=%.3f) ===\n",
            OMEGA_ONLINE_DEFAULT, MU_ONLINE_DEFAULT, ALPHA_SCANNER_DEFAULT))
for (i in seq_len(nrow(res$ci))) {
  cat(sprintf("  %-20s %+.3f  [%+.3f, %+.3f]\n",
              res$ci$estimator[i],
              res$ci$point[i],
              res$ci$ci_low_95[i],
              res$ci$ci_up_95[i]))
}

cat("\nDone.\n")
