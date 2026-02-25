# ── 6b. Aggregate projected ETR ──────────────────────────────────────────────
# Trade-weighted ETR under Section 122, using projected import values as weights.
# This is the effective rate the economy actually faces, not the statutory 15%.

M_proj_total   <- sum(hts10_projected$import_value_proj)
R_proj_total   <- sum(hts10_projected$duties_proj)
tau_proj_tw    <- R_proj_total / M_proj_total

# Also compute using baseline import weights (Laspeyres-style) for comparison
R_proj_laspeyres <- sum(hts10_projected$tau_eff_proj * hts10_projected$import_value)
tau_proj_laspeyres <- R_proj_laspeyres / M_baseline

# By-bucket projected ETRs
etr_by_bucket <- hts10_projected |>
  group_by(bucket) |>
  summarise(
    import_value_base = sum(import_value),
    import_value_proj = sum(import_value_proj),
    duties_base       = sum(duties),
    duties_proj       = sum(duties_proj),
    tw_etr_base       = sum(duties) / sum(import_value),
    tw_etr_proj       = sum(duties_proj) / sum(import_value_proj),
    tw_etr_proj_laspeyres = sum(tau_eff_proj * import_value) / sum(import_value),
    .groups = "drop"
  )

# Summary table
etr_summary <- tibble(
  metric = c(
    "Baseline trade-weighted ETR",
    "Projected trade-weighted ETR (Paasche — projected weights)",
    "Projected trade-weighted ETR (Laspeyres — baseline weights)",
    "Statutory Section 122 rate",
    "ETR increase (Paasche)",
    "ETR increase (Laspeyres)"
  ),
  value = c(
    tau_baseline,
    tau_proj_tw,
    tau_proj_laspeyres,
    TAU_NEW,
    tau_proj_tw - tau_baseline,
    tau_proj_laspeyres - tau_baseline
  ),
  formatted = c(
    fmt_pct(tau_baseline),
    fmt_pct(tau_proj_tw),
    fmt_pct(tau_proj_laspeyres),
    fmt_pct(TAU_NEW),
    fmt_pct(tau_proj_tw - tau_baseline),
    fmt_pct(tau_proj_laspeyres - tau_baseline)
  )
)

cat("══ Aggregate Effective Tariff Rate Comparison ════════════════\n")
cat("ε =", EPSILON_PRIMARY, ", φ =", PHI_PRIMARY, "\n\n")
cat("Baseline trade-weighted ETR:       ", fmt_pct(tau_baseline), "\n")
cat("Projected ETR (Paasche weights):   ", fmt_pct(tau_proj_tw), "\n")
cat("Projected ETR (Laspeyres weights): ", fmt_pct(tau_proj_laspeyres), "\n")
cat("Statutory Section 122 rate:        ", fmt_pct(TAU_NEW), "\n")
cat("ETR increase (Paasche):            ", fmt_pct(tau_proj_tw - tau_baseline), "\n")
cat("ETR increase (Laspeyres):          ", fmt_pct(tau_proj_laspeyres - tau_baseline), "\n\n")
cat("By bucket:\n")
print(etr_by_bucket)
cat("\n")
