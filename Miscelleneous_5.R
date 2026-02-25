# ── 6c. Projected ETR sensitivity over ε × φ ────────────────────────────────

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

etr_matrix_paasche <- etr_sensitivity |>
  select(epsilon_label, phi_label, tw_etr_proj_paasche) |>
  pivot_wider(names_from = phi_label, values_from = tw_etr_proj_paasche)

etr_matrix_laspeyres <- etr_sensitivity |>
  select(epsilon_label, phi_label, tw_etr_proj_laspeyres) |>
  pivot_wider(names_from = phi_label, values_from = tw_etr_proj_laspeyres)

cat("══ Projected Trade-Weighted ETR Sensitivity ══════════════════\n")
cat("Baseline ETR:", fmt_pct(tau_baseline), "\n\n")
cat("Paasche (projected import weights):\n")
print(etr_matrix_paasche |> mutate(across(-epsilon_label, fmt_pct)))
cat("\nLaspeyres (baseline import weights):\n")
print(etr_matrix_laspeyres |> mutate(across(-epsilon_label, fmt_pct)))
cat("\n")
