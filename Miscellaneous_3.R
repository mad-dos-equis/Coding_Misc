# ══════════════════════════════════════════════════════════════════════════════
# CES TRADE MODEL SIMULATION: CONTROL RATE ANALYSIS
# ══════════════════════════════════════════════════════════════════════════════
# 
# Purpose: Monte Carlo simulation of trade policy controls using a CES framework
# - Simulates uncertainty in elasticity parameters (sigma, omega)
# - Computes welfare effects, trade diversion, and market concentration
# - Analyzes pass-through rates and price index changes
# 
# Model: Standard CES trade model with import controls on focal exporter
# Key assumption: Sigma > 1 (substitutes), which ensures well-defined price index
# ══════════════════════════════════════════════════════════════════════════════

# ── Setup ──────────────────────────────────────────
library(dplyr)      # data manipulation
library(tidyr)      # data reshaping (crossing function)
library(readr)      # CSV reading
library(truncnorm)  # for truncated-normal parameter draws
# library(MASS)    # uncomment if you want correlated mvrnorm draws

set.seed(123)  # for reproducible Monte Carlo results

# ── 1. Load & prep panel ──────────────────────────
# Load bilateral trade data and focus on Britain as importer
df <- read_csv("wine_panel.csv") %>%
  filter(importer == "Britain") %>%
  mutate(
    V_exporter = V0,                           # rename for clarity (trade values)
    share      = V_exporter / sum(V_exporter)  # compute value shares (sum to 1)
  )

# Define focal exporter for policy analysis
j <- "France"

# Extract elasticity estimates & standard errors for Britain–France trade
# sigma: elasticity of substitution (>1 means substitutes)
# omega: additional parameter (interpretation depends on specific model)
sigma_hat <- df$sigma_hat[df$exporter == j]  # point estimate
sigma_se  <- df$sigma_se [df$exporter == j]  # standard error
omega_hat <- df$omega_hat[df$exporter == j]  # point estimate  
omega_se  <- df$omega_se [df$exporter == j]  # standard error

# Optional: if you have a covariance matrix for (sigma,omega)
# This would account for correlation between parameter estimates
# cov_matrix <- matrix(c(sigma_se^2, cov_so, cov_so, omega_se^2), 2, 2)

# Simulation settings
control_rates <- c(0.25, 0.50, 0.75)  # tariff/control rates to analyze
n_sims        <- 5000                  # number of Monte Carlo draws
L_target      <- 0.10                  # target welfare loss (10%) for threshold analysis
V_total       <- sum(df$V_exporter)    # total import spending (baseline)
baseline_HHI  <- sum(df$share^2)       # baseline Herfindahl concentration index

# ── 2. Simulate σ, ω & compute threshold c* ────────────────────────────────
# Monte Carlo draws of elasticity parameters with proper uncertainty
sims_base <- tibble(sim = 1:n_sims) %>%
  mutate(
    # Draw from truncated normal distributions to respect theoretical constraints
    # sigma > 1+epsilon ensures CES price index is well-defined
    # omega > epsilon ensures positive values (exact constraint depends on model)
    sigma = rtruncnorm(n_sims, a = 1 + 1e-6, b = Inf, mean = sigma_hat, sd = sigma_se),
    omega = rtruncnorm(n_sims, a = 1e-6,     b = Inf, mean = omega_hat, sd = omega_se),
    
    # Alternative: correlated parameter draws (more realistic but requires covariance matrix)
    # draws <- mvrnorm(n_sims, mu = c(sigma_hat, omega_hat), Sigma = cov_matrix)
    # sigma <- pmax(draws[,1], 1+1e-6)
    # omega <- pmax(draws[,2], 1e-6)
    
    # Threshold analysis: what control rate c* yields exactly L_target welfare loss?
    # Derived from CES welfare formula: solves for c when ΔCS/V_total = L_target
    c_star = (sigma + omega) * (
      (1 - L_target * (sigma - 1))^(1 / (1 - sigma)) - 1
    )
  )

# Summarize threshold distribution (confidence intervals for policy guidance)
threshold_summary <- sims_base %>%
  summarise(
    cstar_med  = median(c_star),                    # median threshold
    cstar_lo95 = quantile(c_star, 0.025),          # 95% CI lower bound
    cstar_hi95 = quantile(c_star, 0.975)           # 95% CI upper bound
  )

# ── 3. Expand for each control rate & compute pass-through ────────────────
# Create full simulation grid: each draw × each control rate scenario
sims <- sims_base %>%
  select(sim, sigma, omega) %>%                     # keep only needed parameters
  crossing(tibble(c = control_rates)) %>%          # cartesian product with control rates
  mutate(
    # Price ratio for France under control rate c
    # Assumes control acts like ad-valorem tariff: p_new = p_old * (1 + c/(σ+ω))
    pr_j        = 1 + c / (sigma + omega),
    
    # Pass-through rate: how much of control cost passes to consumer prices
    # = ∂ln(p)/∂ln(1+c) = 1/(σ+ω) exactly (constant elasticity result)
    pass_through = 1 / (sigma + omega)
  )

# ── 4. Compute CES metrics per sim, c, exporter ─────────────────────────
# Expand to exporter level and compute CES demand responses
expanded <- sims %>%
  crossing(df) %>%                                  # cartesian product with all exporters
  mutate(
    # Price ratios: France faces pr_j, others face pr=1 (no control)
    pr = if_else(exporter == j, pr_j, 1),
    
    # CES "attractiveness" factor: f_i = (p_i/P)^(-σ) 
    # Higher f means more attractive (lower relative price)
    f  = pr^(-sigma)
  ) %>%
  group_by(sim, c) %>%
  mutate(
    # Normalization constant for CES shares: H = Σ_i s_i^0 * f_i
    # Used to construct aggregate price index and renormalize shares
    H       = sum(share * f),
    
    # Aggregate price index ratio: P_new/P_old = H^(1/(1-σ))
    # This is the standard CES price index formula
    P_ratio = H^(1 / (1 - sigma[1]))              # use first sigma (all equal within group)
  ) %>%
  ungroup() %>%
  mutate(
    # New equilibrium shares: CES demand reallocation
    # s_i^new = s_i^old * f_i / H (ensures shares sum to 1)
    share_new = share * f / H,
    
    # New import values: apply new shares to total spending
    # Assumes total import spending V_total remains constant
    V_new     = V_total * share_new,
    
    # Quantity ratios: Q_new/Q_old = (s_new/s_old) * (P_old/P_new)
    # Since V=PQ and total V constant: Q_ratio = share_ratio / P_ratio
    Q_ratio   = share_new / share,
    
    # New quantities: multiply baseline quantities by quantity ratios
    # Requires Q0 to be present in input data
    Q_new     = Q0 * Q_ratio
  )

# ── 5. Summarize metrics per draw ───────────────────────────────────────
# Compute key economic metrics for each simulation draw and control rate
sim_metrics <- expanded %>%
  group_by(sim, c) %>%
  summarise(
    # Consumer surplus loss (Equivalent Variation measure)
    # ΔCS = V_total * (H-1)/(1-σ) from CES expenditure function
    # Negative values indicate welfare loss
    delta_CS = V_total * (unique(H) - 1) / (1 - unique(sigma)),
    
    # Trade diversion analysis: how much of France's lost trade do others gain?
    delta_j     = sum(V_new[exporter == j] - V_exporter[exporter == j]),    # France's change
    gain_others = sum((V_new - V_exporter)[exporter != j]),                 # Others' total gain
    
    # Fill rate: fraction of France's lost imports captured by other exporters
    # = (Others' gain) / |France's loss|
    # NA if France's change is negligible (prevents division by ~zero)
    fill_rate   = if_else(abs(delta_j) < 1e-8, NA_real_, gain_others / -delta_j),
    
    # Pass-through rate (constant across exporters within simulation)
    pass_through = unique(pass_through),
    
    # Aggregate price index change
    P_ratio      = unique(P_ratio),
    
    # New market concentration (Herfindahl-Hirschman Index)
    # Higher values indicate more concentrated market structure
    HHI_new      = sum(share_new^2),
    
    .groups = "drop"
  )

# ── 6. Monte Carlo summaries by control rate ──────────────────────────────
# Aggregate across all Monte Carlo draws to get confidence intervals
summary_results <- sim_metrics %>%
  group_by(c) %>%
  summarise(
    # Consumer surplus loss distribution
    CS_med    = median(delta_CS),
    CS_lo95   = quantile(delta_CS, 0.025),
    CS_hi95   = quantile(delta_CS, 0.975),
    
    # Fill rate distribution (na.rm needed due to edge cases)
    fill_med  = median(fill_rate, na.rm=TRUE),
    fill_lo95 = quantile(fill_rate, 0.025, na.rm=TRUE),
    fill_hi95 = quantile(fill_rate, 0.975, na.rm=TRUE),
    
    # Pass-through rate distribution
    pass_med  = median(pass_through),
    pass_lo95 = quantile(pass_through, 0.025),
    pass_hi95 = quantile(pass_through, 0.975),
    
    # Price index ratio distribution
    Pidx_med  = median(P_ratio),
    Pidx_lo95 = quantile(P_ratio, 0.025),
    Pidx_hi95 = quantile(P_ratio, 0.975),
    
    # Market concentration distribution  
    HHI_med   = median(HHI_new),
    HHI_lo95  = quantile(HHI_new, 0.025),
    HHI_hi95  = quantile(HHI_new, 0.975)
  )

# ── 7. Partner-level medians (quantities & shares) ────────────────────────
# Summarize impacts by individual exporting country
partner_summary <- expanded %>%
  group_by(exporter, c) %>%
  summarise(
    # Median quantity ratio: how much does each country's quantity change?
    Q_ratio_med   = median(Q_ratio),
    
    # Median new quantity level
    Q_new_med     = median(Q_new),
    
    # Median new market share
    share_new_med = median(share_new),
    
    .groups = "drop"
  )

# ── 8. Output ─────────────────────────────────────────────────────────────
# Display key results

print("=== THRESHOLD ANALYSIS ===")
print("Control rate c* that generates 10% welfare loss:")
print(threshold_summary)    

print("\n=== AGGREGATE RESULTS BY CONTROL RATE ===") 
print("Welfare loss, fill rate, pass-through, price index, and concentration:")
print(summary_results)      

print("\n=== PARTNER-LEVEL RESULTS ===")
print("Quantity ratios, new quantities, and market shares by exporter:")
print(partner_summary)

# ══════════════════════════════════════════════════════════════════════════════
# END OF SCRIPT
# 
# Key outputs:
# - threshold_summary: Confidence intervals for control rate causing 10% welfare loss
# - summary_results: Main economic impacts across different control rates  
# - partner_summary: Country-specific trade effects
# 
# Interpretation notes:
# - delta_CS < 0: welfare loss (more negative = larger loss)
# - fill_rate ∈ [0,1]: fraction of France's lost trade captured by others
# - pass_through ∈ [0,1]: how much control cost passes to consumer prices
# - P_ratio > 1: aggregate price increase
# - HHI_new vs baseline_HHI: change in market concentration
# ══════════════════════════════════════════════════════════════════════════════
