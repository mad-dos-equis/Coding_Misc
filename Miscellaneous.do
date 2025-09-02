******STEP 2: ESTIMATE ELASTICITIES USING XTIVREG (OPTIMIZED)******
// Set analysis level for elasticity estimation
global analysis_level "hs4"  // Change this to "hs6" or "hs2" as needed

display "Estimating elasticities at $analysis_level level using xtivreg"

//==============================================================================
// IMPORT DEMAND ELASTICITIES
//==============================================================================
use "$workpath/final_value_.dta", clear

// Check data availability
count if !missing(lntrueval) & !missing(lnunitval) & !missing(lnthirdval_i)
if r(N) == 0 {
    display as error "No valid data for import elasticity estimation"
    exit 198
}

// Set panel structure for import elasticities
// We use exporter as panel var since we want exporter FE
xtset exporter year

// Estimate import demand elasticities by importer-commodity group
// Using third-party import prices as instrument for own price
display "Estimating import demand elasticities..."

statsby raw_coef=_b[lnunitval] se_elasticity=_se[lnunitval] ///
    observations=e(N) sigma_u=e(sigma_u) sigma_e=e(sigma_e) ///
    rho=e(rho), by(importer $analysis_level) ///
    saving("$outputpath/import_elasticities_raw_$analysis_level.dta", replace): ///
    xtivreg lntrueval (lnunitval = lnthirdval_i), fe vce(robust)

// Process import elasticities
use "$outputpath/import_elasticities_raw_$analysis_level.dta", clear
drop if missing(raw_coef)

// CALCULATE TRUE ELASTICITY: subtract 1 from coefficient
gen elasticity = raw_coef - 1
label variable elasticity "Import demand elasticity (ε_d)"
label variable se_elasticity "Standard error of elasticity"
label variable rho "Fraction of variance due to fixed effect"

// Quality checks for import elasticities
gen wrong_sign = (elasticity > 0)  // Import elasticities should be negative
label variable wrong_sign "Indicator for wrong-signed elasticity"

summarize elasticity, detail
display _newline
display "=== IMPORT ELASTICITY DIAGNOSTICS ==="
display "Mean elasticity: " r(mean)
display "Median elasticity: " r(p50)
display "Standard deviation: " r(sd)

count if wrong_sign == 1
display "Elasticities with wrong sign (positive): " r(N) " out of " _N
display "Percentage with wrong sign: " r(N)/_N * 100 "%"

// Handle outliers and standardize
summarize elasticity, detail
scalar p1_imp = r(p1)
scalar p99_imp = r(p99)

display "Trimming at 1st percentile: " p1_imp " and 99th percentile: " p99_imp

gen elasticity_trimmed = elasticity
replace elasticity_trimmed = p1_imp if elasticity < p1_imp & !missing(elasticity)
replace elasticity_trimmed = p99_imp if elasticity > p99_imp & !missing(elasticity)

// Standardize to [0,1] interval
gen std_imp_elast = (elasticity_trimmed - p1_imp) / (p99_imp - p1_imp)
label variable std_imp_elast "Standardized import elasticity [0,1]"

drop elasticity_trimmed
save "$outputpath/import_elasticities_$analysis_level.dta", replace

//==============================================================================
// EXPORT SUPPLY ELASTICITIES
//==============================================================================
use "$workpath/final_value_.dta", clear

// Check data availability
count if !missing(lntrueval) & !missing(lnunitval) & !missing(lnthirdval_e)
if r(N) == 0 {
    display as error "No valid data for export elasticity estimation"
    exit 198
}

// Set panel structure for export elasticities
// We use importer as panel var since we want importer FE
xtset importer year

// Estimate export supply elasticities by exporter-commodity group
display _newline
display "Estimating export supply elasticities..."

statsby raw_coef=_b[lnunitval] se_elasticity=_se[lnunitval] ///
    observations=e(N) sigma_u=e(sigma_u) sigma_e=e(sigma_e) ///
    rho=e(rho), by(exporter $analysis_level) ///
    saving("$outputpath/export_elasticities_raw_$analysis_level.dta", replace): ///
    xtivreg lntrueval (lnunitval = lnthirdval_e), fe vce(robust)

// Process export elasticities
use "$outputpath/export_elasticities_raw_$analysis_level.dta", clear
drop if missing(raw_coef)

// CALCULATE TRUE ELASTICITY: subtract 1 from coefficient
gen elasticity = raw_coef - 1
label variable elasticity "Export supply elasticity (ε_s)"

// Quality checks for export elasticities
gen wrong_sign = (elasticity < 0)  // Export elasticities should be positive
label variable wrong_sign "Indicator for wrong-signed elasticity"

summarize elasticity, detail
display _newline
display "=== EXPORT ELASTICITY DIAGNOSTICS ==="
display "Mean elasticity: " r(mean)
display "Median elasticity: " r(p50)
display "Standard deviation: " r(sd)

count if wrong_sign == 1
display "Elasticities with wrong sign (negative): " r(N) " out of " _N
display "Percentage with wrong sign: " r(N)/_N * 100 "%"

// Handle outliers and standardize
summarize elasticity, detail
scalar p1_exp = r(p1)
scalar p99_exp = r(p99)

display "Trimming at 1st percentile: " p1_exp " and 99th percentile: " p99_exp

gen elasticity_trimmed = elasticity
replace elasticity_trimmed = p1_exp if elasticity < p1_exp & !missing(elasticity)
replace elasticity_trimmed = p99_exp if elasticity > p99_exp & !missing(elasticity)

// Note: Export elasticities are inverted (1 - standardized value)
gen std_exp_elast = 1 - ((elasticity_trimmed - p1_exp) / (p99_exp - p1_exp))
label variable std_exp_elast "Standardized export elasticity [0,1] (inverted)"

drop elasticity_trimmed
save "$outputpath/export_elasticities_$analysis_level.dta", replace

//==============================================================================
// ADDITIONAL ROBUSTNESS CHECKS (OPTIONAL)
//==============================================================================
display _newline
display "=== PERFORMING ROBUSTNESS CHECKS ==="

// 1. Check first-stage strength (manually since xtivreg doesn't report F-stat)
use "$workpath/final_value_.dta", clear
xtset exporter year

// Run first stage regression for a sample
xtreg lnunitval lnthirdval_i i.year, fe
test lnthirdval_i
local f_stat = r(F)
display "Sample first-stage F-statistic for import IV: " `f_stat'
if `f_stat' < 10 {
    display "WARNING: Weak instrument detected (F < 10)"
}

// 2. Alternative: Estimate with different FE structure as robustness
// For example, include time-varying country effects
display _newline
display "Running robustness check with year-specific effects..."

use "$workpath/final_value_.dta", clear

// Create year-importer interactions for time-varying effects
egen importer_year = group(importer year)
xtset exporter

// Re-estimate with additional controls (example for imports)
statsby raw_coef_robust=_b[lnunitval] se_robust=_se[lnunitval], ///
    by(importer $analysis_level) ///
    saving("$outputpath/import_elasticities_robust_$analysis_level.dta", replace): ///
    xtivreg lntrueval i.importer_year (lnunitval = lnthirdval_i), fe vce(robust)

// Compare with main results
use "$outputpath/import_elasticities_robust_$analysis_level.dta", clear
gen elasticity_robust = raw_coef_robust - 1
merge 1:1 importer $analysis_level using "$outputpath/import_elasticities_$analysis_level.dta", ///
    keepusing(elasticity) nogen

gen diff = elasticity_robust - elasticity
summarize diff, detail
display _newline
display "Robustness check - difference in elasticities:"
display "Mean difference: " r(mean)
display "Max absolute difference: " max(abs(r(min)), abs(r(max)))

// 3. Save summary statistics
preserve
use "$outputpath/import_elasticities_$analysis_level.dta", clear
gen elast_type = "Import"
append using "$outputpath/export_elasticities_$analysis_level.dta"
replace elast_type = "Export" if missing(elast_type)

collapse (mean) mean_elast=elasticity ///
         (median) median_elast=elasticity ///
         (sd) sd_elast=elasticity ///
         (count) n_obs=elasticity ///
         (mean) pct_wrong_sign=wrong_sign, ///
         by(elast_type)

list, clean noobs
save "$outputpath/elasticity_summary_stats_$analysis_level.dta", replace
restore

//==============================================================================
// CLEANUP
//==============================================================================
// Remove intermediate files
capture erase "$outputpath/import_elasticities_raw_$analysis_level.dta"
capture erase "$outputpath/export_elasticities_raw_$analysis_level.dta"
capture erase "$outputpath/import_elasticities_robust_$analysis_level.dta"

display _newline
display "========================================"
display "ELASTICITY ESTIMATION COMPLETE"
display "========================================"
display "Analysis level: $analysis_level"
display "Output files saved:"
display "  - $outputpath/import_elasticities_$analysis_level.dta"
display "  - $outputpath/export_elasticities_$analysis_level.dta"
display "  - $outputpath/elasticity_summary_stats_$analysis_level.dta"
display ""
display "Key points:"
display "  - Elasticities estimated using IV approach with xtivreg"
display "  - True elasticities calculated by subtracting 1 from coefficients"
display "  - Import elasticities should be negative (demand curve)"
display "  - Export elasticities should be positive (supply curve)"
display "  - Standardized elasticities mapped to [0,1] interval"
display "========================================"
