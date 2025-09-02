* --- 0) PREP: logs + leave-one-destination-out instrument --------------------
drop if missing(importer, exporter, hs6, year) | trade_value<=0 | unit_value_cif<=0
gen double ln_v = ln(trade_value)
gen double ln_p = ln(unit_value_cif)

bys exporter hs6 year: egen double _sum_ln_p = total(ln_p)
bys exporter hs6 year: egen long   _n       = count(ln_p)
gen double ln_p_3rd = (_sum_ln_p - ln_p) / (_n - 1) if _n>1
drop _sum_ln_p _n
drop if missing(ln_p_3rd)

* --- 1) PREFILTER groups so statsby only iterates over feasible cells ----------
* Need: at least 2 years, at least 2 counterparties, and variation in ln_p & ln_p_3rd

* For import-demand cells (by importer×hs6)
bys importer hs6: egen int n_obs_ih   = count(ln_v)
bys importer hs6: egen int n_years_ih = nvals(year)
bys importer hs6: egen int n_exps_ih  = nvals(exporter)
bys importer hs6: egen double sd_p_ih = sd(ln_p)
bys importer hs6: egen double sd_z_ih = sd(ln_p_3rd)
gen byte ok_ih = n_obs_ih>=10 & n_years_ih>=2 & n_exps_ih>=2 & sd_p_ih>0 & sd_z_ih>0

* For export-supply cells (by exporter×hs6)
bys exporter hs6: egen int n_obs_eh   = count(ln_v)
bys exporter hs6: egen int n_years_eh = nvals(year)
bys exporter hs6: egen int n_imps_eh  = nvals(importer)
bys exporter hs6: egen double sd_p_eh = sd(ln_p)
bys exporter hs6: egen double sd_z_eh = sd(ln_p_3rd)
gen byte ok_eh = n_obs_eh>=10 & n_years_eh>=2 & n_imps_eh>=2 & sd_p_eh>0 & sd_z_eh>0

* --- 2) Defensive wrapper: post missings if a cell fails ----------------------
capture program drop _iv_imp_cell
program define _iv_imp_cell, eclass
    version 16
    syntax
    capture noisily ivreghdfe ln_v (ln_p = ln_p_3rd), absorb(exporter year) cluster(exporter)
    if _rc {
        tempname b V
        matrix `b' = J(1,1,.)
        matrix colnames `b' = ln_p
        matrix rownames `b' = ln_v
        matrix `V' = J(1,1,.)
        ereturn post `b' `V'
        ereturn scalar N = 0
        exit
    }
    * Re-post so statsby always sees e(b)/e(V)
    ereturn post e(b) e(V)
end

capture program drop _iv_exp_cell
program define _iv_exp_cell, eclass
    version 16
    syntax
    capture noisily ivreghdfe ln_v (ln_p = ln_p_3rd), absorb(importer year) cluster(importer)
    if _rc {
        tempname b V
        matrix `b' = J(1,1,.)
        matrix colnames `b' = ln_p
        matrix rownames `b' = ln_v
        matrix `V' = J(1,1,.)
        ereturn post `b' `V'
        ereturn scalar N = 0
        exit
    }
    ereturn post e(b) e(V)
end

* --- 3) IMPORT DEMAND: by(importer hs6) ---------------------------------------
preserve
keep if ok_ih
statsby ///
    beta_p = _b[ln_p]  ///
    se_p   = _se[ln_p] ///
    N      = e(N),     ///
    by(importer hs6) clear: _iv_imp_cell

gen double imp_elast = beta_p - 1
label var imp_elast "Import demand elasticity (IV; beta-1)"
save "results_iv_by_importer_hs6.dta", replace
restore

* --- 4) EXPORT SUPPLY: by(exporter hs6) ---------------------------------------
preserve
keep if ok_eh
statsby ///
    beta_p = _b[ln_p]  ///
    se_p   = _se[ln_p] ///
    N      = e(N),     ///
    by(exporter hs6) clear: _iv_exp_cell

gen double exp_elast = beta_p - 1
label var exp_elast "Export supply elasticity (IV; beta-1)"
save "results_iv_by_exporter_hs6.dta", replace
restore
