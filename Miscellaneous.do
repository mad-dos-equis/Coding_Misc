*-----------------------------
* STEP 2 (UPDATED): ESTIMATE ELASTICITIES WITH THIRD-MARKET IV
*-----------------------------
* Assumed core variables in your working dataset at this point:
* importer, exporter, year, hs6, trade_value, unit_value_cif
* If names differ, change locals here once.
local IMP    importer
local EXP    exporter
local YR     year
local PROD   hs6
local VAL    trade_value
local UV     unit_value_cif   // CIF unit value (price per physical unit)

* Basic hygiene
drop if `VAL'<=0 | `UV'<=0
gen double ln_v = ln(`VAL')
gen double ln_p = ln(`UV')

* --------- Build leave-one-destination-out (LODO) third-market price IV ---------
* Average *log* prices across other destinations for the same exporter×product×year
* (you can swap to log of mean if preferred)
bysort `EXP' `PROD' `YR': egen double _sum_ln_p = total(ln_p)
bysort `EXP' `PROD' `YR': egen long   _n       = count(ln_p)
gen double ln_p_3rd = (_sum_ln_p - ln_p) / (_n - 1) if _n>1
drop _sum_ln_p _n

* Optional: filter out cells without a valid instrument
drop if missing(ln_p_3rd)

* (Optional but recommended with CIF) Route/freight controls
* Uncomment/replace if you have them:
* gen double distxfuel = distance_km * bunker_price

*==============================
* A) IMPORT DEMAND ELASTICITIES
*   One regression per (importer × HS6):
*   Panel index = exporter over time
*   Model: ln V on instrumented ln P, FE for exporter and year
*   Report elasticity = beta - 1
*==============================

program drop _all
program define _iv_imp_cell, eclass
    version 18
    syntax
    xtset `EXP' `YR'
    quietly xtivreg ln_v (ln_p = ln_p_3rd) i.`YR', fe vce(cluster `EXP')
    ereturn post e(b) e(V)
end

tempfile _imp_res
statsby ///
    beta_p = _b[ln_p]  ///
    se_p   = _se[ln_p] ///
    N      = e(N),     ///
    by(`IMP' `PROD') clear: _iv_imp_cell
gen double imp_elast = beta_p - 1
label var imp_elast "Import demand elasticity (IV; beta-1)"
save "`_imp_res'", replace

*==============================
* B) EXPORT SUPPLY ELASTICITIES
*   One regression per (exporter × HS6):
*   Panel index = importer over time
*   Prefer FOB; if CIF only, include route controls if available.
*   Report elasticity = beta - 1
*==============================

program define _iv_exp_cell, eclass
    version 18
    syntax
    xtset `IMP' `YR'
    quietly xtivreg ln_v (ln_p = ln_p_3rd) i.`YR', fe vce(cluster `IMP')
    ereturn post e(b) e(V)
end

tempfile _exp_res
statsby ///
    beta_p = _b[ln_p]  ///
    se_p   = _se[ln_p] ///
    N      = e(N),     ///
    by(`EXP' `PROD') clear: _iv_exp_cell
gen double exp_elast = beta_p - 1
label var exp_elast "Export supply elasticity (IV; beta-1)"
save "`_exp_res'", replace

*-----------------------------
* (Optional) Standardize elasticities to 0–1 like Peterson
* (cap at 5th/95th percentiles within each block)
*-----------------------------
use "`_imp_res'", clear
summ imp_elast if imp_elast<., detail
local p5  = r(p5)
local p95 = r(p95)
gen double std_imp_elast = (imp_elast - `p5') / (`p95' - `p5')
replace std_imp_elast = 0 if imp_elast <= `p5'
replace std_imp_elast = 1 if imp_elast >= `p95'
save "$outputpath/import_elasticities_hs6.dta", replace

use "`_exp_res'", clear
summ exp_elast if exp_elast<., detail
local p5  = r(p5)
local p95 = r(p95)
gen double std_exp_elast = (exp_elast - `p5') / (`p95' - `p5')
replace std_exp_elast = 0 if exp_elast <= `p5'
replace std_exp_elast = 1 if exp_elast >= `p95'
save "$outputpath/export_elasticities_hs6.dta", replace
