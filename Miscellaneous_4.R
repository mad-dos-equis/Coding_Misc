calculate_price_indices <- function(
    dt,
    hs_filter_dt = NULL,
    hs_filter_col = NULL,
    months = 4:9,
    countries = NULL,
    country_group = NULL,
    country_group_label = NULL,
    commodity_col = "COMMODITY",
    country_col = "PTN_ISO",
    year_col = "YEAR",
    month_col = "MONTH",
    value_col = "VALUE",
    unit_value_col = "UNIT_VALUE1",
    hs_digits = 6,
    month_label = NULL,
    pivot_wide = TRUE
) {
  
  
  # Validate inputs
  
  
  if (!is.data.table(dt)) dt <- as.data.table(dt)
  dt <- copy(dt)  # Avoid modifying original
  
  required_cols <- c(commodity_col, country_col, year_col, month_col, value_col, unit_value_col)
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Validate country_group params are both provided or both NULL
  if (xor(is.null(country_group), is.null(country_group_label))) {
    stop("Both 'country_group' and 'country_group_label' must be provided together")
  }
  
  # Standardize column names internally
  
  setnames(
    dt,
    old = c(commodity_col, country_col, year_col, month_col, value_col, unit_value_col),
    new = c("COMMODITY", "PTN_ISO", "YEAR", "MONTH", "VALUE", "UNIT_VALUE1")
  )
  
  # Step 1: Apply filters
  
  dt <- dt[MONTH %in% months]
  
# Country filtering
  if (!is.null(countries_exclude)) {
    dt <- dt[!PTN_ISO %in% countries_exclude]
  }
  
  if (!is.null(country_group)) {
    # Group specific countries, drop others
    dt[
      , PTN_ISO := fifelse(
        PTN_ISO %in% country_group,
        country_group_label,
        NA_character_
      )
    ]
    dt <- dt[!is.na(PTN_ISO)]
  } else if (!is.null(country_group_label)) {
    # Group ALL remaining countries under the label
    dt[, PTN_ISO := country_group_label]
  } else if (!is.null(countries)) {
    # Simple subset, no grouping
    dt <- dt[PTN_ISO %in% countries]
  }
  
  if (!is.null(hs_filter_dt) && !is.null(hs_filter_col)) {
    if (!is.data.table(hs_filter_dt)) hs_filter_dt <- as.data.table(hs_filter_dt)
    
    # Create HS key for joining
    hs_col_name <- paste0("HS", hs_digits)
    dt[, (hs_col_name) := substr(COMMODITY, 1, hs_digits)]
    
    # Ensure filter table has matching column name
    if (!hs_col_name %in% names(hs_filter_dt)) {
      # Assume first column is the HS code
      setnames(hs_filter_dt, 1, hs_col_name)
    }
    
    # Join and filter
    dt <- dt[
      hs_filter_dt,
      on = hs_col_name,
      nomatch = NULL
    ][
      get(hs_filter_col) == 1
    ]
  }
  
  # Step 2: Compute implied quantity
  
  dt[
    , q_m := fifelse(
      is.finite(VALUE) & VALUE > 0 & is.finite(UNIT_VALUE1) & UNIT_VALUE1 > 0,
      VALUE / UNIT_VALUE1,
      NA_real_
    )
  ]
  
  # Step 3: Monthly unit values at commodity level
  
  dt_monthly <- dt[
    , .(V = sum(VALUE, na.rm = TRUE),
        Q = sum(q_m, na.rm = TRUE)),
    by = .(PTN_ISO, COMMODITY, YEAR, MONTH)
  ][
    , P := fifelse(Q > 0, V / Q, NA_real_)
  ][
    is.finite(P) & P > 0 & is.finite(Q) & Q >= 0
  ]
  
  # Step 4: Compute lags for YoY comparison
  
  setorder(dt_monthly, PTN_ISO, COMMODITY, MONTH, YEAR)
  dt_monthly[
    , `:=`(
      P0 = shift(P, 1, type = "lag"),
      Q0 = shift(Q, 1, type = "lag"),
      V0 = shift(V, 1, type = "lag"),
      year0 = shift(YEAR, 1, type = "lag")
    ),
    by = .(PTN_ISO, COMMODITY, MONTH)
  ]
  
  dt_pairs <- dt_monthly[!is.na(P0) & !is.na(Q0) & year0 == YEAR - 1]
  
  # Step 5: Monthly indices at country-month level
  
  dt_idx_monthly <- dt_pairs[
    , .(
      PL_m = sum(P * Q0, na.rm = TRUE) / sum(P0 * Q0, na.rm = TRUE),
      PP_m = sum(P * Q, na.rm = TRUE) / sum(P0 * Q, na.rm = TRUE),
      V0_m = sum(V0, na.rm = TRUE)
    ),
    by = .(PTN_ISO, YEAR, MONTH)
  ][
    , PF_m := sqrt(PL_m * PP_m)
  ]
  
  # Step 6: Aggregate to multi-month level, weighted by base-period value
  
  # Use custom label if provided, otherwise auto-generate
  if (is.null(month_label)) {
    month_label <- if (length(months) == 1) {
      month.abb[months]
    } else {
      paste0(month.abb[min(months)], "_", month.abb[max(months)])
    }
  }
  
  dt_idx <- dt_idx_monthly[
    , .(
      PL = sum(PL_m * V0_m, na.rm = TRUE) / sum(V0_m, na.rm = TRUE),
      PP = sum(PP_m * V0_m, na.rm = TRUE) / sum(V0_m, na.rm = TRUE),
      PF = sum(PF_m * V0_m, na.rm = TRUE) / sum(V0_m, na.rm = TRUE)
    ),
    by = .(PTN_ISO, YEAR)
  ][
    , `:=`(
      Laspeyres_YoY_Pct = PL - 1,
      Paasche_YoY_Pct = PP - 1,
      Fisher_YoY_Pct = PF - 1
    )
  ]
  
  # Rename with month label
  setnames(
    dt_idx,
    old = c("PL", "PP", "PF", "Laspeyres_YoY_Pct", "Paasche_YoY_Pct", "Fisher_YoY_Pct"),
    new = paste0(
      c("PL_", "PP_", "PF_", "Laspeyres_YoY_", "Paasche_YoY_", "Fisher_YoY_"),
      month_label,
      c("", "", "", "_Pct", "_Pct", "_Pct")
    )
  )
  
  # Step 7: Optionally pivot wider
  
  if (pivot_wide) {
    value_vars <- setdiff(names(dt_idx), c("PTN_ISO", "YEAR"))
    dt_idx <- dcast(
      dt_idx,
      PTN_ISO ~ YEAR,
      value.var = value_vars,
      sep = "_"
    )
  }
  
  return(dt_idx[])
}


# East Asia group (replicates your original)
df2_east_asia <- calculate_price_indices(
  dt = df1,
  hs_filter_dt = list,
  hs_filter_col = "feature1",
  months = 4:9,
  country_group = c("TWN", "SGP", "MNG", "JPN", "PRK", "KOR"),
  country_group_label = "East_Asia"
)

# USMCA group
df2_usmca <- calculate_price_indices(
  dt = df1,
  hs_filter_dt = list,
  hs_filter_col = "feature1",
  months = 1:9,
  country_group = c("CAN", "MEX"),
  country_group_label = "USMCA",
  month_label = "YTD_Sep"
)

# EU group
df2_eu <- calculate_price_indices(
  dt = df1,
  months = 4:9,
  country_group = c("DEU", "FRA", "ITA", "NLD", "ESP", "BEL", "AUT", "IRL", "POL"),
  country_group_label = "EU"
)

# Simple country filter (no grouping) still works
df2_china <- calculate_price_indices(
  dt = df1,
  months = 4:9,
  countries = "CHN"
)
