df2 <- df1 %>%
  lazy_dt() %>%
  mutate(HS6 = substr(COMMODITY, 1, 6)) %>%
  left_join(list, by = "HS6") %>%
  filter(feature1 == 1) %>%
  filter(MONTH %in% c(4, 5, 6, 7, 8, 9)) %>%
  mutate(
    q_m = if_else(
      is.finite(VALUE) & VALUE > 0 & is.finite(UNIT_VALUE1) & UNIT_VALUE1 > 0,
      VALUE / UNIT_VALUE1,
      NA_real_
    )
  ) %>%
  # Step 1: Monthly unit values at commodity level
  summarise(
    V = sum(VALUE, na.rm = TRUE),
    Q = sum(q_m, na.rm = TRUE),
    .by = c(PTN_ISO, COMMODITY, YEAR, MONTH)
  ) %>%
  mutate(P = if_else(Q > 0, V / Q, NA_real_)) %>%
  filter(is.finite(P), P > 0, is.finite(Q), Q >= 0) %>%
  # Step 2: Lag for YoY comparison (within commodity-month)
  arrange(PTN_ISO, COMMODITY, MONTH, YEAR) %>%
  mutate(
    P0 = lag(P),
    Q0 = lag(Q),
    V0 = lag(V),
    year0 = lag(YEAR),
    .by = c(PTN_ISO, COMMODITY, MONTH)
  ) %>%
  filter(!is.na(P0), !is.na(Q0), year0 == YEAR - 1) %>%
  # Step 3: Monthly indices at country-month level
  summarise(
    PL_m = sum(P * Q0, na.rm = TRUE) / sum(P0 * Q0, na.rm = TRUE),
    PP_m = sum(P * Q, na.rm = TRUE) / sum(P0 * Q, na.rm = TRUE),
    V0_m = sum(V0, na.rm = TRUE),
    .by = c(PTN_ISO, YEAR, MONTH)
  ) %>%
  mutate(PF_m = sqrt(PL_m * PP_m)) %>%
  # Step 4: Aggregate to 6-month level, weighted by base-period value
  summarise(
    PL_Apr_Sep = sum(PL_m * V0_m, na.rm = TRUE) / sum(V0_m, na.rm = TRUE),
    PP_Apr_Sep = sum(PP_m * V0_m, na.rm = TRUE) / sum(V0_m, na.rm = TRUE),
    PF_Apr_Sep = sum(PF_m * V0_m, na.rm = TRUE) / sum(V0_m, na.rm = TRUE),
    .by = c(PTN_ISO, YEAR)
  ) %>%
  mutate(
    Laspeyres_YoY_Apr_Sep_Pct = PL_Apr_Sep - 1,
    Paasche_YoY_Apr_Sep_Pct = PP_Apr_Sep - 1,
    Fisher_YoY_Apr_Sep_Pct = PF_Apr_Sep - 1
  ) %>%
  pivot_wider(
    id_cols = PTN_ISO,
    names_from = YEAR,
    values_from = c(PL_Apr_Sep, PP_Apr_Sep, PF_Apr_Sep, 
                    Laspeyres_YoY_Apr_Sep_Pct, Paasche_YoY_Apr_Sep_Pct, 
                    Fisher_YoY_Apr_Sep_Pct),
    names_glue = "{.value}_{YEAR}"
  ) %>%
  as_tibble()
