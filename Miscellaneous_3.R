# ========================================
# FREUND-STYLE TRANSSHIPMENT ANALYSIS
# ========================================

# 1. Calculate baseline metrics matching Freund's approach
# Note: Freund uses 2017 as baseline, 2022 as comparison

# Total USA imports of Section 301 goods from China (baseline year)
baseline_china_imports <- trade_data[
  year == year1 & 
  importer == "USA" & 
  exporter == "China",
  sum(value, na.rm = TRUE)
]

# Total USA imports of Section 301 goods from China (current year)
current_china_imports <- trade_data[
  year == year5 & 
  importer == "USA" & 
  exporter == "China",
  sum(value, na.rm = TRUE)
]

# Calculate the decline in direct China imports
china_import_decline <- baseline_china_imports - current_china_imports
china_decline_pct <- (china_import_decline / baseline_china_imports) * 100

# 2. Calculate total USA imports from Mexico and Vietnam (current year)
usa_from_mexico_total <- trade_data[
  year == year5 & 
  importer == "USA" & 
  exporter == "Mexico",
  sum(value, na.rm = TRUE)
]

usa_from_vietnam_total <- trade_data[
  year == year5 & 
  importer == "USA" & 
  exporter == "Vietnam",
  sum(value, na.rm = TRUE)
]

# 3. Calculate growth in imports from Mexico and Vietnam
mexico_baseline <- trade_data[
  year == year1 & 
  importer == "USA" & 
  exporter == "Mexico",
  sum(value, na.rm = TRUE)
]

vietnam_baseline <- trade_data[
  year == year1 & 
  importer == "USA" & 
  exporter == "Vietnam",
  sum(value, na.rm = TRUE)
]

mexico_growth <- usa_from_mexico_total - mexico_baseline
vietnam_growth <- usa_from_vietnam_total - vietnam_baseline
mexico_growth_pct <- (mexico_growth / mexico_baseline) * 100
vietnam_growth_pct <- (vietnam_growth / vietnam_baseline) * 100

# 4. Calculate suspected transshipment values
mexico_transshipment <- results[
  third_country == "Mexico", 
  sum(transshipment_value, na.rm = TRUE)
]

vietnam_transshipment <- results[
  third_country == "Vietnam", 
  sum(transshipment_value, na.rm = TRUE)
]

total_transshipment <- sum(results$transshipment_value, na.rm = TRUE)

# 5. KEY FREUND METRICS
# Freund typically reports these specific metrics:

# A. Transshipment as share of current imports from the country
mexico_transship_share <- (mexico_transshipment / usa_from_mexico_total) * 100
vietnam_transship_share <- (vietnam_transshipment / usa_from_vietnam_total) * 100

# B. Share of total identified transshipment
mexico_of_total_transship <- (mexico_transshipment / total_transshipment) * 100
vietnam_of_total_transship <- (vietnam_transshipment / total_transshipment) * 100

# C. Transshipment as share of China's import decline
mexico_of_china_decline <- (mexico_transshipment / china_import_decline) * 100
vietnam_of_china_decline <- (vietnam_transshipment / china_import_decline) * 100

# D. "Excess growth" that might be attributed to transshipment
# (Growth beyond what might be expected without tariff evasion)
mexico_excess_share <- (mexico_transshipment / mexico_growth) * 100
vietnam_excess_share <- (vietnam_transshipment / vietnam_growth) * 100
