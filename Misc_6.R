#' EU Regulatory Compliance Cost Pipeline -- LONG-FORM REWRITE
#' ==========================================================================
#'
#' Produces ONE deliverable:
#'
#'   regulation_firm_counts_long.csv
#'     One row per (regulation_name, naics_code, naics_depth, nace_code).
#'     Columns:
#'       regulation_name, regulation_name_raw,
#'       naics_code, naics_depth, naics_title,
#'       nace_code,   -- FATS NACE cell, or sentinel:
#'                    --   "B-S_X_O_S94" for horizontal rows
#'                    --   "BEA:<bucket label>" for BEA gap-fill rows
#'       source,      -- "FATS" | "BEA_gap_fill" | "FATS_horizontal_aggregate"
#'       allocated_firms,        -- per-row firm-count assignment
#'       weight,                 -- the share of the NACE/bucket total assigned to this NAICS
#'       nace_total_firms,       -- the underlying NACE/bucket total being allocated
#'       any_partial,            -- TRUE if concordance partial-match flagged
#'       any_suppressed,         -- TRUE if FATS flag == "C"
#'       bea_source_label,       -- BEA bucket name (BEA rows only)
#'       eu27_share_applied,     -- BEA worldwide->EU27 share (BEA rows only)
#'       notes
#'
#' Per-row firm-count assignment is correct under the same methodology as the
#' previous v2.2 pipeline:
#'   - FATS rows:       allocated_firms = nace_total * (n_naics_6dig / total_6dig_in_nace_cell)
#'   - BEA rows:        allocated_firms = bucket_eu27 * (covers_n_slots / bucket_total_slots)
#'   - Horizontal rows: allocated_firms = B-S_X_O_S94 total
#'
#' To roll up to NAICS-level: sum allocated_firms by (naics_code, naics_depth)
#'                            within source = "FATS"; BEA and horizontal already
#'                            have one row per (reg, naics).
#' To roll up to regulation-level with horizontal override:
#'   sectoral_only = sum(allocated_firms) where source != "FATS_horizontal_aggregate"
#'   us_firms_total = horizontal value if any horizontal row exists, else sectoral_only
#' (See compute_regulation_summary() at the bottom for a one-shot helper.)
#'
#' INPUTS expected in project_dir:
#'   usdia2022r-Part-III-A1-A4.xls
#'   ISIC_Rev_4_to_2022_NAICS.xlsx
#'   2-6_digit_2022_Codes.xlsx
#'   eu_reg_naics_crosswalk_v2_1_1.xlsx
#'
#' Run:    source("eu_reg_pipeline_long.R")
#' ----------------------------------------------------------------------------

# ============================================================================
# DATA SOURCES AND THE NAICS / ISIC / NACE CHAIN
# ============================================================================
#
# The regulation crosswalk scopes regulations to NAICS 2022 codes (US). Firm
# counts come from two complementary sources, neither of which is published
# at NAICS:
#
#   FATS (Eurostat fats_activ): publishes counts at NACE Rev. 2 cells.
#     US-UCI enterprises resident in EU27. No size threshold.
#   BEA USDIA (Tables III.A.3 / III.A.4): publishes counts at BEA's own
#     proprietary industry buckets that resemble NAICS 2-3 digit groupings
#     but are not actually NAICS. Majority-owned foreign affiliates with
#     assets/sales/income > $25M.
#
# The two sources reach NAICS via DIFFERENT mechanisms:
#
#   FATS path (algorithmic, via ISIC):
#       NAICS 6-digit ──(US Census ISIC↔NAICS file)──▶ ISIC 4-digit
#       ISIC 4-digit  ──(2-digit divisions identical by construction)──▶ NACE division
#
#     The second hop works without a crosswalk file because NACE Rev. 2
#     was constructed to share its first two digits with ISIC Rev. 4.
#     `substr(isic_code, 1, 2)` IS the NACE division. If a future NACE
#     revision diverges from ISIC at the 2-digit level this would silently
#     break -- check before upgrading classification vintages.
#
#   BEA path (hand-curated, no crosswalk):
#       gap_naics_map directly maps each gap-filled NAICS code to a BEA
#       Table III.A.4 bucket label. There is no algorithmic mapping; the
#       map encodes professional judgment about which bucket best
#       approximates each NAICS code (e.g., the deliberate three-way
#       finance split: banks vs. non-bank finance vs. insurance).
#
# COMPLEMENTARITY (NOT ADDITIVITY)
#
# For each NAICS code, exactly ONE of FATS or BEA is authoritative. They are
# alternative measurements of the same population, not separate populations.
# The script enforces this by joining FATS to the concordance, then DROPPING
# every (naics_code, naics_depth) row that appears in gap_naics_map and
# replacing it with the BEA gap-fill row. Never sum FATS and BEA for the
# same code.
#
# (For reference: at the horizontal-aggregate level FATS reports ~2.2x BEA,
# because FATS has no size threshold and uses ultimate controlling unit
# whereas BEA has a $25M threshold and uses direct US parent. The gap is
# mostly small firms below BEA's threshold.)
#
# TWO PARALLEL-BUT-DISTINCT WEIGHTING SYSTEMS
#
# Both sources use a `weight` column to allocate the source-cell total
# across the NAICS codes it covers, but the denominators are different
# concepts:
#
#   FATS (equal-split-by-6-digit-count, in build_concordance_long):
#       weight = n_naics_6dig / total_6dig_in_nace_cell
#       "Of the 6-digit NAICS classes mapping to this NACE cell, what
#        fraction are children of this NAICS parent?"
#       Assumes each 6-digit class contributes equally to its NACE parent.
#
#   BEA (slot-based, in build_bea_allocated):
#       weight = covers_n_slots / bucket_total_slots
#       "Of the NAICS codes the crosswalk treats as scoping this BEA
#        bucket, how many slots does this code cover?"
#       A 4-digit code is 1 slot. A 3-digit code with 4-digit children in
#        the same bucket is a roll-up that covers n_children slots but
#        does not add to the denominator.
#
# Both compose linearly: allocated_firms = source_cell_total × weight.
# The meaning of `weight` in the output table depends on `source`.
#
# WHY GAP_NAICS_MAP NAICS CODES GET DROPPED FROM FATS
#
# Two reasons (encoded in gap_reason):
#
#   Scope-excluded -- FATS doesn't publish the relevant NACE cell at all
#     or only at section-level granularity:
#       NAICS 111/112/113 + 4-digit children: no Section A (agriculture)
#       NAICS 2211: only NACE D as a whole, no division for electric
#       NAICS 486: only NACE H49 for all land transport, no pipeline split
#     Any FATS rows landing on these NAICS via the concordance are
#     partial-overlap noise, not real signal.
#
#   Suppressed -- FATS publishes the NACE cell but the EU27 aggregate is
#     flagged "C" because IE/LU concentrations would identify individual
#     firms. Affects K (finance), C26 (computer/electronic mfg), H50
#     (water transport), N77 (rental/leasing), Q87_Q88 (health/social).
#     BEA doesn't have the same disclosure problem because its buckets
#     are too coarse to identify individual firms.
#
# THE HORIZONTAL AGGREGATE (B-S_X_O_S94)
#
# A third path that uses neither the concordance nor gap_naics_map. FATS
# publishes one cell containing the total business economy, defined as
# "NACE sections B through S, excluding section O (public admin) and
# division 94 (membership organizations)" -- that's what B-S_X_O_S94
# encodes. ~27,000 US-UCI firms in EU27 for 2022. Used directly for
# regulations with horizontal scope (GDPR, AI Act limited-risk tier),
# where there's no NAICS-level allocation to do.
#
# WHY FATS PUBLISHES AT MULTIPLE GRANULARITIES
#
# FATS publishes whichever NACE cell -- single section letter, 2-digit
# division, or multi-division bundle (e.g., C10-C12) -- has enough firms
# to escape disclosure suppression while still being sectorally
# meaningful. Different sectors get different granularities depending on
# how concentrated US-UCI activity is. expand_fats_to_divisions() is the
# function that resolves whatever FATS published into a list of 2-digit
# divisions so the concordance can be applied uniformly.
#
# ============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(eurostat)
  library(readxl)
})

# ============================================================================
# CONFIG
# ============================================================================

project_dir <- "C:/Users/maxxj/OneDrive/Desktop/Misc/EU_regs"

pipeline_config <- list(
  project_dir          = project_dir,
  bea_a1_a4_file       = file.path(project_dir, "usdia2022r-Part-III-A1-A4.xls"),
  naics_isic_file      = file.path(project_dir, "ISIC_Rev_4_to_2022_NAICS.xlsx"),
  naics_struct_file    = file.path(project_dir, "2-6_digit_2022_Codes.xlsx"),
  crosswalk_file       = file.path(project_dir, "eu_reg_naics_crosswalk_v2_1_1.xlsx"),
  output_csv           = file.path(project_dir, "regulation_firm_counts_long.csv"),
  
  fats_indicator       = "ENT_NR",
  fats_c_ctrl          = "US",
  fats_geo             = "EU27_2020",
  horizontal_nace_code = "B-S_X_O_S94",
  anchor_year          = 2022L,
  
  bea_vintage_year     = 2022L
)

# ============================================================================
# STATIC LOOKUPS
# ============================================================================

# NACE Rev. 2: section letter -> 2-digit divisions it contains
nace_section_to_divisions <- list(
  A = sprintf("%02d", 1:3),  B = sprintf("%02d", 5:9),
  C = sprintf("%02d", 10:33), D = "35",
  E = sprintf("%02d", 36:39), F = sprintf("%02d", 41:43),
  G = sprintf("%02d", 45:47), H = sprintf("%02d", 49:53),
  I = sprintf("%02d", 55:56), J = sprintf("%02d", 58:63),
  K = sprintf("%02d", 64:66), L = "68",
  M = sprintf("%02d", 69:75), N = sprintf("%02d", 77:82),
  O = "84", P = "85",
  Q = sprintf("%02d", 86:88), R = sprintf("%02d", 90:93),
  S = sprintf("%02d", 94:96), T = sprintf("%02d", 97:98), U = "99"
)

# FATS-published bundle codes -> component 2-digit divisions.
# FATS publishes whichever cell granularity escapes disclosure suppression
# while remaining sectorally meaningful. A given sector may be published as
# a section letter (whole "C"), a single division ("C26"), or a multi-
# division bundle like these. expand_fats_to_divisions() resolves any of
# these into a uniform list of 2-digit divisions for the concordance.
fats_bundle_expansions <- list(
  "C10-C12" = c("10","11","12"), "C13-C15" = c("13","14","15"),
  "C16-C18" = c("16","17","18"), "C22_C23" = c("22","23"),
  "C24_C25" = c("24","25"),       "C29_C30" = c("29","30"),
  "C31_C32" = c("31","32"),       "H52_H53" = c("52","53"),
  "J59_J60" = c("59","60"),       "J62_J63" = c("62","63"),
  "M69-M71" = c("69","70","71"),  "M73-M75" = c("73","74","75"),
  "N78-N82" = c("78","79","80","81","82"),
  "Q87_Q88" = c("87","88"),       "S95_S96" = c("95","96")
)

# FATS aggregate codes (excluded from concordance):
#   B-S_X_O_S94: total business economy (NACE sections B-S, excl. O public
#                admin and division 94 membership organizations) -- used
#                directly as the horizontal aggregate for regulations with
#                horizontal scope. NOT distributed to NAICS via concordance.
#   G-S_X_O_S94: same but starting at section G (excludes industry).
fats_aggregate_codes <- c("B-S_X_O_S94", "G-S_X_O_S94")

# gap_naics_map: the hand-curated parallel mapping system that bypasses the
# NAICS/ISIC/NACE chain entirely. Each row pairs a NAICS code with a BEA
# Table III.A.4 bucket label that the script will use as that NAICS code's
# firm-count source instead of FATS. There is no algorithmic crosswalk
# between NAICS and BEA buckets -- this table IS the mapping.
#
# See the architectural block at top of file for why this exists. Two row
# categories, distinguished by gap_reason:
#   "fats_scope_excluded_*" -- FATS doesn't publish the relevant NACE cell
#       at usable granularity (Section A entirely absent; D, H49 only at
#       coarse aggregates).
#   "fats_suppressed_*"     -- FATS publishes the cell but the EU27 value
#       is flagged "C" (confidential) due to IE/LU concentration.
#
# Notable design choice: section K (finance) gets a deliberate three-way
# split into separate BEA buckets (depository / non-depository / insurance)
# rather than a single "Finance" bucket, because DORA, MiCA, and insurance-
# specific regs scope these very differently and a single bucket would
# erase those distinctions.
#
# Schema: naics_code (3- or 4-digit), crosswalk_depth, naics_description,
#         a4_industry_label (verbatim from BEA Table III.A.4), gap_reason.
# See v2.2 docstring in original pipeline for the full categorization rationale.
# NOTE on scoping: This map has been trimmed to the 14-regulation digital
# scope (GDPR, DMA, DSA, AI Act, NIS2, ePrivacy, CDSM, Data Act, Political
# Ads, EMFA, CSRD, CSDDD, DORA, CRA -- plus four more being added via
# crosswalk extension). Three full buckets were dropped because no target
# regulation scopes any of their NAICS codes: Agriculture/Forestry/Fishing
# (17 codes), Real estate and rental/leasing (6 codes), Health care and
# social assistance (9 codes). Within each KEPT bucket, ALL members are
# retained even if some are not invoked by target regs -- this preserves
# the slot denominator (bucket_total_slots), which represents the BEA
# bucket's full sectoral landscape, not the regulatory landscape. Trimming
# within a bucket would inflate per-NAICS allocations for the codes that
# remain.
#
gap_naics_map <- rbindlist(list(
  # Utilities (NACE D depth-limited): NAICS 2211 used by NIS2 (energy).
  data.table(
    naics_code = "2211",
    crosswalk_depth = "3-digit",
    naics_description = "Electric power generation, transmission, distribution",
    a4_industry_label = "Utilities",
    gap_reason = "fats_depth_limited_within_section_d"
  ),
  # Transportation (NACE H49 depth-limited + H50 suppressed): bucket spans
  # 486 (pipelines) and 483/4831/4832 (water transport). NIS2 scopes 486
  # and 483.
  data.table(
    naics_code = c("486","483","4831","4832"),
    crosswalk_depth = c("3-digit","3-digit","4-digit","4-digit"),
    naics_description = c("Pipeline transportation",
                          "Water transportation",
                          "Deep sea, coastal, and Great Lakes water transportation",
                          "Inland water transportation"),
    a4_industry_label = "Transportation and warehousing",
    gap_reason = c("fats_depth_limited_within_h49",
                   "fats_suppressed_h50",
                   "fats_suppressed_h50",
                   "fats_suppressed_h50")
  ),
  # Finance section K (suppressed): used by DORA, NIS2.
  # Three-way split into separate BEA buckets preserved.
  data.table(
    naics_code = c("522","523","524","525"),
    crosswalk_depth = "3-digit",
    naics_description = c("Credit intermediation and related activities",
                          "Securities, commodity contracts, financial investments",
                          "Insurance carriers and related activities",
                          "Funds, trusts, and other financial vehicles"),
    a4_industry_label = c("Finance, except depository institutions",
                          "Finance, except depository institutions",
                          "Insurance carriers and related activities",
                          "Finance, except depository institutions"),
    gap_reason = "fats_suppressed_section_k"
  ),
  data.table(
    naics_code = c("5221","5222","5223","5231","5232","5239","5241","5242"),
    crosswalk_depth = "4-digit",
    naics_description = c("Depository credit intermediation (banks)",
                          "Nondepository credit intermediation",
                          "Activities related to credit intermediation",
                          "Securities and commodity contracts intermediation",
                          "Securities and commodity exchanges",
                          "Other financial investment activities",
                          "Insurance carriers",
                          "Insurance agencies, brokerages, and related activities"),
    a4_industry_label = c("Depository credit intermediation (banking)",
                          "Finance, except depository institutions",
                          "Finance, except depository institutions",
                          "Finance, except depository institutions",
                          "Finance, except depository institutions",
                          "Finance, except depository institutions",
                          "Insurance carriers and related activities",
                          "Insurance carriers and related activities"),
    gap_reason = "fats_suppressed_section_k"
  ),
  # Computer/electronic mfg (NACE C26 suppressed): used by CRA for
  # connected products.
  data.table(
    naics_code = c("3341","3342","3343","3344","3345","3346"),
    crosswalk_depth = "4-digit",
    naics_description = c("Computer and peripheral equipment manufacturing",
                          "Communications equipment manufacturing",
                          "Audio and video equipment manufacturing",
                          "Semiconductor and other electronic component manufacturing",
                          "Navigational, measuring, electromedical, and control instruments manufacturing",
                          "Manufacturing and reproducing magnetic and optical media"),
    a4_industry_label = "Computers and electronic products",
    gap_reason = "fats_suppressed_c26"
  )
))

# ============================================================================
# HELPERS
# ============================================================================

find_col <- function(dt, patterns, fallback_msg, exclude = NULL) {
  cols <- names(dt)
  for (pat in patterns) {
    hit <- grep(pat, cols, ignore.case = TRUE, value = TRUE)
    if (!is.null(exclude)) for (e in exclude) hit <- hit[!grepl(e, hit, ignore.case = TRUE)]
    if (length(hit) >= 1) return(hit[1])
  }
  stop(sprintf("Could not find column matching any of: %s\n  Available: %s\n  %s",
               paste(patterns, collapse = ", "),
               paste(cols, collapse = ", "), fallback_msg))
}

# Expand a FATS NACE code (section letter, division like "C26", or bundle
# like "C10-C12") into its component 2-digit NACE divisions.
expand_fats_to_divisions <- function(fats_codes) {
  out <- list()
  for (fc in fats_codes) {
    if (fc %in% fats_aggregate_codes) next
    if (fc %in% names(fats_bundle_expansions)) {
      divs <- fats_bundle_expansions[[fc]]
    } else if (nchar(fc) == 1 && fc %in% names(nace_section_to_divisions)) {
      divs <- nace_section_to_divisions[[fc]]
    } else if (grepl("^[A-Z][0-9]{2}$", fc)) {
      divs <- substr(fc, 2, 3)
    } else {
      warning(sprintf("FATS code '%s' did not match any expansion rule", fc))
      next
    }
    out[[length(out) + 1]] <- data.table(fats_code = fc, nace_division = divs)
  }
  rbindlist(out)
}

# ============================================================================
# STEP 1: PULL FATS
# ============================================================================
# Returns a data.table at (year, nace_r2) granularity for US-UCI EU27
# enterprise counts, plus the full unique nace_r2 list (used to build the
# concordance) and the horizontal aggregate value.

pull_fats <- function(config) {
  message("\n=== STEP 1: FATS PULL ===")
  raw <- as.data.table(get_eurostat("fats_activ", time_format = "num",
                                    keepFlags = TRUE))
  time_col <- intersect(c("TIME_PERIOD","time"), names(raw))[1]
  setnames(raw, time_col, "year")
  raw[, year := as.integer(year)]
  
  filtered <- raw[indic_sbs == config$fats_indicator &
                    c_ctrl    == config$fats_c_ctrl &
                    geo       == config$fats_geo &
                    year      == config$anchor_year]
  
  eu27 <- filtered[, .(nace_code = as.character(nace_r2),
                       nace_total_firms = values,
                       flag = flags)]
  
  hz_row <- eu27[nace_code == config$horizontal_nace_code]
  if (nrow(hz_row) == 0) {
    stop(sprintf("Horizontal aggregate %s not found for %d",
                 config$horizontal_nace_code, config$anchor_year))
  }
  horizontal_total <- hz_row$nace_total_firms[1]
  horizontal_flag  <- hz_row$flag[1]
  message(sprintf("  Horizontal aggregate (%s): %s firms",
                  config$horizontal_nace_code,
                  format(horizontal_total, big.mark = ",")))
  
  nace_codes <- sort(unique(as.character(raw$nace_r2)))
  message(sprintf("  Unique NACE codes: %d | filtered rows for %d: %d",
                  length(nace_codes), config$anchor_year, nrow(eu27)))
  
  list(eu27 = eu27, nace_codes = nace_codes,
       horizontal_total = horizontal_total,
       horizontal_flag  = horizontal_flag)
}

# ============================================================================
# STEP 2: PARSE BEA + COMPUTE GAP-FILL EU27 ESTIMATES
# ============================================================================

parse_bea_a3 <- function(path) {
  raw <- as.data.table(read_excel(path, sheet = "Table III. A 3",
                                  col_names = FALSE, .name_repair = "minimal"))
  setnames(raw, paste0("V", seq_along(raw)))
  rows <- raw[!is.na(V1) & !is.na(V2),
              .(country_label = trimws(as.character(V1)),
                count_raw = as.character(V2))]
  rows <- rows[!grepl("^\\([0-9]+\\)$", count_raw)]
  rows[, count := suppressWarnings(as.integer(count_raw))]
  rows
}

parse_bea_a4 <- function(path) {
  raw <- as.data.table(read_excel(path, sheet = "Table III. A 4",
                                  col_names = FALSE, .name_repair = "minimal"))
  setnames(raw, paste0("V", seq_along(raw)))
  rows <- raw[!is.na(V1) & !is.na(V2),
              .(industry_label = trimws(as.character(V1)),
                count_raw = as.character(V2))]
  rows <- rows[!grepl("^\\([0-9]+\\)$", count_raw)]
  rows <- rows[!grepl("^NOTE", industry_label)]
  rows[, count := suppressWarnings(as.integer(count_raw))]
  rows
}

# Compute slot-weighted EU27 firm allocations per (naics_code, depth) row in
# gap_naics_map, joining the BEA bucket totals.
#
# Slot logic (preserves the v2.1 Fix 2(b) methodology):
#   - 4-digit code: 1 slot.
#   - 3-digit code with 4-digit children in the same bucket: covers n_children
#     slots (a roll-up), but does NOT add new slots to the bucket denominator.
#   - 3-digit code with no children in the same bucket: 1 slot.
#   weight = covers_n_slots / bucket_total_slots
#   allocated = bucket_eu27 * weight
build_bea_allocated <- function(config) {
  message("\n=== STEP 2: BEA GAP-FILL ===")
  a3 <- parse_bea_a3(config$bea_a1_a4_file)
  a4 <- parse_bea_a4(config$bea_a1_a4_file)
  
  eu27_row  <- a3[grepl("European Union \\(27\\)", country_label)]
  world_row <- a3[country_label == "All countries"]
  if (nrow(eu27_row) != 1 || nrow(world_row) != 1) {
    stop("Could not locate EU27 addendum or 'All countries' row in BEA Table A.3")
  }
  eu27_share <- eu27_row$count / world_row$count
  message(sprintf("  EU27/World MOFA share: %.3f (EU27=%s, World=%s)",
                  eu27_share, format(eu27_row$count, big.mark = ","),
                  format(world_row$count, big.mark = ",")))
  
  # Join gap_naics_map to A.4 buckets
  gap <- copy(gap_naics_map)
  gap[, bucket_world := NA_integer_]
  gap[, matched_label := NA_character_]
  for (i in seq_len(nrow(gap))) {
    needle <- gap$a4_industry_label[i]
    m <- a4[industry_label == needle]
    if (nrow(m) == 0) m <- a4[grepl(needle, industry_label, ignore.case = TRUE)]
    if (nrow(m) >= 1) {
      gap$bucket_world[i]  <- m$count[1]
      gap$matched_label[i] <- m$industry_label[1]
    }
  }
  gap[, bucket_eu27 := round(bucket_world * eu27_share)]
  gap[, eu27_share_applied := eu27_share]
  
  # Slot weighting -- the BEA half of the two parallel weighting systems
  # described in the architectural block at top of file. A BEA bucket is
  # divided into "4-digit-equivalent slots":
  #   - A 4-digit code is 1 slot.
  #   - A 3-digit code with 4-digit children in the same bucket is a
  #     roll-up; it covers n_children slots when invoked but does NOT
  #     add to the bucket denominator.
  #   - A 3-digit code with no children in the same bucket is 1 slot.
  # weight = covers_n_slots / bucket_total_slots
  # allocated_firms = bucket_eu27 * weight
  three <- gap[crosswalk_depth == "3-digit"]
  four  <- gap[crosswalk_depth == "4-digit"]
  
  if (nrow(three) > 0) {
    three[, n_children := vapply(seq_len(.N), function(i) {
      sum(four$matched_label == matched_label[i] &
            startsWith(four$naics_code, naics_code[i]))
    }, integer(1))]
    three[, covers_n_slots := fifelse(n_children == 0L, 1L, n_children)]
  }
  if (nrow(four) > 0) {
    four[, n_children := 1L]
    four[, covers_n_slots := 1L]
  }
  bea <- rbind(three, four, use.names = TRUE)
  
  # Bucket denominator = (# 4-digit rows) + (# 3-digit rows w/ no children)
  bea[, bucket_total_slots := {
    n4   <- sum(crosswalk_depth == "4-digit")
    n3nc <- sum(crosswalk_depth == "3-digit" & n_children == 0L)
    n4 + n3nc
  }, by = matched_label]
  
  bea[, weight := covers_n_slots / bucket_total_slots]
  bea[, allocated_firms := bucket_eu27 * weight]
  
  message(sprintf("  Gap-fill rows: %d across %d BEA buckets",
                  nrow(bea), uniqueN(bea$matched_label)))
  
  bea[, .(naics_code, naics_depth = crosswalk_depth,
          naics_title = naics_description,
          nace_code = paste0("BEA:", matched_label),
          source = "BEA_gap_fill",
          weight, nace_total_firms = bucket_eu27,
          allocated_firms,
          bea_source_label = matched_label,
          eu27_share_applied,
          gap_reason)]
}

# ============================================================================
# STEP 3: NAICS-NACE CONCORDANCE (long, no rollup)
# ============================================================================
# Output: one row per (nace_code, naics_code, naics_depth) carrying the
# weight = n_naics_6dig / total_6dig_in_nace_cell, plus partial-match flag.

load_naics_isic <- function(path) {
  raw <- as.data.table(read_xlsx(path))
  part_isic  <- grep("Part.*ISIC",  names(raw), value = TRUE, ignore.case = TRUE)[1]
  part_naics <- grep("Part.*NAICS", names(raw), value = TRUE, ignore.case = TRUE)[1]
  is_us_census <- !is.na(part_isic) && !is.na(part_naics)
  
  naics_col <- find_col(raw,
                        c("NAICS.*[Cc]ode","^Code.*NAICS","NAICS\\s*US","NAICS US","NAICS 2022"),
                        "Expected a NAICS code column.",
                        exclude = c("Part","Title","note"))
  isic_col  <- find_col(raw,
                        c("ISIC.*[Cc]ode","^Code.*ISIC","^ISIC\\s*Rev","^ISIC\\s*4"),
                        "Expected an ISIC code column.",
                        exclude = c("Part","Title","note"))
  
  naics_chr <- if (is.numeric(raw[[naics_col]])) {
    as.character(as.integer(raw[[naics_col]]))
  } else as.character(raw[[naics_col]])
  
  out <- data.table(naics_raw = naics_chr,
                    isic_raw  = as.character(raw[[isic_col]]))
  if (is_us_census) {
    out[, naics_partial := !is.na(raw[[part_naics]]) &
          trimws(as.character(raw[[part_naics]])) == "*"]
    out[, isic_partial  := !is.na(raw[[part_isic]]) &
          trimws(as.character(raw[[part_isic]])) == "*"]
  } else {
    out[, c("naics_partial","isic_partial") := list(FALSE, FALSE)]
  }
  
  out <- out[grepl("^[0-9]{6}$", gsub("\\*","", trimws(naics_raw)))]
  out <- out[!is.na(naics_raw) & !is.na(isic_raw) &
               naics_raw != "" & isic_raw != "" &
               naics_raw != "0" & isic_raw != "0"]
  out[, naics_2022 := trimws(gsub("\\*", "", naics_raw))]
  out[, isic4      := trimws(gsub("\\*", "", isic_raw))]
  out[, isic_numeric := sprintf("%04d", as.integer(gsub("[^0-9]","", isic4)))]
  # ISIC -> NACE: NACE Rev. 2 was constructed to share its first two digits
  # with ISIC Rev. 4, so substr(., 1, 2) IS the NACE division. No third
  # crosswalk file needed. See the architectural block at top of file.
  out[, nace_division := substr(isic_numeric, 1, 2)]
  out[, is_partial := naics_partial | isic_partial]
  out[, .(naics_2022, nace_division, is_partial)]
}

load_naics_structure <- function(path) {
  raw <- as.data.table(read_xlsx(path))
  code_col  <- find_col(raw, c("NAICS.*[Cc]ode","^Code.*NAICS","NAICS\\s*US.*Code","^Code"),
                        "Expected a NAICS code column.",
                        exclude = c("Part","Title"))
  title_col <- find_col(raw, c("NAICS.*Title","Title","Description","[Cc]lass title"),
                        "Expected a NAICS title column.")
  
  codes <- raw[[code_col]]
  if (is.numeric(codes)) codes <- as.character(as.integer(codes))
  codes <- trimws(as.character(codes))
  titles <- as.character(raw[[title_col]])
  
  valid <- grepl("^[0-9]{2,6}$", codes) & !is.na(titles) & titles != ""
  all <- unique(data.table(code = codes[valid], title = titles[valid]), by = "code")
  all[, digits := nchar(code)]
  
  list(
    six = all[digits == 6L, .(naics_2022 = code,
                              naics_3digit = substr(code, 1, 3),
                              naics_4digit = substr(code, 1, 4))],
    titles_3 = all[digits == 3L, .(naics_code = code, naics_title = title)],
    titles_4 = all[digits == 4L, .(naics_code = code, naics_title = title)]
  )
}

# Build the (nace_code, naics_code, naics_depth) long table with weights.
# Row meaning: this NAICS gets `weight` of whatever firm count is published
# at this NACE cell, where weight = (# of 6-digit NAICS classes mapping to
# both this NACE division and this NAICS parent) / (total # of 6-digit
# classes mapping to this NACE cell).
build_concordance_long <- function(config, fats_nace_codes) {
  message("\n=== STEP 3: CONCORDANCE ===")
  ni <- load_naics_isic(config$naics_isic_file)
  ns <- load_naics_structure(config$naics_struct_file)
  
  # 6-digit -> NACE division, with partial-match flag and parent codes
  six <- merge(ni, ns$six, by = "naics_2022", all.x = TRUE)
  six <- six[!is.na(naics_3digit)]
  message(sprintf("  6-digit NAICS pairs in concordance: %d", nrow(six)))
  
  # FATS NACE codes -> divisions
  fats_div <- expand_fats_to_divisions(fats_nace_codes)
  message(sprintf("  FATS NACE cells mappable: %d", uniqueN(fats_div$fats_code)))
  
  # For each FATS NACE cell x division, attach all 6-digit NAICS classes,
  # then count classes per (FATS cell, NAICS parent) at both 3- and 4-digit.
  rolled <- merge(fats_div, six, by = "nace_division", allow.cartesian = TRUE)
  
  agg_3 <- rolled[, .(n_naics_6dig = uniqueN(naics_2022),
                      any_partial  = any(is_partial)),
                  by = .(fats_code, naics_3digit)]
  agg_3[, naics_depth := "3-digit"]
  setnames(agg_3, "naics_3digit", "naics_code")
  
  agg_4 <- rolled[, .(n_naics_6dig = uniqueN(naics_2022),
                      any_partial  = any(is_partial)),
                  by = .(fats_code, naics_4digit)]
  agg_4[, naics_depth := "4-digit"]
  setnames(agg_4, "naics_4digit", "naics_code")
  
  long <- rbind(agg_3, agg_4, use.names = TRUE)
  setnames(long, "fats_code", "nace_code")
  
  # FATS weight: equal-split-by-6-digit-count. For each NACE cell, count
  # how many 6-digit NAICS classes roll up to each NAICS parent (3- or
  # 4-digit), then weight = that count / total 6-digit classes in the
  # NACE cell. Assumes each 6-digit class contributes equally to its
  # NACE parent -- wrong in detail but acceptable as a prior. This is
  # the FATS half of the two parallel weighting systems described in
  # the architectural block at top of file.
  long[, total_6dig_in_nace_cell := sum(n_naics_6dig),
       by = .(nace_code, naics_depth)]
  long[, weight := n_naics_6dig / total_6dig_in_nace_cell]
  
  # Attach NAICS titles
  titles_all <- rbind(
    ns$titles_3[, .(naics_code, naics_depth = "3-digit", naics_title)],
    ns$titles_4[, .(naics_code, naics_depth = "4-digit", naics_title)]
  )
  long <- merge(long, titles_all, by = c("naics_code","naics_depth"), all.x = TRUE)
  
  long[, .(naics_code, naics_depth, naics_title, nace_code,
           weight, n_naics_6dig, any_partial)]
}

# ============================================================================
# STEP 4: BUILD UNIFIED LONG TABLE OF (naics_code, depth, nace_code) FIRM ROWS
# ============================================================================
# Three components, all with the same column schema:
#   FATS rows:       per (nace_code, naics_code, depth), allocated = nace_total * weight
#   BEA gap-fill:    per (bucket, naics_code, depth), allocated = bucket_eu27 * weight
#   Horizontal:      one row, naics_code = "ALL", nace_code = "B-S_X_O_S94"

build_firm_rows_long <- function(config, fats, bea_long, concord_long) {
  message("\n=== STEP 4: BUILD UNIFIED FIRM-ROWS TABLE ===")
  
  # --- FATS rows ---
  # Join the concordance to FATS firm counts on the NACE cell.
  fats_join <- merge(concord_long, fats$eu27,
                     by = "nace_code", all.x = TRUE)
  fats_rows <- fats_join[!is.na(nace_total_firms), .(
    naics_code, naics_depth, naics_title,
    nace_code,
    source = "FATS",
    weight,
    nace_total_firms,
    allocated_firms = nace_total_firms * weight,
    any_partial,
    any_suppressed = !is.na(flag) & flag == "C",
    bea_source_label = NA_character_,
    eu27_share_applied = NA_real_,
    gap_reason = NA_character_
  )]
  
  # --- DROP FATS rows for NAICS in gap_naics_map (BEA is authoritative there) ---
  gap_keys <- gap_naics_map[, .(naics_code, naics_depth = crosswalk_depth)]
  before <- nrow(fats_rows)
  fats_rows <- fats_rows[!gap_keys, on = .(naics_code, naics_depth)]
  message(sprintf("  FATS rows after dropping BEA-gap NAICS: %d (dropped %d)",
                  nrow(fats_rows), before - nrow(fats_rows)))
  
  # --- BEA rows ---
  # bea_long already has the right schema; just add the missing flags.
  bea_rows <- copy(bea_long)
  bea_rows[, any_partial := FALSE]
  bea_rows[, any_suppressed := FALSE]
  
  # --- Horizontal aggregate ---
  horiz_row <- data.table(
    naics_code = "ALL",
    naics_depth = "ALL",
    naics_title = "Horizontal -- total business economy (NACE B-S_X_O_S94)",
    nace_code = config$horizontal_nace_code,
    source = "FATS_horizontal_aggregate",
    weight = 1.0,
    nace_total_firms = as.numeric(fats$horizontal_total),
    allocated_firms  = as.numeric(fats$horizontal_total),
    any_partial = FALSE,
    any_suppressed = !is.na(fats$horizontal_flag) & fats$horizontal_flag == "C",
    bea_source_label = NA_character_,
    eu27_share_applied = NA_real_,
    gap_reason = NA_character_
  )
  
  out <- rbind(fats_rows, bea_rows, horiz_row, use.names = TRUE, fill = TRUE)
  message(sprintf("  Combined firm-rows table: %d rows (FATS=%d, BEA=%d, horizontal=%d)",
                  nrow(out), nrow(fats_rows), nrow(bea_rows), 1L))
  out
}

# ============================================================================
# STEP 5: RESOLVE CROSSWALK + JOIN
# ============================================================================
# Output: one row per (regulation_name, naics_code, naics_depth, nace_code)

resolve_crosswalk <- function(crosswalk) {
  cw <- copy(crosswalk)
  setnames(cw, names(cw), tolower(names(cw)))
  required <- c("regulation_name","naics_3digit","naics_4digit","assigned_depth")
  miss <- setdiff(required, names(cw))
  if (length(miss) > 0) stop("Crosswalk missing columns: ", paste(miss, collapse = ", "))
  
  if (!"canonical_regulation_name" %in% names(cw)) {
    warning("Crosswalk has no 'canonical_regulation_name'; using 'regulation_name'.")
    cw[, canonical_regulation_name := regulation_name]
  }
  cw[is.na(canonical_regulation_name),
     canonical_regulation_name := regulation_name]
  
  cw[, assigned_depth_int := suppressWarnings(as.integer(assigned_depth))]
  cw[, naics_4digit_chr := fifelse(
    is.na(naics_4digit), NA_character_,
    as.character(as.integer(suppressWarnings(as.numeric(naics_4digit))))
  )]
  cw[, naics_code := fifelse(
    !is.na(assigned_depth_int) & assigned_depth_int == 4L,
    naics_4digit_chr, as.character(naics_3digit)
  )]
  cw[, naics_depth := fcase(
    toupper(naics_code) == "ALL", "ALL",
    !is.na(assigned_depth_int) & assigned_depth_int == 4L, "4-digit",
    default = "3-digit"
  )]
  
  cw_long <- cw[!is.na(naics_code) & naics_code != "" &
                  !is.na(canonical_regulation_name),
                .(regulation_name = canonical_regulation_name,
                  regulation_name_raw = as.character(regulation_name),
                  naics_code, naics_depth)]
  
  # Collapse raw names per canonical (reg, code, depth) so audit info survives
  # without inflating row counts.
  cw_long[, .(regulation_name_raw =
                paste(sort(unique(regulation_name_raw)), collapse = " | ")),
          by = .(regulation_name, naics_code, naics_depth)]
}

# Join the crosswalk to the firm-rows table. This explodes per crosswalk row
# to one row per contributing NACE cell (FATS) or BEA bucket sentinel.
join_crosswalk_to_firm_rows <- function(cw_long, firm_rows_long) {
  message("\n=== STEP 5: REGULATION JOIN ===")
  joined <- merge(cw_long, firm_rows_long,
                  by = c("naics_code","naics_depth"),
                  all.x = TRUE, allow.cartesian = TRUE)
  
  # Build per-row notes from gap_reason + suppression flags. Branches for
  # dropped gap_reasons (fats_scope_excluded_section_a, fats_suppressed_n77,
  # fats_suppressed_q87_q88) removed along with their gap_naics_map blocks.
  joined[, notes := ""]
  joined[!is.na(gap_reason),
         notes := fcase(
           gap_reason == "fats_depth_limited_within_section_d",
           "FATS publishes NACE D at section level only; BEA Utilities bucket used (overstates electricity-only).",
           gap_reason == "fats_depth_limited_within_h49",
           "FATS H49 covers all land transport; BEA Transport bucket used (overstates pipelines only).",
           gap_reason == "fats_suppressed_section_k",
           "FATS EU27 K aggregate suppressed (IE/LU concentration); BEA gap-fill applied.",
           gap_reason == "fats_suppressed_c26",
           "FATS EU27 C26 suppressed; BEA Computers/electronic-products bucket used.",
           gap_reason == "fats_suppressed_h50",
           "FATS EU27 H50 suppressed; BEA Transport bucket used (overstates water transport).",
           default = ""
         )]
  joined[any_suppressed == TRUE & source == "FATS",
         notes := fifelse(
           notes == "",
           "FATS cell flagged 'C' (suppressed); EU27 aggregate undercounts IE/LU.",
           paste(notes,
                 "FATS cell flagged 'C' (suppressed); EU27 aggregate undercounts IE/LU.")
         )]
  
  # Diagnose unmatched
  unmatched <- joined[is.na(allocated_firms),
                      .(regulation_name, naics_code, naics_depth)]
  if (nrow(unmatched) > 0) {
    summ <- unmatched[, .(n = .N,
                          regs = paste(head(unique(regulation_name), 5), collapse = "; ")),
                      by = .(naics_code, naics_depth)]
    message(sprintf("  Crosswalk rows with no firm-count match: %d (across %d (NAICS,depth) keys)",
                    nrow(unmatched), nrow(summ)))
    for (i in seq_len(min(nrow(summ), 8))) {
      message(sprintf("    %s (%s) - %d rows: %s",
                      summ$naics_code[i], summ$naics_depth[i],
                      summ$n[i], summ$regs[i]))
    }
  }
  
  out_cols <- c("regulation_name","regulation_name_raw",
                "naics_code","naics_depth","naics_title",
                "nace_code","source","allocated_firms","weight",
                "nace_total_firms","any_partial","any_suppressed",
                "bea_source_label","eu27_share_applied","notes")
  joined <- joined[, ..out_cols]
  setorder(joined, regulation_name, naics_depth, naics_code, source, nace_code)
  joined[]
}

# ============================================================================
# OPTIONAL: ROLL UP TO REGULATION-LEVEL SUMMARY
# ============================================================================
# Provided as a helper if you want one-row-per-regulation totals with the
# horizontal-override rule applied. NOT written to disk by default.

compute_regulation_summary <- function(long_df) {
  long_df[, {
    has_horiz <- any(source == "FATS_horizontal_aggregate", na.rm = TRUE)
    horiz_val <- if (has_horiz) {
      sum(allocated_firms[source == "FATS_horizontal_aggregate"], na.rm = TRUE)
    } else NA_real_
    sectoral_only <- sum(allocated_firms[source != "FATS_horizontal_aggregate"],
                         na.rm = TRUE)
    .(
      scope_type = fcase(
        has_horiz & sectoral_only > 0, "mixed",
        has_horiz, "horizontal",
        sectoral_only > 0, "sectoral",
        default = "unmatched"
      ),
      us_firms_total          = if (has_horiz) horiz_val else sectoral_only,
      us_firms_sectoral_only  = sectoral_only,
      us_firms_fats           = sum(allocated_firms[source == "FATS"], na.rm = TRUE),
      us_firms_bea_gap        = sum(allocated_firms[source == "BEA_gap_fill"], na.rm = TRUE),
      n_rows                  = .N,
      n_naics                 = uniqueN(paste(naics_code, naics_depth)),
      n_unmatched             = sum(is.na(source)),
      any_partial             = any(any_partial, na.rm = TRUE),
      any_suppressed          = any(any_suppressed, na.rm = TRUE)
    )
  }, by = regulation_name]
}

# ============================================================================
# MAIN
# ============================================================================

main <- function(config = pipeline_config) {
  message("===================================================")
  message(sprintf("PROJECT DIR: %s", config$project_dir))
  message("===================================================")
  
  fats         <- pull_fats(config)
  bea_long     <- build_bea_allocated(config)
  concord_long <- build_concordance_long(config, fats$nace_codes)
  firm_rows    <- build_firm_rows_long(config, fats, bea_long, concord_long)
  
  message("\nLoading crosswalk...")
  crosswalk <- as.data.table(read_xlsx(config$crosswalk_file, sheet = "Crosswalk"))
  cw_long   <- resolve_crosswalk(crosswalk)
  message(sprintf("  %d crosswalk rows resolved across %d canonical regulations",
                  nrow(cw_long), uniqueN(cw_long$regulation_name)))
  
  long <- join_crosswalk_to_firm_rows(cw_long, firm_rows)
  
  fwrite(long, config$output_csv)
  message(sprintf("\nWrote %s (%d rows)", config$output_csv, nrow(long)))
  
  message("\n===================================================")
  message("PIPELINE COMPLETE")
  message("===================================================")
  
  invisible(long)
}

# ============================================================================
# RUN
# ============================================================================

result <- main()

cat("\n--- Row counts by source ---\n")
print(result[, .N, by = source])

cat("\n--- Sample: 8 regulations x rows ---\n")
print(result[, .(n_rows = .N,
                 sectoral_firms = sum(allocated_firms[source != "FATS_horizontal_aggregate"],
                                      na.rm = TRUE),
                 has_horizontal = any(source == "FATS_horizontal_aggregate")),
             by = regulation_name][1:8])

cat("\n--- Optional regulation-level rollup ---\n")
print(compute_regulation_summary(result))
