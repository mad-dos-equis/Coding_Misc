# ============================================================================
# Example: how to call the company-name fuzzy matching pipeline on your data
# ----------------------------------------------------------------------------
# Replace the data-loading section with your own data-frame loads.
# Everything else can stay the same.
# ============================================================================

library(data.table)
library(readxl)

# 1. Source the library (adjust path as needed)
source("company_name_fuzzy_match.R")


# 2. Load YOUR two data frames here.
#    Replace these lines with however you read your real data.

left_df  <- fread("path/to/small_frame.csv")
right_df <- as.data.table(read_xlsx("path/to/large_frame.xlsx"))


# 3. Run the pipeline.

result <- run_fuzzy_match_pipeline(
  left_df  = left_df,
  right_df = right_df,
  left_name_col  = "company",       # <-- name column in left_df
  right_name_col = "company",       # <-- name column in right_df

  # Optional: existing ID columns. If NULL, row index is used.
  left_id_col    = NULL,
  right_id_col   = NULL,

  # Subsample left for the parameter-selection grid search. Doesn't affect
  # the final tiered match (which always runs on the full left side).
  # 500-2000 is usually plenty.
  grid_sample_size = 1000,

  # Tier strictness multipliers applied to the winning max_dist.
  # Default is strict (0.5x) and moderate (1.0x) only — no loose tier.
  # The loose tier is omitted by default because in asymmetric-subset
  # matching (e.g. only some left rows have a true right-side match),
  # loose thresholds manufacture false positives by matching unmatchable
  # left rows to whichever right row happens to be nearest.
  tier_multipliers = c(strict = 0.5, moderate = 1.0),

  # Metric for selecting the winning (method, max_dist) configuration.
  # "silver_f1" (default) balances precision and recall.
  # "silver_recall" is preferable when the silver set is small or when
  # left and right have asymmetric coverage (e.g., right is a strict subset
  # of the universe), because silver-precision becomes noisy on small
  # silver subsets.
  selection_metric = "silver_recall",

  verbose = TRUE
)


# 4. Inspect the results.

# Matches with tier, method, distance, and gap diagnostics.
# Important columns added to the matched output:
#   match_dist   = distance of the chosen match
#   runner_up    = distance of the second-best candidate (NA if only one)
#   gap          = runner_up - match_dist  (larger = more decisive)
#   n_candidates = how many candidates were within max_dist
#   tier         = "strict", "moderate", or "unmatched"
print(head(result$matches, 20))

# Configuration the pipeline picked
print(result$config)

# Full grid evaluation (sortable by silver_f1 or silver_recall)
print(result$grid)

# Silver-standard pairs the pipeline bootstrapped
print(head(result$silver))

# Audit sample, stratified by distance bucket AND gap status:
#   gap_status = "low_gap"  : best match has near-tied competitors (suspect)
#                "solo"     : only one candidate (decisive)
#                "decisive" : best match is clearly closer than runner-up
# Audit sample is biased toward low_gap matches in higher buckets,
# since that's where false positives concentrate.
print(result$audit)


# 5. Get a tidy enriched copy of the original left data frame.
# Every original left row is preserved, augmented with match metadata.
# Rows that weren't in result$matches (dropped for empty normalized keys)
# are recovered here and flagged with match_tier = "dropped_empty_key".

enriched_left <- enrich_left(
  result         = result,
  left_df        = left_df,
  left_id_col    = NULL,             # same value you passed to the pipeline
  right_name_col = "company"         # the right-side name column (for match_right_name)
)

# Columns added by enrich_left():
#   matched          - logical, TRUE if a match was found
#   match_tier       - "strict", "moderate", "unmatched", "dropped_empty_key"
#   match_confidence - "high", "medium", "low", or NA (derived from tier + gap)
#   match_method     - which stringdist method
#   match_dist       - distance of the chosen match
#   match_gap        - gap to runner-up (NA if solo)
#   match_n_cands    - candidates within threshold
#   match_right_id   - matched row's right_id (NA if unmatched)
#   match_right_name - matched company name from right (NA if unmatched)

print(head(enriched_left, 20))

cat("\nMatch summary:\n")
print(enriched_left[, .N, by = .(match_tier, match_confidence)])


# 6. Inspecting suspect matches in production output.
# Even after picking a tier threshold, you can flag low-confidence matches
# using the gap column. These are the matches most likely to be wrong:

suspect <- enriched_left[matched & !is.na(match_gap) & match_gap < 0.02]
cat("\nMatches with very small gap to runner-up:", nrow(suspect), "\n")
cat("(These are good candidates for manual review.)\n")
print(head(suspect, 20))


# 7. Optional: save outputs
# fwrite(enriched_left,   "enriched_left.csv")
# fwrite(result$audit,    "audit_sample.csv")
# fwrite(result$grid,     "grid_evaluation.csv")
