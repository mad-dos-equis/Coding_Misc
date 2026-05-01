# ============================================================================
# Example: how to call the company-name fuzzy matching pipeline on your data
# ----------------------------------------------------------------------------
# Replace the data-loading section with your own data-frame loads.
# Everything else can stay the same.
# ============================================================================

# 1. Source the library (adjust path as needed)
source("company_name_fuzzy_match.R")


# 2. Load YOUR two data frames here.
#    Replace these lines with however you read your real data.
#
# Examples:
#   left_df  <- fread("path/to/small_frame.csv")
#   right_df <- fread("path/to/large_frame.csv")
#   left_df  <- readRDS("left.rds")
#   right_df <- readRDS("right.rds")

left_df  <- NULL  # <-- your smaller data frame
right_df <- NULL  # <-- your larger data frame


# 3. Run the pipeline. The required arguments are the column names holding
#    the company names on each side. Everything else has sensible defaults.

result <- run_fuzzy_match_pipeline(
  left_df  = left_df,
  right_df = right_df,
  left_name_col  = "company",       # <-- name column in left_df
  right_name_col = "company",       # <-- name column in right_df

  # Optional: pass existing ID columns if you have them; otherwise row index is used
  left_id_col    = NULL,
  right_id_col   = NULL,

  # Optional: subsample the left side for the grid-search step if it's huge.
  # The final tiered match still runs on the FULL left side; only the
  # parameter selection uses the sample. 500-2000 is usually plenty.
  grid_sample_size = 1000,

  # Optional: tier strictness multipliers applied to the winning max_dist.
  # Default produces strict (0.5x), moderate (1.0x), and loose (1.5x) tiers.
  tier_multipliers = c(strict = 0.5, moderate = 1.0, loose = 1.5),

  verbose = TRUE
)


# 4. Inspect the results.

# The matched output, with tier and distance for each match
print(head(result$matches, 20))

# Configuration the pipeline picked
print(result$config)

# Full grid evaluation (sortable by silver_f1 to see how methods compared)
print(result$grid)

# Silver-standard pairs the pipeline bootstrapped
print(head(result$silver))

# Audit sample for manual review — eyeball this, find the bucket where
# matches start being wrong, and tighten tier_multipliers accordingly.
print(result$audit)


# 5. Optional: save outputs
# fwrite(result$matches, "fuzzy_matches.csv")
# fwrite(result$audit,   "audit_sample.csv")
# fwrite(result$grid,    "grid_evaluation.csv")
