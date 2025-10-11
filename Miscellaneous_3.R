# ---- Helper: normalize month to Jan–Dec factor ----
to_month_factor <- function(m) {
  # If numeric 1:12, convert; if already "Jan" etc., map safely
  if (is.numeric(m)) {
    factor(month.abb[m], levels = month.abb)
  } else {
    # Try to match regardless of case; fall back to original if unmatched
    idx <- match(str_to_title(as.character(m)), month.abb)
    factor(month.abb[idx], levels = month.abb)
  }
}
