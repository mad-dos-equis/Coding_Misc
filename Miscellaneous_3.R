# ---- Packages ----
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(purrr)

# ------------------------------------------------------------------------------
# INPUT: a data frame named df with columns: item, year, month, value
# - month can be 1..12 OR text ("Jan", "January", etc.)
# Example scaffold (delete when using your real df):
# df <- tibble::tibble(
#   item  = rep(c("Widget A", "Gizmo/Beta (Rev.2)"), each = 36),
#   year  = rep(rep(2023:2025, each = 12), 2),
#   month = rep(1:12, 6),
#   value = runif(72, 1e4, 2e6)
# )
# ------------------------------------------------------------------------------

# ---- Output directory for saved plots ----
out_dir <- "outputs/item_charts"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Helper: normalize month to Jan–Dec factor ----
to_month_factor <- function(m) {
  # If numeric 1:12, convert; if text, try to map "Jan"/"January" (case-insensitive) to month.abb
  if (is.numeric(m)) {
    return(factor(month.abb[m], levels = month.abb))
  }
  m_chr <- as.character(m)
  # Try to coerce full names to abbreviations first using base date parsing
  # Fallback: title-case and match to month.abb directly
  try_full <- suppressWarnings(match(tolower(m_chr), tolower(month.name)))
  abb_from_full <- ifelse(!is.na(try_full), month.abb[try_full], NA_character_)
  # Where that failed, try direct match to abbreviations
  try_abb <- match(str_to_title(m_chr), month.abb)
  resolved <- ifelse(!is.na(abb_from_full), abb_from_full, month.abb[try_abb])
  factor(resolved, levels = month.abb)
}

# ---- Helper: robust filename sanitizer (fixes mismatched-paren regex issue) ----
sanitize_filename <- function(x) {
  # Optional: transliterate Unicode to ASCII if stringi is available
  if (requireNamespace("stringi", quietly = TRUE)) {
    x <- stringi::stri_trans_general(x, "Any-Latin; NFD; [:Nonspacing Mark:] Remove; NFC")
  }
  x |>
    tolower() |>
    str_replace_all("[^a-z0-9]+", "-") |>   # collapse non-alnum to hyphens
    str_replace_all("^[-]+|[-]+$", "")      # trim leading/trailing hyphens (no grouping parens)
}

# ---- Plot function for a single item ----
plot_item <- function(df_item) {
  item_name <- unique(df_item$item)[1]
  yr_min <- suppressWarnings(min(df_item$year, na.rm = TRUE))
  yr_max <- suppressWarnings(max(df_item$year, na.rm = TRUE))
  subtitle_txt <- if (is.finite(yr_min) && is.finite(yr_max) && yr_min != yr_max) {
    paste0(yr_min, "–", yr_max, " Comparison")
  } else {
    as.character(yr_min)
  }

  df_plot <- df_item %>%
    mutate(
      month_fac = to_month_factor(month),
      year = factor(year)
    ) %>%
    arrange(year, month_fac)

  ggplot(df_plot, aes(x = month_fac, y = value, color = year, group = year)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2) +
    scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
    labs(
      title = paste0("Monthly Values by Year — ", item_name),
      subtitle = subtitle_txt,
      x = NULL,
      y = NULL,
      color = "Year"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),                    # only major gridlines
      axis.text.x = element_text(size = 13),
      axis.text.y = element_text(size = 13),
      legend.title = element_text(size = 13),
      legend.text  = element_text(size = 12),
      plot.title   = element_text(size = 18, face = "bold", hjust = 0,
                                  margin = margin(l = -10)),
      plot.subtitle = element_text(size = 14, hjust = 0,
                                   margin = margin(l = -10)),
      plot.title.position = "plot",
      plot.margin = margin(t = 10, r = 12, b = 10, l = 0)
    )
}

# ---- Generate & save one chart per item (robust to errors inside map/walk) ----
df %>%
  group_split(item, .keep = TRUE) %>%
  walk(function(dfi) {
    item_name <- unique(dfi$item)[1]
    safe_name <- sanitize_filename(item_name)
    file_path <- file.path(out_dir, paste0("monthly-values-", safe_name, ".png"))

    # Build plot and save; report any failures with the item name
    tryCatch(
      {
        p <- plot_item(dfi)
        ggsave(filename = file_path, plot = p, width = 8.5, height = 5, dpi = 300)
        message("Saved: ", file_path)
      },
      error = function(e) {
        message("Failed on item: ", item_name, " | Error: ", conditionMessage(e))
      }
    )
  })
