library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(purrr)

# ---- CONFIG: set your real column names if you know them ----
item_col  <- "item"         # e.g., "product_name"
year_col  <- "year"
month_col <- "month"
value_col <- "value"
desc_col  <- "description"  # <- NEW (set to NULL if you have no description)

# ---- Output folder ----
out_dir <- "outputs/item_charts"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Helpers ----
to_month_factor <- function(m) {
  if (is.numeric(m)) return(factor(month.abb[m], levels = month.abb))
  m_chr <- as.character(m)
  try_full <- suppressWarnings(match(tolower(m_chr), tolower(month.name)))
  abb_from_full <- ifelse(!is.na(try_full), month.abb[try_full], NA_character_)
  try_abb <- match(stringr::str_to_title(m_chr), month.abb)
  resolved <- ifelse(!is.na(abb_from_full), abb_from_full, month.abb[try_abb])
  factor(resolved, levels = month.abb)
}

sanitize_filename <- function(x, fallback = "item") {
  if (requireNamespace("stringi", quietly = TRUE)) {
    x <- stringi::stri_trans_general(x, "Any-Latin; NFD; [:Nonspacing Mark:] Remove; NFC")
  }
  x <- x %>%
    tolower() %>%
    str_replace_all("[^a-z0-9]+", "-") %>%
    str_replace_all("^[-]+|[-]+$", "")
  if (is.na(x) || !nzchar(x)) fallback else x
}

# ---- Main plot function ----
# set use_cumulative = TRUE to plot the cumulative series
plot_item <- function(df_item, use_cumulative = TRUE) {
  item_name <- unique(df_item[[item_col]])[1]

  # Pull a single, non-missing description if available
  desc <- if (!is.null(desc_col) && desc_col %in% names(df_item)) {
    d <- unique(na.omit(df_item[[desc_col]]))
    if (length(d)) as.character(d[1]) else NA_character_
  } else NA_character_

  # Year range for subtitle
  yr_min <- suppressWarnings(min(df_item[[year_col]], na.rm = TRUE))
  yr_max <- suppressWarnings(max(df_item[[year_col]], na.rm = TRUE))
  yr_span <- if (is.finite(yr_min) && is.finite(yr_max) && yr_min != yr_max) {
    paste0(yr_min, "–", yr_max, " Comparison")
  } else as.character(yr_min)

  # Build subtitle: "Description — 2023–2025 Comparison"
  # (truncate very long descriptions)
  desc_show <- if (!is.na(desc)) str_trunc(desc, 120) else NULL
  subtitle_txt <- if (!is.null(desc_show)) {
    paste0(desc_show, " — ", yr_span)
  } else {
    yr_span
  }

  df_plot <- df_item %>%
    mutate(
      month_fac = to_month_factor(.data[[month_col]]),
      month_num = as.integer(month_fac),
      year_fac  = factor(.data[[year_col]])
    ) %>%
    arrange(year_fac, month_num) %>%
    group_by(.data[[item_col]], year_fac) %>%
    # ---- NEW: cumulative value within each item-year ----
    mutate(value_cum = cumsum(.data[[value_col]])) %>%
    ungroup()

  y_mapping <- if (use_cumulative) "value_cum" else value_col

  ggplot(df_plot, aes(x = month_fac, y = .data[[y_mapping]], color = year_fac, group = year_fac)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2) +
    scale_y_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
    labs(
      title = paste0("Monthly Values by Year — ", item_name),
      subtitle = subtitle_txt,
      x = NULL, y = NULL, color = "Year"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 13),
      axis.text.y = element_text(size = 13),
      legend.title = element_text(size = 13),
      legend.text  = element_text(size = 12),
      plot.title   = element_text(size = 18, face = "bold", hjust = 0, margin = margin(l = -10)),
      plot.subtitle = element_text(size = 14, hjust = 0, margin = margin(l = -10)),
      plot.title.position = "plot",
      plot.margin = margin(t = 10, r = 12, b = 10, l = 0)
    )
}

# ---- Clean data, compute cumulative, and save one plot per item ----
df_clean <- df %>%
  # basic type hygiene
  mutate(
    across(all_of(year_col), as.integer),
    across(all_of(value_col), as.numeric)
  ) %>%
  # drop rows missing essentials
  filter(
    !is.na(.data[[item_col]]),
    !is.na(.data[[year_col]]),
    !is.na(.data[[month_col]]),
    !is.na(.data[[value_col]])
  )

# Split and save
df_clean %>%
  group_split(.data[[item_col]], .keep = TRUE) %>%
  walk2(
    .y = seq_along(.),
    .x = .,
    .f = function(dfi, i) {
      raw_name <- unique(dfi[[item_col]])[1]
      label_name <- if (is.na(raw_name) || !nzchar(as.character(raw_name))) paste0("Item ", sprintf("%03d", i)) else as.character(raw_name)
      safe_name  <- sanitize_filename(label_name, fallback = paste0("item-", sprintf("%03d", i)))
      file_path  <- file.path(out_dir, paste0("monthly-values-", safe_name, ".png"))

      tryCatch(
        {
          p <- plot_item(dfi, use_cumulative = TRUE)  # toggle FALSE to plot raw monthly values
          ggsave(filename = file_path, plot = p, width = 8.5, height = 5, dpi = 300)
          message("Saved: ", file_path)
        },
        error = function(e) {
          message("Failed on item index ", i, " (label: ", label_name, "): ", conditionMessage(e))
        }
      )
    }
  )
