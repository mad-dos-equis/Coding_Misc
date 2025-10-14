# ---- Packages ----
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(purrr)

# ─────────────────────────────────────────────────────────────────────────────
# CONFIG: set to your actual column names (strings)
item_col  <- "item"          # e.g., "product_name"
year_col  <- "year"
month_col <- "month"         # 1–12 or "Jan"/"February"/etc.
value_col <- "value"
desc_col  <- "description"   # set to NULL if no description column

# Output folder
out_dir <- "outputs/item_charts"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
# ─────────────────────────────────────────────────────────────────────────────

# =============================================================================
# 1) Build a cleaned/aggregated data frame with value_cum OUTSIDE the function
#    - Aggregates multiple rows within the same item–year–month (sum of value)
#    - Computes cumulative within each item–year across months
# =============================================================================
df_cum <- df %>%
  # basic type hygiene
  mutate(
    !!year_col  := as.integer(.data[[year_col]]),
    !!value_col := as.numeric(.data[[value_col]])
  ) %>%
  # drop rows missing essentials
  filter(
    !is.na(.data[[item_col]]),
    !is.na(.data[[year_col]]),
    !is.na(.data[[month_col]]),
    !is.na(.data[[value_col]])
  ) %>%
  # aggregate duplicates within month (if any)
  group_by(.data[[item_col]], .data[[year_col]], .data[[month_col]]) %>%
  summarise(!!value_col := sum(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
  # cumulative within item-year; month order will be set inside plot_item()
  arrange(.data[[item_col]], .data[[year_col]]) %>%
  group_by(.data[[item_col]], .data[[year_col]]) %>%
  mutate(value_cum = cumsum(.data[[value_col]])) %>%
  ungroup()

# =============================================================================
# 2) Plot function (helpers defined & applied INSIDE the function)
#    - recomputes month factor safely
#    - prefers description as subtitle
#    - returns both plot and safe filename stem
# =============================================================================
plot_item <- function(df_item, use_cumulative = TRUE) {
  # ---- local helpers (scoped to this function) ----
  to_month_factor <- function(m) {
    if (is.numeric(m)) return(factor(month.abb[m], levels = month.abb))
    m_chr <- as.character(m)
    i_full <- suppressWarnings(match(tolower(m_chr), tolower(month.name)))  # "january"
    abb_from_full <- ifelse(!is.na(i_full), month.abb[i_full], NA_character_)
    i_abb  <- match(stringr::str_to_title(m_chr), month.abb)                # "Jan"
    resolved <- ifelse(!is.na(abb_from_full), abb_from_full, month.abb[i_abb])
    factor(resolved, levels = month.abb)
  }
  sanitize_filename <- function(x, fallback = "item") {
    if (requireNamespace("stringi", quietly = TRUE)) {
      x <- stringi::stri_trans_general(x, "Any-Latin; NFD; [:Nonspacing Mark:] Remove; NFC")
    }
    x <- x |>
      tolower() |>
      str_replace_all("[^a-z0-9]+", "-") |>
      str_replace_all("^[-]+|[-]+$", "")
    if (is.na(x) || !nzchar(x)) fallback else x
  }

  # ---- item name and safe filename ----
  item_name <- unique(df_item[[item_col]])[1] |> as.character()
  label     <- if (is.na(item_name) || !nzchar(item_name)) "Item" else item_name
  safe_stem <- sanitize_filename(label, fallback = "item")

  # ---- subtitle: prefer description if provided; else year span ----
  desc <- if (!is.null(desc_col) && desc_col %in% names(df_item)) {
    d <- unique(na.omit(df_item[[desc_col]]))
    if (length(d)) as.character(d[1]) else NA_character_
  } else NA_character_

  yr_min <- suppressWarnings(min(df_item[[year_col]], na.rm = TRUE))
  yr_max <- suppressWarnings(max(df_item[[year_col]], na.rm = TRUE))
  yr_span <- if (is.finite(yr_min) && is.finite(yr_max) && yr_min != yr_max) {
    paste0(yr_min, "–", yr_max, " Comparison")
  } else as.character(yr_min)

  subtitle_txt <- if (!is.na(desc) && nzchar(desc)) str_trunc(desc, 120) else yr_span

  # ---- prep data for plotting (apply helper here) ----
  df_plot <- df_item %>%
    mutate(
      month_fac = to_month_factor(.data[[month_col]]),
      year_fac  = factor(.data[[year_col]])
    ) %>%
    arrange(year_fac, month_fac)

  # which y to plot
  y_mapping <- if (use_cumulative) "value_cum" else value_col

  # draw lines only for year groups with >=2 points to avoid warning
  df_lines <- df_plot %>% group_by(year_fac) %>% filter(dplyr::n() >= 2) %>% ungroup()

  # ---- the plot ----
  p <- ggplot(df_plot, aes(x = month_fac, y = .data[[y_mapping]], color = year_fac, group = year_fac)) +
    geom_line(data = df_lines, linewidth = 1.2) +
    geom_point(size = 2) +
    scale_y_continuous(labels = scales::label_dollar(prefix = "$", big.mark = ",")) +
    labs(
      title = paste0("Monthly Values by Year — ", label),
      subtitle = subtitle_txt,
      x = NULL, y = NULL, color = "Year"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),     # only major gridlines
      axis.text.x = element_text(size = 13),
      axis.text.y = element_text(size = 13),
      legend.title = element_text(size = 13),
      legend.text  = element_text(size = 12),
      plot.title.position = "plot",           # clean left align
      plot.title    = element_text(size = 18, face = "bold", hjust = 0,
                                   margin = margin(l = -10)),
      plot.subtitle = element_text(size = 14, hjust = 0,
                                   margin = margin(l = -10)),
      plot.margin = margin(t = 10, r = 12, b = 10, l = 0)
    )

  # return both
  list(plot = p, safe_stem = safe_stem)
}

# =============================================================================
# 3) Generate & save one plot per item (using safe_stem returned by plot_item)
# =============================================================================
df_cum %>%
  group_split(.data[[item_col]], .keep = TRUE) %>%
  walk2(.x = ., .y = seq_along(.), .f = function(dfi, i) {
    res <- tryCatch(
      plot_item(dfi, use_cumulative = TRUE),  # set FALSE for raw monthly values
      error = function(e) {
        message("Failed on item index ", i, ": ", conditionMessage(e))
        return(NULL)
      }
    )
    if (is.null(res)) return(invisible())

    file_path <- file.path(out_dir, paste0("monthly-values-", res$safe_stem, ".png"))
    ggsave(filename = file_path, plot = res$plot, width = 8.5, height = 5, dpi = 300)
    message("Saved: ", file_path)
  })
