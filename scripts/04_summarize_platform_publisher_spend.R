# Platform and publisher spend summary
#
# Purpose:
#   Create simple descriptive tables that answer:
#   - How much was spent on Google?
#   - How much was spent on Meta?
#   - How much did each of the 68 publishers spend on each platform?
#
# This script is intentionally separate from 05_event_study_0710.R and
# 06_did_0710.R. It does not estimate regressions and does not use the events
# file. It only summarizes the cleaned weekly spend inputs.
#
# Default inputs:
#   data/processed/second_cleaning/weekly_party_spend_google.csv
#   data/processed/second_cleaning/weekly_party_spend_meta.csv
#
# Default outputs:
#   outputs/analysis/descriptive/platform_spend_summary.csv
#   outputs/analysis/descriptive/publisher_platform_spend_summary.csv
#   outputs/analysis/descriptive/publisher_count_validation.csv
#   outputs/analysis/descriptive/publisher_group_platform_spend_summary.csv
#
# Run from repo root:
#   Rscript scripts/04_summarize_platform_publisher_spend.R
#
# Optional arguments:
#   Rscript scripts/04_summarize_platform_publisher_spend.R <google_csv> <meta_csv> <output_dir>

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(readr)
  library(stringr)
  library(tidyr)
})

options(scipen = 999)

args <- commandArgs(trailingOnly = TRUE)

google_data_path <- if (length(args) >= 1) {
  args[[1]]
} else {
  "./data/processed/second_cleaning/weekly_party_spend_google.csv"
}

meta_data_path <- if (length(args) >= 2) {
  args[[2]]
} else {
  "./data/processed/second_cleaning/weekly_party_spend_meta.csv"
}

output_directory <- if (length(args) >= 3) {
  args[[3]]
} else {
  "./outputs/analysis/descriptive"
}

analysis_start_week <- as.Date("2020-01-05")
analysis_end_week <- as.Date("2025-12-28")
expected_publishers <- 68L

dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

parse_date_flexible <- function(x) {
  parsed <- suppressWarnings(lubridate::dmy(x))
  missing <- is.na(parsed)
  if (any(missing)) {
    parsed[missing] <- suppressWarnings(lubridate::ymd(x[missing]))
  }
  as.Date(parsed)
}

normalize_publisher_group <- function(class_values) {
  dplyr::case_when(
    stringr::str_detect(class_values, "מפלגה|party|Party") ~ "political_party",
    TRUE ~ "other_org_or_person"
  )
}

format_output_table <- function(dataframe, digits = 3L) {
  dataframe %>%
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ round(.x, digits = digits)
      )
    )
}

write_clean_csv <- function(dataframe, file_path, digits = 3L) {
  readr::write_csv(format_output_table(dataframe, digits = digits), file_path, na = "")
}

read_weekly_spend_file <- function(file_path) {
  weekly_spend <- readr::read_csv(file_path, show_col_types = FALSE)
  names(weekly_spend) <- trimws(names(weekly_spend))

  required_columns <- c(
    "source",
    "party_name",
    "week_start_sunday",
    "total_spend_week",
    "class",
    "currency"
  )
  missing_columns <- setdiff(required_columns, names(weekly_spend))

  if (length(missing_columns) > 0) {
    stop(file_path, " missing columns: ", paste(missing_columns, collapse = ", "))
  }

  weekly_spend
}

weekly_spend_panel <- dplyr::bind_rows(
  read_weekly_spend_file(google_data_path),
  read_weekly_spend_file(meta_data_path)
) %>%
  dplyr::transmute(
    data_source = as.character(source),
    publisher = as.character(party_name),
    week_start_sunday = parse_date_flexible(week_start_sunday),
    weekly_spend_ils = as.numeric(total_spend_week),
    publisher_group = normalize_publisher_group(as.character(class)),
    currency = as.character(currency)
  ) %>%
  dplyr::filter(
    !is.na(week_start_sunday),
    !is.na(weekly_spend_ils),
    week_start_sunday >= analysis_start_week,
    week_start_sunday <= analysis_end_week
  )

if (nrow(weekly_spend_panel) == 0) {
  stop("No valid weekly spending rows after cleaning.")
}

publisher_count <- dplyr::n_distinct(weekly_spend_panel$publisher)
if (publisher_count != expected_publishers) {
  stop(
    "Expected exactly ",
    expected_publishers,
    " publishers in the analysis window, found ",
    publisher_count,
    "."
  )
}

platform_spend_summary <- weekly_spend_panel %>%
  dplyr::group_by(data_source) %>%
  dplyr::summarise(
    analysis_start_week = analysis_start_week,
    analysis_end_week = analysis_end_week,
    total_spend_ils = sum(weekly_spend_ils, na.rm = TRUE),
    share_of_combined_spend_pct = 100 * total_spend_ils / sum(weekly_spend_panel$weekly_spend_ils, na.rm = TRUE),
    publishers = dplyr::n_distinct(publisher),
    rows = dplyr::n(),
    active_week_rows = dplyr::n_distinct(week_start_sunday),
    average_weekly_row_spend_ils = mean(weekly_spend_ils, na.rm = TRUE),
    median_weekly_row_spend_ils = median(weekly_spend_ils, na.rm = TRUE),
    sd_weekly_row_spend_ils = stats::sd(weekly_spend_ils, na.rm = TRUE),
    min_weekly_row_spend_ils = min(weekly_spend_ils, na.rm = TRUE),
    p25_weekly_row_spend_ils = as.numeric(stats::quantile(weekly_spend_ils, 0.25, na.rm = TRUE)),
    p75_weekly_row_spend_ils = as.numeric(stats::quantile(weekly_spend_ils, 0.75, na.rm = TRUE)),
    max_weekly_row_spend_ils = max(weekly_spend_ils, na.rm = TRUE),
    first_week = min(week_start_sunday, na.rm = TRUE),
    last_week = max(week_start_sunday, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(data_source)

publisher_platform_long <- weekly_spend_panel %>%
  dplyr::group_by(publisher, publisher_group, data_source) %>%
  dplyr::summarise(
    total_spend_ils = sum(weekly_spend_ils, na.rm = TRUE),
    rows = dplyr::n(),
    active_week_rows = dplyr::n_distinct(week_start_sunday),
    average_weekly_row_spend_ils = mean(weekly_spend_ils, na.rm = TRUE),
    median_weekly_row_spend_ils = median(weekly_spend_ils, na.rm = TRUE),
    sd_weekly_row_spend_ils = stats::sd(weekly_spend_ils, na.rm = TRUE),
    min_weekly_row_spend_ils = min(weekly_spend_ils, na.rm = TRUE),
    p25_weekly_row_spend_ils = as.numeric(stats::quantile(weekly_spend_ils, 0.25, na.rm = TRUE)),
    p75_weekly_row_spend_ils = as.numeric(stats::quantile(weekly_spend_ils, 0.75, na.rm = TRUE)),
    max_weekly_row_spend_ils = max(weekly_spend_ils, na.rm = TRUE),
    first_week = min(week_start_sunday, na.rm = TRUE),
    last_week = max(week_start_sunday, na.rm = TRUE),
    .groups = "drop"
  )

publisher_platform_spend_summary <- publisher_platform_long %>%
  tidyr::pivot_wider(
    names_from = data_source,
    values_from = c(
      total_spend_ils,
      rows,
      active_week_rows,
      average_weekly_row_spend_ils,
      median_weekly_row_spend_ils,
      sd_weekly_row_spend_ils,
      min_weekly_row_spend_ils,
      p25_weekly_row_spend_ils,
      p75_weekly_row_spend_ils,
      max_weekly_row_spend_ils,
      first_week,
      last_week
    ),
    values_fill = list(
      total_spend_ils = 0,
      rows = 0,
      active_week_rows = 0
    )
  ) %>%
  dplyr::mutate(
    total_spend_ils_google = dplyr::coalesce(total_spend_ils_google, 0),
    total_spend_ils_meta = dplyr::coalesce(total_spend_ils_meta, 0),
    rows_google = dplyr::coalesce(rows_google, 0L),
    rows_meta = dplyr::coalesce(rows_meta, 0L),
    active_week_rows_google = dplyr::coalesce(active_week_rows_google, 0L),
    active_week_rows_meta = dplyr::coalesce(active_week_rows_meta, 0L),
    total_spend_ils_all_sources = total_spend_ils_google + total_spend_ils_meta,
    google_share_of_publisher_spend_pct = dplyr::if_else(
      total_spend_ils_all_sources > 0,
      100 * total_spend_ils_google / total_spend_ils_all_sources,
      NA_real_
    ),
    meta_share_of_publisher_spend_pct = dplyr::if_else(
      total_spend_ils_all_sources > 0,
      100 * total_spend_ils_meta / total_spend_ils_all_sources,
      NA_real_
    ),
    appears_in_google = rows_google > 0,
    appears_in_meta = rows_meta > 0
  ) %>%
  dplyr::select(
    publisher,
    publisher_group,
    total_spend_ils_all_sources,
    total_spend_ils_google,
    total_spend_ils_meta,
    google_share_of_publisher_spend_pct,
    meta_share_of_publisher_spend_pct,
    appears_in_google,
    appears_in_meta,
    dplyr::everything()
  ) %>%
  dplyr::arrange(dplyr::desc(total_spend_ils_all_sources), publisher)

google_publisher_names <- unique(weekly_spend_panel$publisher[weekly_spend_panel$data_source == "google"])
meta_publisher_names <- unique(weekly_spend_panel$publisher[weekly_spend_panel$data_source == "meta"])

publisher_count_validation <- tibble::tibble(
  analysis_start_week = analysis_start_week,
  analysis_end_week = analysis_end_week,
  expected_publishers = expected_publishers,
  actual_publishers = publisher_count,
  publisher_count_status = "PASS",
  google_publishers = length(google_publisher_names),
  meta_publishers = length(meta_publisher_names),
  publishers_in_both_sources = length(base::intersect(google_publisher_names, meta_publisher_names)),
  google_only_publishers = length(base::setdiff(google_publisher_names, meta_publisher_names)),
  meta_only_publishers = length(base::setdiff(meta_publisher_names, google_publisher_names)),
  total_rows = nrow(weekly_spend_panel),
  total_spend_ils = sum(weekly_spend_panel$weekly_spend_ils, na.rm = TRUE)
)

publisher_group_platform_summary <- weekly_spend_panel %>%
  dplyr::group_by(publisher_group, data_source) %>%
  dplyr::summarise(
    total_spend_ils = sum(weekly_spend_ils, na.rm = TRUE),
    publishers = dplyr::n_distinct(publisher),
    rows = dplyr::n(),
    active_week_rows = dplyr::n_distinct(week_start_sunday),
    average_weekly_row_spend_ils = mean(weekly_spend_ils, na.rm = TRUE),
    median_weekly_row_spend_ils = median(weekly_spend_ils, na.rm = TRUE),
    sd_weekly_row_spend_ils = stats::sd(weekly_spend_ils, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(publisher_group, data_source)

write_clean_csv(
  platform_spend_summary,
  file.path(output_directory, "platform_spend_summary.csv")
)
write_clean_csv(
  publisher_platform_spend_summary,
  file.path(output_directory, "publisher_platform_spend_summary.csv")
)
write_clean_csv(
  publisher_count_validation,
  file.path(output_directory, "publisher_count_validation.csv")
)
write_clean_csv(
  publisher_group_platform_summary,
  file.path(output_directory, "publisher_group_platform_spend_summary.csv")
)

cat("Saved platform/publisher spend summaries to: ", normalizePath(output_directory), "\n", sep = "")
cat("Publisher validation: ", publisher_count, " publishers (expected ", expected_publishers, ").\n", sep = "")
