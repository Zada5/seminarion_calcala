rm(list = ls())
gc()
cat("\014")

# =====================================================
# Weekly Spending Analysis + Event-Study Regressions
# =====================================================
# Goal:
# 1) Provide readable descriptive statistics
# 2) Run event-study regressions split by:
#    - political parties vs. other organizations/people
#    - political events vs. terror events
#
# Run in RStudio or command line:
# Rscript event_study_0710.R [google_csv] [meta_csv] [events_csv] [output_dir] [window_weeks]

required_packages <- c(
  "readr", "dplyr", "tidyr", "lubridate", "stringr",
  "ggplot2", "fixest", "broom", "purrr", "tibble"
)

install_missing_packages <- function(package_names) {
  missing_packages <- package_names[!sapply(package_names, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    install.packages(missing_packages, repos = "https://cloud.r-project.org")
  }
}

install_missing_packages(required_packages)
invisible(lapply(required_packages, library, character.only = TRUE))

resolve_default_path <- function(candidate_paths) {
  existing <- candidate_paths[file.exists(candidate_paths)]
  if (length(existing) == 0) {
    stop("Could not find default file. Checked: ", paste(candidate_paths, collapse = ", "))
  }
  existing[1]
}

parse_date_flexible <- function(date_values) {
  parsed_dates <- suppressWarnings(lubridate::ymd(date_values, quiet = TRUE))
  missing_dates <- is.na(parsed_dates)
  if (any(missing_dates)) {
    parsed_dates[missing_dates] <- suppressWarnings(lubridate::dmy(date_values[missing_dates], quiet = TRUE))
  }
  parsed_dates
}

normalize_entity_group <- function(class_values) {
  dplyr::case_when(
    stringr::str_detect(class_values, "מפלגה|party|Party") ~ "political_party",
    TRUE ~ "other_org_or_person"
  )
}

get_next_sunday <- function(input_date) {
  input_date <- as.Date(input_date)
  days_until_sunday <- (7 - as.integer(format(input_date, "%w"))) %% 7
  input_date + lubridate::days(days_until_sunday)
}

print_section <- function(title) {
  border <- paste(rep("=", nchar(title) + 8), collapse = "")
  cat("\n", border, "\n", "== ", title, " ==\n", border, "\n", sep = "")
}

safe_fitstat <- function(model_object, stat_name) {
  stat_value <- tryCatch(fixest::fitstat(model_object, stat_name), error = function(e) NA_real_)
  as.numeric(stat_value)[1]
}

round_numeric_columns <- function(dataframe, digits = 3L) {
  dataframe %>%
    dplyr::mutate(
      dplyr::across(
        .cols = where(is.numeric),
        .fns = ~ round(as.numeric(Re(.x)), digits = digits)
      )
    )
}

write_clean_csv <- function(dataframe, file_path, digits = 3L) {
  readr::write_csv(round_numeric_columns(dataframe, digits = digits), file_path)
}

add_baseline_coefficient_row <- function(coefficients_table, model_name_label) {
  baseline_row <- tibble::tibble(
    model_name = model_name_label,
    relative_week = 0L,
    estimate = 0,
    std.error = NA_real_,
    statistic = NA_real_,
    p.value = NA_real_,
    conf.low = 0,
    conf.high = 0
  )

  dplyr::bind_rows(coefficients_table, baseline_row) %>%
    dplyr::distinct(model_name, relative_week, .keep_all = TRUE) %>%
    dplyr::arrange(relative_week)
}

plot_event_study_coefficients <- function(coefficients_table, plot_title, plot_subtitle) {
  ggplot2::ggplot(coefficients_table, ggplot2::aes(x = relative_week, y = estimate)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted") +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
    ggplot2::scale_x_continuous(breaks = seq(-analysis_window_weeks, analysis_window_weeks, by = 1)) +
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = "Weeks relative to event week",
      y = "Estimated change vs baseline week (95% CI)"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

run_event_study_model <- function(model_data) {
  if (nrow(model_data) < 10) {
    return(NULL)
  }
  if (dplyr::n_distinct(model_data$entity_name) < 2 || dplyr::n_distinct(model_data$relative_week) < 2) {
    return(NULL)
  }

  model_formula <- if (dplyr::n_distinct(model_data$event_id) > 1) {
    as.formula("log_weekly_spend ~ i(relative_week, ref = 0) | entity_name + data_source + event_id")
  } else {
    as.formula("log_weekly_spend ~ i(relative_week, ref = 0) | entity_name + data_source")
  }

  tryCatch(
    fixest::feols(model_formula, data = model_data, vcov = ~ entity_name),
    error = function(e) {
      message("Model failed: ", conditionMessage(e))
      NULL
    }
  )
}

extract_relative_week_coefficients <- function(model_object, model_name_label) {
  if (is.null(model_object)) {
    return(tibble::tibble())
  }

  broom::tidy(model_object, conf.int = TRUE, conf.level = 0.95) %>%
    dplyr::filter(stringr::str_detect(term, "^relative_week::")) %>%
    dplyr::mutate(
      relative_week = as.integer(stringr::str_extract(term, "-?\\d+$")),
      model_name = model_name_label
    ) %>%
    dplyr::select(
      model_name,
      relative_week,
      estimate,
      std.error,
      statistic,
      p.value,
      conf.low,
      conf.high
    ) %>%
    dplyr::arrange(relative_week)
}

extract_model_fit <- function(model_object, model_name_label, input_row_count) {
  if (is.null(model_object)) {
    return(
      tibble::tibble(
        model_name = model_name_label,
        model_status = "failed_or_insufficient_data",
        input_rows = input_row_count,
        used_rows = NA_real_,
        r2 = NA_real_,
        within_r2 = NA_real_,
        adjusted_r2 = NA_real_
      )
    )
  }

  tibble::tibble(
    model_name = model_name_label,
    model_status = "ok",
    input_rows = input_row_count,
    used_rows = nobs(model_object),
    r2 = safe_fitstat(model_object, "r2"),
    within_r2 = safe_fitstat(model_object, "wr2"),
    adjusted_r2 = safe_fitstat(model_object, "ar2")
  )
}

plot_correlation_coefficients <- function(correlation_table, plot_title, plot_subtitle) {
  ggplot2::ggplot(
    correlation_table,
    ggplot2::aes(
      x = event_scope,
      y = entity_group,
      fill = correlation_weekly_spend_vs_event_count
    )
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.3f", correlation_weekly_spend_vs_event_count)),
      size = 3.3
    ) +
    ggplot2::scale_fill_gradient2(
      low = "#2166AC",
      mid = "#F7F7F7",
      high = "#B2182B",
      midpoint = 0,
      na.value = "grey85"
    ) +
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = "Event scope",
      y = "Entity group",
      fill = "Correlation"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

plot_correlation_scatter <- function(correlation_panel_long, plot_title, plot_subtitle) {
  ggplot2::ggplot(
    correlation_panel_long,
    ggplot2::aes(x = weekly_event_count, y = total_weekly_spend_ils)
  ) +
    ggplot2::geom_point(alpha = 0.6, size = 1.8) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#1B4D89", size = 0.7) +
    ggplot2::facet_grid(entity_group ~ event_scope, scales = "free_y") +
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = "Weekly event count",
      y = "Total weekly spend (ILS)"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

build_event_count_wide <- function(input_events_table) {
  weekly_event_counts <- input_events_table %>%
    dplyr::group_by(event_week_start_sunday, event_type_group) %>%
    dplyr::summarise(event_count = dplyr::n(), .groups = "drop") %>%
    dplyr::rename(week_start_sunday = event_week_start_sunday)

  weekly_event_counts %>%
    tidyr::pivot_wider(
      names_from = event_type_group,
      values_from = event_count,
      values_fill = 0
    ) %>%
    dplyr::mutate(all_events = political + terror)
}

build_correlation_outputs <- function(event_count_wide_table, spend_totals_for_correlation) {
  correlation_panel_local <- spend_totals_for_correlation %>%
    dplyr::left_join(event_count_wide_table, by = "week_start_sunday") %>%
    dplyr::mutate(
      all_events = dplyr::coalesce(all_events, 0),
      political = dplyr::coalesce(political, 0),
      terror = dplyr::coalesce(terror, 0)
    )

  correlation_panel_long_local <- correlation_panel_local %>%
    tidyr::pivot_longer(
      cols = c(all_events, political, terror),
      names_to = "event_scope",
      values_to = "weekly_event_count"
    )

  correlation_summary_local <- correlation_panel_long_local %>%
    dplyr::group_by(entity_group, event_scope) %>%
    dplyr::summarise(
      correlation_weekly_spend_vs_event_count = safe_correlation(total_weekly_spend_ils, weekly_event_count),
      weeks_in_sample = dplyr::n(),
      weeks_with_any_event = sum(weekly_event_count > 0, na.rm = TRUE),
      average_weekly_spend_ils = mean(total_weekly_spend_ils, na.rm = TRUE),
      average_weekly_event_count = mean(weekly_event_count, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(entity_group, event_scope)

  list(
    correlation_panel = correlation_panel_local,
    correlation_panel_long = correlation_panel_long_local,
    correlation_summary = correlation_summary_local
  )
}

create_placebo_events <- function(real_events_table, candidate_weeks, min_gap_weeks = 3L, seed = 7102023L) {
  real_event_weeks <- unique(real_events_table$event_week_start_sunday)

  valid_placebo_weeks <- candidate_weeks[
    sapply(candidate_weeks, function(candidate_week) {
      all(abs(as.integer(candidate_week - real_event_weeks) / 7) > min_gap_weeks)
    })
  ]

  required_n <- nrow(real_events_table)
  if (length(valid_placebo_weeks) < required_n) {
    stop(
      "Not enough valid placebo weeks after excluding weeks near real events. Needed: ",
      required_n,
      ", available: ",
      length(valid_placebo_weeks)
    )
  }

  set.seed(seed)
  placebo_weeks <- sample(valid_placebo_weeks, size = required_n, replace = FALSE)
  placebo_types <- sample(real_events_table$event_type_group, size = required_n, replace = FALSE)

  tibble::tibble(
    event_date = placebo_weeks,
    event_type_raw = paste0("placebo_", placebo_types),
    event_type_group = placebo_types,
    event_name = paste0("placebo_event_", seq_len(required_n)),
    event_details = "Random placebo week (seeded) with buffer from real events",
    event_id = seq_len(required_n),
    event_week_start_sunday = placebo_weeks
  ) %>%
    dplyr::arrange(event_week_start_sunday) %>%
    dplyr::mutate(event_id = dplyr::row_number())
}

read_weekly_spend_file <- function(file_path) {
  dataset <- readr::read_csv(file_path, show_col_types = FALSE)
  names(dataset) <- trimws(names(dataset))
  dataset
}

# -------------------------
# Inputs and configuration
# -------------------------
script_arguments <- commandArgs(trailingOnly = TRUE)

default_google_path <- resolve_default_path(c(
  "./second_cleaning/weekly_party_spend_google.csv",
  "./cleaned_data/weekly_party_spend_google.csv"
))
default_meta_path <- resolve_default_path(c(
  "./second_cleaning/weekly_party_spend_meta.csv",
  "./cleaned_data/weekly_party_spend_meta.csv"
))
default_events_path <- resolve_default_path(c(
  "./Consolidated List of Terror and Political incidents 2020-2025 v3.csv"
))

google_data_path <- if (length(script_arguments) >= 1) script_arguments[1] else default_google_path
meta_data_path <- if (length(script_arguments) >= 2) script_arguments[2] else default_meta_path
events_data_path <- if (length(script_arguments) >= 3) script_arguments[3] else default_events_path
output_directory <- if (length(script_arguments) >= 4) script_arguments[4] else "./analysis_outputs"
analysis_window_weeks <- if (length(script_arguments) >= 5) as.integer(script_arguments[5]) else 2L

if (is.na(analysis_window_weeks) || analysis_window_weeks < 1L) {
  stop("window_weeks must be an integer >= 1")
}

dir.create(output_directory, showWarnings = FALSE, recursive = TRUE)

print_section("Input Files")
cat("Google weekly data: ", google_data_path, "\n", sep = "")
cat("Meta weekly data  : ", meta_data_path, "\n", sep = "")
cat("Events file       : ", events_data_path, "\n", sep = "")
cat("Output directory  : ", output_directory, "\n", sep = "")
cat("Event window      : +/- ", analysis_window_weeks, " weeks\n", sep = "")

# -------------------------
# Load and clean weekly spending data
# -------------------------
google_weekly_spend <- read_weekly_spend_file(google_data_path)
meta_weekly_spend <- read_weekly_spend_file(meta_data_path)

required_spend_columns <- c("source", "party_name", "week_start_sunday", "total_spend_week", "class", "currency")
missing_google_columns <- setdiff(required_spend_columns, names(google_weekly_spend))
missing_meta_columns <- setdiff(required_spend_columns, names(meta_weekly_spend))

if (length(missing_google_columns) > 0) {
  stop("Google file missing columns: ", paste(missing_google_columns, collapse = ", "))
}
if (length(missing_meta_columns) > 0) {
  stop("Meta file missing columns: ", paste(missing_meta_columns, collapse = ", "))
}

weekly_spend_panel <- dplyr::bind_rows(google_weekly_spend, meta_weekly_spend) %>%
  dplyr::transmute(
    data_source = as.character(source),
    entity_name = as.character(party_name),
    week_start_sunday = parse_date_flexible(week_start_sunday),
    weekly_spend_ils = as.numeric(total_spend_week),
    entity_class_raw = as.character(class),
    entity_group = normalize_entity_group(as.character(class)),
    currency = as.character(currency)
  ) %>%
  dplyr::filter(!is.na(week_start_sunday), !is.na(weekly_spend_ils)) %>%
  dplyr::mutate(calendar_year = lubridate::year(week_start_sunday))

if (nrow(weekly_spend_panel) == 0) {
  stop("No valid weekly spending rows after cleaning.")
}

# -------------------------
# Load and clean events data
# -------------------------
raw_events <- readr::read_csv(events_data_path, show_col_types = FALSE)
names(raw_events) <- trimws(names(raw_events))

required_event_columns <- c("Date", "Type", "Article")
missing_event_columns <- setdiff(required_event_columns, names(raw_events))
if (length(missing_event_columns) > 0) {
  stop("Events file missing columns: ", paste(missing_event_columns, collapse = ", "))
}
if (!"Details" %in% names(raw_events)) {
  raw_events$Details <- NA_character_
}

events_table <- raw_events %>%
  dplyr::transmute(
    event_date = parse_date_flexible(Date),
    event_type_raw = as.character(Type),
    event_type_group = dplyr::case_when(
      stringr::str_detect(tolower(Type), "terror") ~ "terror",
      stringr::str_detect(tolower(Type), "political") ~ "political",
      TRUE ~ "other"
    ),
    event_name = as.character(Article),
    event_details = as.character(Details)
  ) %>%
  dplyr::filter(!is.na(event_date), event_type_group %in% c("political", "terror")) %>%
  dplyr::arrange(event_date) %>%
  dplyr::mutate(
    event_id = dplyr::row_number(),
    event_week_start_sunday = get_next_sunday(event_date)
  )

if (nrow(events_table) == 0) {
  stop("No valid political/terror events found in events file.")
}

# -------------------------
# Week-level spend/event panel for correlation summaries
# -------------------------
weekly_spend_totals <- weekly_spend_panel %>%
  dplyr::group_by(week_start_sunday, entity_group) %>%
  dplyr::summarise(
    total_weekly_spend_ils = sum(weekly_spend_ils, na.rm = TRUE),
    .groups = "drop"
  )

weekly_spend_totals_all_entities <- weekly_spend_panel %>%
  dplyr::group_by(week_start_sunday) %>%
  dplyr::summarise(
    total_weekly_spend_ils = sum(weekly_spend_ils, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(entity_group = "all_entities")

weekly_spend_totals_for_correlation <- dplyr::bind_rows(
  weekly_spend_totals_all_entities,
  weekly_spend_totals
)

safe_correlation <- function(x, y) {
  valid_rows <- stats::complete.cases(x, y)
  x <- x[valid_rows]
  y <- y[valid_rows]

  if (length(x) < 3 || stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(NA_real_)
  }

  stats::cor(x, y)
}

weekly_event_counts_wide <- build_event_count_wide(events_table)
correlation_outputs <- build_correlation_outputs(weekly_event_counts_wide, weekly_spend_totals_for_correlation)
correlation_panel <- correlation_outputs$correlation_panel
correlation_panel_long <- correlation_outputs$correlation_panel_long
correlation_summary <- correlation_outputs$correlation_summary

# -------------------------
# Descriptive statistics
# -------------------------
overall_spend_stats <- weekly_spend_panel %>%
  dplyr::summarise(
    total_spend_ils = sum(weekly_spend_ils, na.rm = TRUE),
    average_weekly_row_spend_ils = mean(weekly_spend_ils, na.rm = TRUE),
    median_weekly_row_spend_ils = median(weekly_spend_ils, na.rm = TRUE),
    total_rows = dplyr::n(),
    total_entities = dplyr::n_distinct(entity_name),
    first_week = min(week_start_sunday, na.rm = TRUE),
    last_week = max(week_start_sunday, na.rm = TRUE)
  )

spend_stats_by_group <- weekly_spend_panel %>%
  dplyr::group_by(entity_group) %>%
  dplyr::summarise(
    total_spend_ils = sum(weekly_spend_ils, na.rm = TRUE),
    average_weekly_row_spend_ils = mean(weekly_spend_ils, na.rm = TRUE),
    median_weekly_row_spend_ils = median(weekly_spend_ils, na.rm = TRUE),
    rows = dplyr::n(),
    entities = dplyr::n_distinct(entity_name),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(total_spend_ils))

yearly_spend_stats <- weekly_spend_panel %>%
  dplyr::group_by(calendar_year) %>%
  dplyr::summarise(
    total_spend_ils = sum(weekly_spend_ils, na.rm = TRUE),
    average_weekly_row_spend_ils = mean(weekly_spend_ils, na.rm = TRUE),
    median_weekly_row_spend_ils = median(weekly_spend_ils, na.rm = TRUE),
    rows = dplyr::n(),
    entities = dplyr::n_distinct(entity_name),
    .groups = "drop"
  ) %>%
  dplyr::arrange(calendar_year) %>%
  dplyr::mutate(
    yoy_total_change_ils = total_spend_ils - dplyr::lag(total_spend_ils),
    yoy_total_change_pct = 100 * (total_spend_ils / dplyr::lag(total_spend_ils) - 1)
  )

yearly_spend_stats_by_group <- weekly_spend_panel %>%
  dplyr::group_by(calendar_year, entity_group) %>%
  dplyr::summarise(
    total_spend_ils = sum(weekly_spend_ils, na.rm = TRUE),
    average_weekly_row_spend_ils = mean(weekly_spend_ils, na.rm = TRUE),
    rows = dplyr::n(),
    entities = dplyr::n_distinct(entity_name),
    .groups = "drop"
  ) %>%
  dplyr::arrange(calendar_year, entity_group)

# Example period split requested by the team: before/after October 7 event-week
# Week 0 for 07/10/2023 is the week that starts on Sunday 2023-10-08.
oct7_reference_week <- as.Date("2023-10-08")
pre_post_oct7_stats <- weekly_spend_panel %>%
  dplyr::mutate(
    period_vs_oct7 = dplyr::if_else(
      week_start_sunday < oct7_reference_week,
      "before_2023_10_08",
      "on_or_after_2023_10_08"
    )
  ) %>%
  dplyr::group_by(period_vs_oct7, entity_group) %>%
  dplyr::summarise(
    total_spend_ils = sum(weekly_spend_ils, na.rm = TRUE),
    average_weekly_row_spend_ils = mean(weekly_spend_ils, na.rm = TRUE),
    rows = dplyr::n(),
    entities = dplyr::n_distinct(entity_name),
    .groups = "drop"
  ) %>%
  dplyr::arrange(period_vs_oct7, entity_group)

# -------------------------
# Event-study dataset (all events)
# -------------------------
event_window_panel <- tidyr::crossing(
  weekly_spend_panel %>%
    dplyr::select(data_source, entity_name, entity_group, week_start_sunday, weekly_spend_ils),
  events_table %>%
    dplyr::select(event_id, event_date, event_week_start_sunday, event_type_group, event_name)
) %>%
  dplyr::mutate(
    relative_week = as.integer((week_start_sunday - event_week_start_sunday) / 7),
    log_weekly_spend = log1p(weekly_spend_ils)
  ) %>%
  dplyr::filter(relative_week >= -analysis_window_weeks, relative_week <= analysis_window_weeks)

if (nrow(event_window_panel) == 0) {
  stop("No rows in event window panel. Check event dates and weekly dates.")
}

# -------------------------
# Regressions requested by user
# -------------------------
model_specifications <- tibble::tribble(
  ~model_name, ~entity_group_filter, ~event_type_filter,
  "all_entities_all_events", "all", "all",
  "political_parties_all_events", "political_party", "all",
  "other_orgs_people_all_events", "other_org_or_person", "all",
  "all_entities_political_events", "all", "political",
  "all_entities_terror_events", "all", "terror",
  "political_parties_political_events", "political_party", "political",
  "political_parties_terror_events", "political_party", "terror",
  "other_orgs_people_political_events", "other_org_or_person", "political",
  "other_orgs_people_terror_events", "other_org_or_person", "terror"
)

model_results <- model_specifications %>%
  dplyr::mutate(
    model_data = purrr::map2(entity_group_filter, event_type_filter, function(group_filter, type_filter) {
      filtered_data <- event_window_panel

      if (group_filter != "all") {
        filtered_data <- filtered_data %>% dplyr::filter(entity_group == group_filter)
      }
      if (type_filter != "all") {
        filtered_data <- filtered_data %>% dplyr::filter(event_type_group == type_filter)
      }

      filtered_data
    }),
    input_rows = purrr::map_int(model_data, nrow),
    model = purrr::map(model_data, run_event_study_model),
    coefficients = purrr::map2(model, model_name, extract_relative_week_coefficients),
    fit = purrr::pmap(list(model, model_name, input_rows), extract_model_fit)
  )

all_model_coefficients <- dplyr::bind_rows(model_results$coefficients)
all_model_fit <- dplyr::bind_rows(model_results$fit)

# -------------------------
# Save event-study figures for every model split
# -------------------------
event_study_figures_directory <- file.path(output_directory, "event_study_figures")
dir.create(event_study_figures_directory, showWarnings = FALSE, recursive = TRUE)

event_study_coefficients_for_plot <- purrr::map_dfr(
  split(all_model_coefficients, all_model_coefficients$model_name),
  ~ add_baseline_coefficient_row(.x, unique(.x$model_name)[1])
)

for (row_index in seq_len(nrow(model_results))) {
  current_model_name <- model_results$model_name[[row_index]]
  current_coefficients <- event_study_coefficients_for_plot %>%
    dplyr::filter(model_name == current_model_name)

  if (nrow(current_coefficients) == 0) {
    next
  }

  current_plot <- plot_event_study_coefficients(
    coefficients_table = current_coefficients,
    plot_title = paste("Event Study:", current_model_name),
    plot_subtitle = "DV: log(1 + weekly spend). Baseline: relative_week = 0"
  )

  ggplot2::ggsave(
    filename = file.path(event_study_figures_directory, paste0(current_model_name, ".png")),
    plot = current_plot,
    width = 9,
    height = 5,
    dpi = 300
  )
}

if (nrow(event_study_coefficients_for_plot) > 0) {
  combined_event_study_plot <- ggplot2::ggplot(
    event_study_coefficients_for_plot,
    ggplot2::aes(x = relative_week, y = estimate)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted") +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
    ggplot2::facet_wrap(~ model_name, scales = "free_y") +
    ggplot2::scale_x_continuous(breaks = seq(-analysis_window_weeks, analysis_window_weeks, by = 1)) +
    ggplot2::labs(
      title = "Event-Study Coefficients Across All Model Splits",
      subtitle = "DV: log(1 + weekly spend). Baseline: relative_week = 0",
      x = "Weeks relative to event week",
      y = "Estimated change vs baseline week (95% CI)"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  ggplot2::ggsave(
    filename = file.path(output_directory, "event_study_figure_all_models.png"),
    plot = combined_event_study_plot,
    width = 14,
    height = 10,
    dpi = 300
  )
}

if (nrow(correlation_summary) > 0) {
  correlation_coefficient_plot <- plot_correlation_coefficients(
    correlation_table = correlation_summary,
    plot_title = "Weekly Spend vs Event Count Correlations (Real Events)",
    plot_subtitle = "Pearson correlation by entity group and event scope"
  )
  ggplot2::ggsave(
    filename = file.path(output_directory, "correlation_coefficients_heatmap.png"),
    plot = correlation_coefficient_plot,
    width = 8.5,
    height = 4.8,
    dpi = 300
  )

  correlation_scatter_plot <- plot_correlation_scatter(
    correlation_panel_long,
    plot_title = "Weekly Spend vs Event Count (Real Events)",
    plot_subtitle = "Points are weeks; line is OLS fit with 95% CI"
  )
  ggplot2::ggsave(
    filename = file.path(output_directory, "correlation_scatter_panels.png"),
    plot = correlation_scatter_plot,
    width = 11.5,
    height = 6.2,
    dpi = 300
  )
}

# -------------------------
# Placebo events (seeded random dates with distance from real events)
# -------------------------
placebo_window_start <- as.Date("2020-01-05")
placebo_window_end <- as.Date("2025-12-28")
candidate_placebo_weeks <- sort(unique(
  weekly_spend_panel$week_start_sunday[
    weekly_spend_panel$week_start_sunday >= placebo_window_start &
      weekly_spend_panel$week_start_sunday <= placebo_window_end
  ]
))
placebo_events_table <- create_placebo_events(
  real_events_table = events_table,
  candidate_weeks = candidate_placebo_weeks,
  min_gap_weeks = analysis_window_weeks + 1L,
  seed = 7102023L
)

placebo_weekly_event_counts_wide <- build_event_count_wide(placebo_events_table)
placebo_correlation_outputs <- build_correlation_outputs(
  placebo_weekly_event_counts_wide,
  weekly_spend_totals_for_correlation
)
placebo_correlation_summary <- placebo_correlation_outputs$correlation_summary
placebo_correlation_panel_long <- placebo_correlation_outputs$correlation_panel_long

placebo_event_window_panel <- tidyr::crossing(
  weekly_spend_panel %>%
    dplyr::select(data_source, entity_name, entity_group, week_start_sunday, weekly_spend_ils),
  placebo_events_table %>%
    dplyr::select(event_id, event_date, event_week_start_sunday, event_type_group, event_name)
) %>%
  dplyr::mutate(
    relative_week = as.integer((week_start_sunday - event_week_start_sunday) / 7),
    log_weekly_spend = log1p(weekly_spend_ils)
  ) %>%
  dplyr::filter(relative_week >= -analysis_window_weeks, relative_week <= analysis_window_weeks)

placebo_model_results <- model_specifications %>%
  dplyr::mutate(
    model_data = purrr::map2(entity_group_filter, event_type_filter, function(group_filter, type_filter) {
      filtered_data <- placebo_event_window_panel

      if (group_filter != "all") {
        filtered_data <- filtered_data %>% dplyr::filter(entity_group == group_filter)
      }
      if (type_filter != "all") {
        filtered_data <- filtered_data %>% dplyr::filter(event_type_group == type_filter)
      }

      filtered_data
    }),
    input_rows = purrr::map_int(model_data, nrow),
    model = purrr::map(model_data, run_event_study_model),
    coefficients = purrr::map2(model, model_name, extract_relative_week_coefficients),
    fit = purrr::pmap(list(model, model_name, input_rows), extract_model_fit)
  )

placebo_model_coefficients <- dplyr::bind_rows(placebo_model_results$coefficients)
placebo_model_fit <- dplyr::bind_rows(placebo_model_results$fit)

placebo_figures_directory <- file.path(output_directory, "placebo_event_study_figures")
dir.create(placebo_figures_directory, showWarnings = FALSE, recursive = TRUE)

if (nrow(placebo_model_coefficients) > 0) {
  placebo_coefficients_for_plot <- purrr::map_dfr(
    split(placebo_model_coefficients, placebo_model_coefficients$model_name),
    ~ add_baseline_coefficient_row(.x, unique(.x$model_name)[1])
  )

  for (row_index in seq_len(nrow(placebo_model_results))) {
    current_model_name <- placebo_model_results$model_name[[row_index]]
    current_coefficients <- placebo_coefficients_for_plot %>%
      dplyr::filter(model_name == current_model_name)

    if (nrow(current_coefficients) == 0) {
      next
    }

    current_plot <- plot_event_study_coefficients(
      coefficients_table = current_coefficients,
      plot_title = paste("Placebo Event Study:", current_model_name),
      plot_subtitle = "DV: log(1 + weekly spend). Baseline: relative_week = 0"
    )

    ggplot2::ggsave(
      filename = file.path(placebo_figures_directory, paste0(current_model_name, ".png")),
      plot = current_plot,
      width = 9,
      height = 5,
      dpi = 300
    )
  }

  combined_placebo_plot <- ggplot2::ggplot(
    placebo_coefficients_for_plot,
    ggplot2::aes(x = relative_week, y = estimate)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted") +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
    ggplot2::facet_wrap(~ model_name, scales = "free_y") +
    ggplot2::scale_x_continuous(breaks = seq(-analysis_window_weeks, analysis_window_weeks, by = 1)) +
    ggplot2::labs(
      title = "Placebo Event-Study Coefficients Across All Model Splits",
      subtitle = "DV: log(1 + weekly spend). Baseline: relative_week = 0",
      x = "Weeks relative to placebo event week",
      y = "Estimated change vs baseline week (95% CI)"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  ggplot2::ggsave(
    filename = file.path(output_directory, "placebo_event_study_figure_all_models.png"),
    plot = combined_placebo_plot,
    width = 14,
    height = 10,
    dpi = 300
  )
}

if (nrow(placebo_correlation_summary) > 0) {
  placebo_correlation_heatmap <- plot_correlation_coefficients(
    correlation_table = placebo_correlation_summary,
    plot_title = "Weekly Spend vs Event Count Correlations (Placebo Events)",
    plot_subtitle = "Pearson correlation by entity group and placebo event scope"
  )

  ggplot2::ggsave(
    filename = file.path(output_directory, "placebo_correlation_coefficients_heatmap.png"),
    plot = placebo_correlation_heatmap,
    width = 8.5,
    height = 4.8,
    dpi = 300
  )

  placebo_correlation_scatter <- plot_correlation_scatter(
    placebo_correlation_panel_long,
    plot_title = "Weekly Spend vs Event Count (Placebo Events)",
    plot_subtitle = "Points are weeks; line is OLS fit with 95% CI"
  )

  ggplot2::ggsave(
    filename = file.path(output_directory, "placebo_correlation_scatter_panels.png"),
    plot = placebo_correlation_scatter,
    width = 11.5,
    height = 6.2,
    dpi = 300
  )
}

# -------------------------
# Keep dedicated 07/10/2023 event-study figure (for continuity)
# -------------------------
oct7_event <- events_table %>%
  dplyr::filter(event_date == as.Date("2023-10-07")) %>%
  dplyr::slice_head(n = 1)

oct7_coefficients_for_plot <- tibble::tibble()
oct7_model <- NULL

if (nrow(oct7_event) == 1) {
  oct7_event_window <- event_window_panel %>%
    dplyr::filter(event_id == oct7_event$event_id)

  oct7_model <- run_event_study_model(oct7_event_window)

  if (!is.null(oct7_model)) {
    oct7_coefficients <- extract_relative_week_coefficients(oct7_model, "oct7_event")
    oct7_coefficients_for_plot <- add_baseline_coefficient_row(oct7_coefficients, "oct7_event")

    oct7_plot <- plot_event_study_coefficients(
      coefficients_table = oct7_coefficients_for_plot,
      plot_title = "Event Study Around 2023-10-07",
      plot_subtitle = "DV: log(1 + weekly spend). Baseline: relative_week = 0 (week starts 2023-10-08)"
    )

    ggplot2::ggsave(
      filename = file.path(output_directory, "event_study_figure_0710.png"),
      plot = oct7_plot,
      width = 9,
      height = 5,
      dpi = 300
    )
  }
}

# -------------------------
# Dedicated 07/10/2023 split by entity group
# -------------------------
oct7_group_specifications <- tibble::tribble(
  ~model_name, ~entity_group_filter,
  "oct7_all_entities", "all",
  "oct7_political_parties", "political_party",
  "oct7_other_orgs_people", "other_org_or_person"
)

oct7_group_results <- tibble::tibble()
oct7_group_coefficients <- tibble::tibble()
oct7_group_fit <- tibble::tibble()

if (nrow(oct7_event) == 1) {
  oct7_group_results <- oct7_group_specifications %>%
    dplyr::mutate(
      model_data = purrr::map(entity_group_filter, function(group_filter) {
        if (group_filter == "all") {
          oct7_event_window
        } else {
          oct7_event_window %>% dplyr::filter(entity_group == group_filter)
        }
      }),
      input_rows = purrr::map_int(model_data, nrow),
      model = purrr::map(model_data, run_event_study_model),
      coefficients = purrr::map2(model, model_name, extract_relative_week_coefficients),
      fit = purrr::pmap(list(model, model_name, input_rows), extract_model_fit)
    )

  oct7_group_coefficients <- dplyr::bind_rows(oct7_group_results$coefficients)
  oct7_group_fit <- dplyr::bind_rows(oct7_group_results$fit)

  oct7_group_coefficients_for_plot <- purrr::map_dfr(
    split(oct7_group_coefficients, oct7_group_coefficients$model_name),
    ~ add_baseline_coefficient_row(.x, unique(.x$model_name)[1])
  ) %>%
    dplyr::mutate(
      entity_group = dplyr::case_when(
        model_name == "oct7_all_entities" ~ "all_entities",
        model_name == "oct7_political_parties" ~ "political_party",
        model_name == "oct7_other_orgs_people" ~ "other_org_or_person",
        TRUE ~ model_name
      )
    )

  if (nrow(oct7_group_coefficients_for_plot) > 0) {
    oct7_group_plot <- ggplot2::ggplot(
      oct7_group_coefficients_for_plot,
      ggplot2::aes(x = relative_week, y = estimate, color = entity_group)
    ) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dotted") +
      ggplot2::geom_line() +
      ggplot2::geom_point(size = 2) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.12) +
      ggplot2::scale_x_continuous(breaks = seq(-analysis_window_weeks, analysis_window_weeks, by = 1)) +
      ggplot2::labs(
        title = "Event Study Around 2023-10-07 by Entity Group",
        subtitle = "DV: log(1 + weekly spend). Baseline: relative_week = 0 (week starts 2023-10-08)",
        x = "Weeks relative to event week",
        y = "Estimated change vs baseline week (95% CI)",
        color = "Entity group"
      ) +
      ggplot2::theme_minimal(base_size = 12)

    ggplot2::ggsave(
      filename = file.path(output_directory, "event_study_figure_0710_by_group.png"),
      plot = oct7_group_plot,
      width = 9,
      height = 5,
      dpi = 300
    )
  }
}

# -------------------------
# Save outputs
# -------------------------
write_clean_csv(overall_spend_stats, file.path(output_directory, "descriptive_overall.csv"))
write_clean_csv(spend_stats_by_group, file.path(output_directory, "descriptive_by_group.csv"))
write_clean_csv(yearly_spend_stats, file.path(output_directory, "descriptive_by_year.csv"))
write_clean_csv(yearly_spend_stats_by_group, file.path(output_directory, "descriptive_by_year_and_group.csv"))
write_clean_csv(pre_post_oct7_stats, file.path(output_directory, "descriptive_pre_post_oct7.csv"))
write_clean_csv(correlation_summary, file.path(output_directory, "correlation_summary.csv"))
write_clean_csv(placebo_events_table, file.path(output_directory, "placebo_events_dates.csv"))
write_clean_csv(placebo_correlation_summary, file.path(output_directory, "placebo_correlation_summary.csv"))

write_clean_csv(all_model_coefficients, file.path(output_directory, "event_study_coefficients_by_model.csv"))
write_clean_csv(all_model_fit, file.path(output_directory, "event_study_model_fit.csv"))
write_clean_csv(placebo_model_coefficients, file.path(output_directory, "placebo_event_study_coefficients_by_model.csv"))
write_clean_csv(placebo_model_fit, file.path(output_directory, "placebo_event_study_model_fit.csv"))

if (nrow(oct7_coefficients_for_plot) > 0) {
  write_clean_csv(oct7_coefficients_for_plot, file.path(output_directory, "event_study_coefs_0710.csv"))
}
if (nrow(oct7_group_coefficients) > 0) {
  write_clean_csv(oct7_group_coefficients, file.path(output_directory, "event_study_coefs_0710_by_group.csv"))
  write_clean_csv(oct7_group_coefficients, file.path(output_directory, "event_study_coefs_0710_all_party_org.csv"))
}
if (nrow(oct7_group_fit) > 0) {
  write_clean_csv(oct7_group_fit, file.path(output_directory, "event_study_model_fit_0710_by_group.csv"))
}

# Write textual summaries for easy reading in RStudio and externally
summary_file_path <- file.path(output_directory, "regression_summary.txt")
summary_connection <- file(summary_file_path, open = "wt")
on.exit(close(summary_connection), add = TRUE)

writeLines("Event-Study Regression Summary", con = summary_connection)
writeLines("================================", con = summary_connection)
writeLines(sprintf("Generated: %s", as.character(Sys.time())), con = summary_connection)
writeLines("", con = summary_connection)

writeLines("--- correlation_summary ---", con = summary_connection)
writeLines(capture.output(print(round_numeric_columns(correlation_summary))), con = summary_connection)
writeLines("", con = summary_connection)

writeLines("--- placebo_correlation_summary ---", con = summary_connection)
writeLines(capture.output(print(round_numeric_columns(placebo_correlation_summary))), con = summary_connection)
writeLines("", con = summary_connection)

for (row_index in seq_len(nrow(model_results))) {
  current_model_name <- model_results$model_name[[row_index]]
  current_model <- model_results$model[[row_index]]

  writeLines(sprintf("\n--- %s ---", current_model_name), con = summary_connection)

  if (is.null(current_model)) {
    writeLines("Model not estimated (failed or insufficient data).", con = summary_connection)
  } else {
    model_output <- capture.output(summary(current_model))
    writeLines(model_output, con = summary_connection)
  }
}

if (!is.null(oct7_model)) {
  writeLines("\n--- dedicated_oct7_model ---", con = summary_connection)
  writeLines(capture.output(summary(oct7_model)), con = summary_connection)
}

if (nrow(oct7_group_results) > 0) {
  for (row_index in seq_len(nrow(oct7_group_results))) {
    current_model_name <- oct7_group_results$model_name[[row_index]]
    current_model <- oct7_group_results$model[[row_index]]

    writeLines(sprintf("\n--- %s ---", current_model_name), con = summary_connection)

    if (is.null(current_model)) {
      writeLines("Model not estimated (failed or insufficient data).", con = summary_connection)
    } else {
      writeLines(capture.output(summary(current_model)), con = summary_connection)
    }
  }
}

# -------------------------
# Console output for RStudio users
# -------------------------
print_section("Descriptive Statistics (Overall)")
print(overall_spend_stats)

print_section("Descriptive Statistics (By Group)")
print(spend_stats_by_group)

print_section("Descriptive Statistics (By Year)")
print(yearly_spend_stats)

print_section("Pre/Post October 7 Reference Week")
print(pre_post_oct7_stats)

print_section("Correlation Summary")
print(correlation_summary)

print_section("Regression Model Fit (All Splits)")
print(all_model_fit)

if (nrow(oct7_group_fit) > 0) {
  print_section("October 7 Model Fit (By Group)")
  print(oct7_group_fit)
}

print_section("Output Files")
cat("Saved descriptive stats and regressions to: ", normalizePath(output_directory), "\n", sep = "")
cat("- descriptive_overall.csv\n")
cat("- descriptive_by_group.csv\n")
cat("- descriptive_by_year.csv\n")
cat("- descriptive_by_year_and_group.csv\n")
cat("- descriptive_pre_post_oct7.csv\n")
cat("- correlation_summary.csv\n")
cat("- correlation_coefficients_heatmap.png\n")
cat("- correlation_scatter_panels.png\n")
cat("- placebo_events_dates.csv\n")
cat("- placebo_correlation_summary.csv\n")
cat("- placebo_correlation_coefficients_heatmap.png\n")
cat("- placebo_correlation_scatter_panels.png\n")
cat("- event_study_coefficients_by_model.csv\n")
cat("- event_study_model_fit.csv\n")
cat("- placebo_event_study_coefficients_by_model.csv\n")
cat("- placebo_event_study_model_fit.csv\n")
cat("- regression_summary.txt\n")
cat("- event_study_figure_all_models.png\n")
cat("- event_study_figures/*.png\n")
cat("- placebo_event_study_figure_all_models.png\n")
cat("- placebo_event_study_figures/*.png\n")
if (nrow(oct7_coefficients_for_plot) > 0) {
  cat("- event_study_coefs_0710.csv\n")
  cat("- event_study_figure_0710.png\n")
}
if (nrow(oct7_group_coefficients) > 0) {
  cat("- event_study_coefs_0710_by_group.csv\n")
  cat("- event_study_coefs_0710_all_party_org.csv\n")
  cat("- event_study_model_fit_0710_by_group.csv\n")
  cat("- event_study_figure_0710_by_group.png\n")
}
