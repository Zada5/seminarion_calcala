rm(list = ls())
gc()
cat("\014")

# =====================================================
# DiD-Style Post-Event Regressions on Weekly Spending
# =====================================================
# Goal:
# 1) Reuse the cleaned weekly spend inputs from the main event-study script
# 2) Estimate a stacked post-vs-pre specification around events
# 3) Save outputs to a separate directory for review
#
# Requested core specification:
# log(1 + Spending[i,t]) = alpha(i) + beta * PostEvent(i,t) + u(i,t)
#
# When pooling multiple events, this script also adds event fixed effects so the
# post-event estimate is identified within each event window.
#
# Run in RStudio or command line:
# Rscript did_0710.R [google_csv] [meta_csv] [events_csv] [output_dir] [window_weeks]

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

format_output_column <- function(values, column_name, digits = 3L) {
  numeric_values <- as.numeric(Re(values))
  formatted_values <- rep(NA_character_, length(numeric_values))
  finite_values <- is.finite(numeric_values)
  p_value_threshold <- 10^-digits

  if (column_name == "p.value") {
    tiny_values <- finite_values & numeric_values < p_value_threshold
    regular_values <- finite_values & !tiny_values

    formatted_values[tiny_values] <- paste0(
      "<",
      formatC(p_value_threshold, format = "f", digits = digits)
    )
    formatted_values[regular_values] <- formatC(
      round(numeric_values[regular_values], digits = digits),
      format = "f",
      digits = digits
    )

    return(formatted_values)
  }

  if (any(finite_values)) {
    integer_like_column <- all(
      abs(numeric_values[finite_values] - round(numeric_values[finite_values])) <
        sqrt(.Machine$double.eps)
    )

    if (integer_like_column) {
      formatted_values[finite_values] <- formatC(
        round(numeric_values[finite_values]),
        format = "f",
        digits = 0
      )
    } else {
      formatted_values[finite_values] <- formatC(
        round(numeric_values[finite_values], digits = digits),
        format = "f",
        digits = digits
      )
    }
  }

  formatted_values
}

format_output_table <- function(dataframe, digits = 3L) {
  dataframe %>%
    dplyr::mutate(
      dplyr::across(
        .cols = where(is.numeric),
        .fns = ~ format_output_column(.x, dplyr::cur_column(), digits = digits)
      )
    )
}

write_clean_csv <- function(dataframe, file_path, digits = 3L) {
  readr::write_csv(format_output_table(dataframe, digits = digits), file_path, na = "")
}

write_formatted_table <- function(dataframe, summary_connection, digits = 3L) {
  if (nrow(dataframe) == 0) {
    writeLines("No rows.", con = summary_connection)
    return(invisible(NULL))
  }

  table_output <- capture.output(print(format_output_table(dataframe, digits = digits), n = Inf, width = Inf))
  table_output <- sub("[[:space:]]+$", "", table_output)
  writeLines(table_output, con = summary_connection)
  invisible(NULL)
}

write_model_summary_sections <- function(model_names, coefficients_table, fit_table, summary_connection) {
  for (current_model_name in model_names) {
    writeLines(sprintf("\n--- %s ---", current_model_name), con = summary_connection)

    current_coefficients <- if ("model_name" %in% names(coefficients_table)) {
      coefficients_table %>% dplyr::filter(model_name == current_model_name)
    } else {
      tibble::tibble()
    }
    current_fit <- if ("model_name" %in% names(fit_table)) {
      fit_table %>% dplyr::filter(model_name == current_model_name)
    } else {
      tibble::tibble()
    }

    writeLines("Coefficients:", con = summary_connection)
    write_formatted_table(current_coefficients, summary_connection)
    writeLines("Model fit:", con = summary_connection)
    write_formatted_table(current_fit, summary_connection)
  }
}

read_weekly_spend_file <- function(file_path) {
  dataset <- readr::read_csv(file_path, show_col_types = FALSE)
  names(dataset) <- trimws(names(dataset))
  dataset
}

run_did_model <- function(model_data) {
  if (nrow(model_data) < 10) {
    return(NULL)
  }
  if (dplyr::n_distinct(model_data$entity_name) < 2) {
    return(NULL)
  }
  if (dplyr::n_distinct(model_data$post_event) < 2) {
    return(NULL)
  }

  model_formula <- if (dplyr::n_distinct(model_data$event_id) > 1) {
    as.formula("log_weekly_spend ~ post_event | entity_name + data_source + event_id")
  } else {
    as.formula("log_weekly_spend ~ post_event | entity_name + data_source")
  }

  tryCatch(
    fixest::feols(model_formula, data = model_data, vcov = ~ entity_name),
    error = function(e) {
      message("Model failed: ", conditionMessage(e))
      NULL
    }
  )
}

extract_post_event_coefficient <- function(model_object, model_name_label) {
  if (is.null(model_object)) {
    return(tibble::tibble())
  }

  broom::tidy(model_object, conf.int = TRUE, conf.level = 0.95) %>%
    dplyr::filter(term == "post_event") %>%
    dplyr::mutate(model_name = model_name_label) %>%
    dplyr::select(
      model_name,
      term,
      estimate,
      std.error,
      statistic,
      p.value,
      conf.low,
      conf.high
    )
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

build_sample_summary <- function(model_data, model_name_label) {
  if (nrow(model_data) == 0) {
    return(tibble::tibble())
  }

  model_data %>%
    dplyr::mutate(
      period = dplyr::if_else(post_event == 1L, "post_event", "pre_event")
    ) %>%
    dplyr::group_by(period) %>%
    dplyr::summarise(
      model_name = model_name_label,
      rows = dplyr::n(),
      entities = dplyr::n_distinct(entity_name),
      events = dplyr::n_distinct(event_id),
      avg_weekly_spend_ils = mean(weekly_spend_ils, na.rm = TRUE),
      median_weekly_spend_ils = median(weekly_spend_ils, na.rm = TRUE),
      avg_log_weekly_spend = mean(log_weekly_spend, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::select(
      model_name,
      period,
      rows,
      entities,
      events,
      avg_weekly_spend_ils,
      median_weekly_spend_ils,
      avg_log_weekly_spend
    )
}

plot_did_coefficients <- function(coefficients_table, plot_title, plot_subtitle) {
  ggplot2::ggplot(
    coefficients_table,
    ggplot2::aes(x = reorder(model_name, estimate), y = estimate)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = conf.low, ymax = conf.high),
      width = 0.18
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = "Model",
      y = "Estimated post-event change (95% CI)"
    ) +
    ggplot2::theme_minimal(base_size = 12)
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
output_directory <- if (length(script_arguments) >= 4) script_arguments[4] else "./analysis_outputs_did"
analysis_window_weeks <- if (length(script_arguments) >= 5) as.integer(script_arguments[5]) else 2L

if (is.na(analysis_window_weeks) || analysis_window_weeks < 1L) {
  stop("window_weeks must be an integer >= 1")
}

dir.create(output_directory, showWarnings = FALSE, recursive = TRUE)

output_paths <- list(
  summaries = file.path(output_directory, "summaries"),
  post_from_0 = file.path(output_directory, "post_from_0"),
  post_from_minus1 = file.path(output_directory, "post_from_minus1"),
  oct7_post_from_0 = file.path(output_directory, "oct7", "post_from_0"),
  oct7_post_from_minus1 = file.path(output_directory, "oct7", "post_from_minus1")
)
invisible(lapply(output_paths, dir.create, showWarnings = FALSE, recursive = TRUE))

legacy_output_paths <- file.path(
  output_directory,
  c(
    "did_design_overview.csv",
    "did_sample_summary_by_model.csv",
    "did_coefficients_by_model.csv",
    "did_model_fit.csv",
    "did_coefficients_by_model.png",
    "did_design_overview_post_from_minus1.csv",
    "did_sample_summary_by_model_post_from_minus1.csv",
    "did_coefficients_by_model_post_from_minus1.csv",
    "did_model_fit_post_from_minus1.csv",
    "did_coefficients_by_model_post_from_minus1.png",
    "did_coefs_0710.csv",
    "did_sample_summary_0710.csv",
    "did_coefs_0710_by_group.csv",
    "did_model_fit_0710_by_group.csv",
    "did_sample_summary_0710_by_group.csv",
    "did_coefficients_0710_by_group.png",
    "did_coefs_0710_all_party_org.csv",
    "did_coefs_0710_post_from_minus1.csv",
    "did_sample_summary_0710_post_from_minus1.csv",
    "did_coefs_0710_by_group_post_from_minus1.csv",
    "did_model_fit_0710_by_group_post_from_minus1.csv",
    "did_sample_summary_0710_by_group_post_from_minus1.csv",
    "did_coefficients_0710_by_group_post_from_minus1.png",
    "did_regression_summary.txt"
  )
)
legacy_output_paths <- c(legacy_output_paths, file.path(output_paths$summaries, "did_regression_summary.txt"))
unlink(legacy_output_paths[file.exists(legacy_output_paths)], recursive = TRUE)

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
# Event-window panel for DiD-style regressions
# -------------------------
event_window_panel <- tidyr::crossing(
  weekly_spend_panel %>%
    dplyr::select(data_source, entity_name, entity_group, week_start_sunday, weekly_spend_ils),
  events_table %>%
    dplyr::select(event_id, event_date, event_week_start_sunday, event_type_group, event_name)
) %>%
  dplyr::mutate(
    relative_week = as.integer((week_start_sunday - event_week_start_sunday) / 7),
    post_event = dplyr::if_else(relative_week >= 0L, 1L, 0L),
    log_weekly_spend = log1p(weekly_spend_ils)
  ) %>%
  dplyr::filter(relative_week >= -analysis_window_weeks, relative_week <= analysis_window_weeks)

if (nrow(event_window_panel) == 0) {
  stop("No rows in event window panel. Check event dates and weekly dates.")
}

did_design_overview <- event_window_panel %>%
  dplyr::summarise(
    event_window_weeks = analysis_window_weeks,
    total_rows = dplyr::n(),
    total_entities = dplyr::n_distinct(entity_name),
    total_events = dplyr::n_distinct(event_id),
    pre_event_rows = sum(post_event == 0L),
    post_event_rows = sum(post_event == 1L),
    first_week = min(week_start_sunday, na.rm = TRUE),
    last_week = max(week_start_sunday, na.rm = TRUE)
  )

# -------------------------
# DiD-style model splits
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

fit_did_specs <- function(input_panel, specifications) {
  specifications %>%
    dplyr::mutate(
      model_data = purrr::map2(entity_group_filter, event_type_filter, function(group_filter, type_filter) {
        filtered_data <- input_panel

        if (group_filter != "all") {
          filtered_data <- filtered_data %>% dplyr::filter(entity_group == group_filter)
        }
        if (type_filter != "all") {
          filtered_data <- filtered_data %>% dplyr::filter(event_type_group == type_filter)
        }

        filtered_data
      }),
      input_rows = purrr::map_int(model_data, nrow),
      model = purrr::map(model_data, run_did_model),
      coefficient = purrr::map2(model, model_name, extract_post_event_coefficient),
      fit = purrr::pmap(list(model, model_name, input_rows), extract_model_fit),
      sample_summary = purrr::map2(model_data, model_name, build_sample_summary)
    )
}

event_window_panel_post_from_minus1 <- event_window_panel %>%
  dplyr::mutate(
    post_event = dplyr::if_else(relative_week >= -1L, 1L, 0L)
  )

did_design_overview_post_from_minus1 <- event_window_panel_post_from_minus1 %>%
  dplyr::summarise(
    event_window_weeks = analysis_window_weeks,
    total_rows = dplyr::n(),
    total_entities = dplyr::n_distinct(entity_name),
    total_events = dplyr::n_distinct(event_id),
    pre_event_rows = sum(post_event == 0L),
    post_event_rows = sum(post_event == 1L),
    first_week = min(week_start_sunday, na.rm = TRUE),
    last_week = max(week_start_sunday, na.rm = TRUE)
  )

model_results <- fit_did_specs(
  input_panel = event_window_panel,
  specifications = model_specifications
)

model_results_post_from_minus1 <- fit_did_specs(
  input_panel = event_window_panel_post_from_minus1,
  specifications = model_specifications
)

all_model_coefficients <- dplyr::bind_rows(model_results$coefficient)
all_model_fit <- dplyr::bind_rows(model_results$fit)
all_model_sample_summary <- dplyr::bind_rows(model_results$sample_summary)

all_model_coefficients_post_from_minus1 <- dplyr::bind_rows(model_results_post_from_minus1$coefficient)
all_model_fit_post_from_minus1 <- dplyr::bind_rows(model_results_post_from_minus1$fit)
all_model_sample_summary_post_from_minus1 <- dplyr::bind_rows(
  model_results_post_from_minus1$sample_summary
)

if (nrow(all_model_coefficients) > 0) {
  combined_did_plot <- plot_did_coefficients(
    coefficients_table = all_model_coefficients,
    plot_title = "DiD-Style Post-Event Estimates Across Model Splits",
    plot_subtitle = "DV: log(1 + weekly spend). PostEvent = 1 for relative_week >= 0"
  )

  ggplot2::ggsave(
    filename = file.path(output_paths$post_from_0, "did_coefficients_by_model.png"),
    plot = combined_did_plot,
    width = 11,
    height = 6.5,
    dpi = 300
  )
}

if (nrow(all_model_coefficients_post_from_minus1) > 0) {
  combined_did_post_from_minus1_plot <- plot_did_coefficients(
    coefficients_table = all_model_coefficients_post_from_minus1,
    plot_title = "DiD-Style Post-Event Estimates Across Model Splits",
    plot_subtitle = "DV: log(1 + weekly spend). PostEvent = 1 for relative_week >= -1"
  )

  ggplot2::ggsave(
    filename = file.path(output_paths$post_from_minus1, "did_coefficients_by_model.png"),
    plot = combined_did_post_from_minus1_plot,
    width = 11,
    height = 6.5,
    dpi = 300
  )
}

# -------------------------
# Dedicated 07/10/2023 DiD-style model
# -------------------------
oct7_event <- events_table %>%
  dplyr::filter(event_date == as.Date("2023-10-07")) %>%
  dplyr::slice_head(n = 1)

oct7_coefficient <- tibble::tibble()
oct7_model <- NULL
oct7_sample_summary <- tibble::tibble()
oct7_coefficient_post_from_minus1 <- tibble::tibble()
oct7_model_post_from_minus1 <- NULL
oct7_sample_summary_post_from_minus1 <- tibble::tibble()

if (nrow(oct7_event) == 1) {
  oct7_event_window <- event_window_panel %>%
    dplyr::filter(event_id == oct7_event$event_id)
  oct7_event_window_post_from_minus1 <- event_window_panel_post_from_minus1 %>%
    dplyr::filter(event_id == oct7_event$event_id)

  oct7_model <- run_did_model(oct7_event_window)
  oct7_coefficient <- extract_post_event_coefficient(oct7_model, "oct7_event")
  oct7_sample_summary <- build_sample_summary(oct7_event_window, "oct7_event")

  oct7_model_post_from_minus1 <- run_did_model(oct7_event_window_post_from_minus1)
  oct7_coefficient_post_from_minus1 <- extract_post_event_coefficient(
    oct7_model_post_from_minus1,
    "oct7_event"
  )
  oct7_sample_summary_post_from_minus1 <- build_sample_summary(
    oct7_event_window_post_from_minus1,
    "oct7_event"
  )
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
oct7_group_sample_summary <- tibble::tibble()
oct7_group_results_post_from_minus1 <- tibble::tibble()
oct7_group_coefficients_post_from_minus1 <- tibble::tibble()
oct7_group_fit_post_from_minus1 <- tibble::tibble()
oct7_group_sample_summary_post_from_minus1 <- tibble::tibble()

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
      model = purrr::map(model_data, run_did_model),
      coefficient = purrr::map2(model, model_name, extract_post_event_coefficient),
      fit = purrr::pmap(list(model, model_name, input_rows), extract_model_fit),
      sample_summary = purrr::map2(model_data, model_name, build_sample_summary)
    )

  oct7_group_coefficients <- dplyr::bind_rows(oct7_group_results$coefficient)
  oct7_group_fit <- dplyr::bind_rows(oct7_group_results$fit)
  oct7_group_sample_summary <- dplyr::bind_rows(oct7_group_results$sample_summary)

  if (nrow(oct7_group_coefficients) > 0) {
    oct7_group_plot <- plot_did_coefficients(
      coefficients_table = oct7_group_coefficients,
      plot_title = "DiD-Style Post-Event Estimates Around 2023-10-07",
      plot_subtitle = "DV: log(1 + weekly spend). Split by entity group"
    )

    ggplot2::ggsave(
      filename = file.path(output_paths$oct7_post_from_0, "did_coefficients_0710_by_group.png"),
      plot = oct7_group_plot,
      width = 8.5,
      height = 4.5,
      dpi = 300
    )
  }

  oct7_group_results_post_from_minus1 <- oct7_group_specifications %>%
    dplyr::mutate(
      model_data = purrr::map(entity_group_filter, function(group_filter) {
        if (group_filter == "all") {
          oct7_event_window_post_from_minus1
        } else {
          oct7_event_window_post_from_minus1 %>% dplyr::filter(entity_group == group_filter)
        }
      }),
      input_rows = purrr::map_int(model_data, nrow),
      model = purrr::map(model_data, run_did_model),
      coefficient = purrr::map2(model, model_name, extract_post_event_coefficient),
      fit = purrr::pmap(list(model, model_name, input_rows), extract_model_fit),
      sample_summary = purrr::map2(model_data, model_name, build_sample_summary)
    )

  oct7_group_coefficients_post_from_minus1 <- dplyr::bind_rows(
    oct7_group_results_post_from_minus1$coefficient
  )
  oct7_group_fit_post_from_minus1 <- dplyr::bind_rows(oct7_group_results_post_from_minus1$fit)
  oct7_group_sample_summary_post_from_minus1 <- dplyr::bind_rows(
    oct7_group_results_post_from_minus1$sample_summary
  )

  if (nrow(oct7_group_coefficients_post_from_minus1) > 0) {
    oct7_group_post_from_minus1_plot <- plot_did_coefficients(
      coefficients_table = oct7_group_coefficients_post_from_minus1,
      plot_title = "DiD-Style Post-Event Estimates Around 2023-10-07",
      plot_subtitle = "DV: log(1 + weekly spend). PostEvent = 1 for relative_week >= -1"
    )

    ggplot2::ggsave(
      filename = file.path(output_paths$oct7_post_from_minus1, "did_coefficients_0710_by_group.png"),
      plot = oct7_group_post_from_minus1_plot,
      width = 8.5,
      height = 4.5,
      dpi = 300
    )
  }
}

# -------------------------
# Save outputs
# -------------------------
write_clean_csv(did_design_overview, file.path(output_paths$post_from_0, "did_design_overview.csv"))
write_clean_csv(all_model_sample_summary, file.path(output_paths$post_from_0, "did_sample_summary_by_model.csv"))
write_clean_csv(all_model_coefficients, file.path(output_paths$post_from_0, "did_coefficients_by_model.csv"))
write_clean_csv(all_model_fit, file.path(output_paths$post_from_0, "did_model_fit.csv"))
write_clean_csv(
  did_design_overview_post_from_minus1,
  file.path(output_paths$post_from_minus1, "did_design_overview.csv")
)
write_clean_csv(
  all_model_sample_summary_post_from_minus1,
  file.path(output_paths$post_from_minus1, "did_sample_summary_by_model.csv")
)
write_clean_csv(
  all_model_coefficients_post_from_minus1,
  file.path(output_paths$post_from_minus1, "did_coefficients_by_model.csv")
)
write_clean_csv(
  all_model_fit_post_from_minus1,
  file.path(output_paths$post_from_minus1, "did_model_fit.csv")
)

if (nrow(oct7_coefficient) > 0) {
  write_clean_csv(oct7_coefficient, file.path(output_paths$oct7_post_from_0, "did_coefs_0710.csv"))
}
if (nrow(oct7_coefficient_post_from_minus1) > 0) {
  write_clean_csv(
    oct7_coefficient_post_from_minus1,
    file.path(output_paths$oct7_post_from_minus1, "did_coefs_0710.csv")
  )
}
if (nrow(oct7_sample_summary) > 0) {
  write_clean_csv(oct7_sample_summary, file.path(output_paths$oct7_post_from_0, "did_sample_summary_0710.csv"))
}
if (nrow(oct7_sample_summary_post_from_minus1) > 0) {
  write_clean_csv(
    oct7_sample_summary_post_from_minus1,
    file.path(output_paths$oct7_post_from_minus1, "did_sample_summary_0710.csv")
  )
}
if (nrow(oct7_group_coefficients) > 0) {
  write_clean_csv(oct7_group_coefficients, file.path(output_paths$oct7_post_from_0, "did_coefs_0710_by_group.csv"))
}
if (nrow(oct7_group_fit) > 0) {
  write_clean_csv(oct7_group_fit, file.path(output_paths$oct7_post_from_0, "did_model_fit_0710_by_group.csv"))
}
if (nrow(oct7_group_sample_summary) > 0) {
  write_clean_csv(oct7_group_sample_summary, file.path(output_paths$oct7_post_from_0, "did_sample_summary_0710_by_group.csv"))
}
if (nrow(oct7_group_coefficients_post_from_minus1) > 0) {
  write_clean_csv(
    oct7_group_coefficients_post_from_minus1,
    file.path(output_paths$oct7_post_from_minus1, "did_coefs_0710_by_group.csv")
  )
}
if (nrow(oct7_group_fit_post_from_minus1) > 0) {
  write_clean_csv(
    oct7_group_fit_post_from_minus1,
    file.path(output_paths$oct7_post_from_minus1, "did_model_fit_0710_by_group.csv")
  )
}
if (nrow(oct7_group_sample_summary_post_from_minus1) > 0) {
  write_clean_csv(
    oct7_group_sample_summary_post_from_minus1,
    file.path(output_paths$oct7_post_from_minus1, "did_sample_summary_0710_by_group.csv")
  )
}

summary_file_path <- file.path(output_paths$summaries, "regression_summary.txt")
summary_connection <- file(summary_file_path, open = "wt")
on.exit(close(summary_connection), add = TRUE)

writeLines("DiD-Style Post-Event Regression Summary", con = summary_connection)
writeLines("=======================================", con = summary_connection)
writeLines(sprintf("Generated: %s", as.character(Sys.time())), con = summary_connection)
writeLines(sprintf("Event window: +/- %s weeks", analysis_window_weeks), con = summary_connection)
writeLines("PostEvent definition: 1 if relative_week >= 0, else 0", con = summary_connection)
writeLines("", con = summary_connection)

writeLines("--- did_design_overview ---", con = summary_connection)
write_formatted_table(did_design_overview, summary_connection)
writeLines("", con = summary_connection)

writeLines("--- did_sample_summary_by_model ---", con = summary_connection)
write_formatted_table(all_model_sample_summary, summary_connection)
writeLines("", con = summary_connection)

writeLines("--- did_design_overview_post_from_minus1 ---", con = summary_connection)
write_formatted_table(did_design_overview_post_from_minus1, summary_connection)
writeLines("", con = summary_connection)

writeLines("--- did_sample_summary_by_model_post_from_minus1 ---", con = summary_connection)
write_formatted_table(all_model_sample_summary_post_from_minus1, summary_connection)
writeLines("", con = summary_connection)

writeLines("--- did_models_post_from_0 ---", con = summary_connection)
write_model_summary_sections(
  model_names = model_results$model_name,
  coefficients_table = all_model_coefficients,
  fit_table = all_model_fit,
  summary_connection = summary_connection
)

writeLines("\n--- did_models_post_from_minus1 ---", con = summary_connection)
write_model_summary_sections(
  model_names = model_results_post_from_minus1$model_name,
  coefficients_table = all_model_coefficients_post_from_minus1,
  fit_table = all_model_fit_post_from_minus1,
  summary_connection = summary_connection
)

if (nrow(oct7_coefficient) > 0) {
  writeLines("\n--- dedicated_oct7_model ---", con = summary_connection)
  write_formatted_table(oct7_coefficient, summary_connection)
}

if (nrow(oct7_coefficient_post_from_minus1) > 0) {
  writeLines("\n--- dedicated_oct7_model_post_from_minus1 ---", con = summary_connection)
  write_formatted_table(oct7_coefficient_post_from_minus1, summary_connection)
}

if (nrow(oct7_group_results) > 0) {
  writeLines("\n--- oct7_group_models ---", con = summary_connection)
  write_model_summary_sections(
    model_names = oct7_group_results$model_name,
    coefficients_table = oct7_group_coefficients,
    fit_table = oct7_group_fit,
    summary_connection = summary_connection
  )
}

if (nrow(oct7_group_results_post_from_minus1) > 0) {
  writeLines("\n--- oct7_group_models_post_from_minus1 ---", con = summary_connection)
  write_model_summary_sections(
    model_names = oct7_group_results_post_from_minus1$model_name,
    coefficients_table = oct7_group_coefficients_post_from_minus1,
    fit_table = oct7_group_fit_post_from_minus1,
    summary_connection = summary_connection
  )
}

# -------------------------
# Console output for RStudio users
# -------------------------
print_section("DiD Design Overview")
print(format_output_table(did_design_overview))

print_section("DiD Design Overview (Post From -1)")
print(format_output_table(did_design_overview_post_from_minus1))

print_section("DiD Sample Summary")
print(format_output_table(all_model_sample_summary))

print_section("DiD Model Fit")
print(format_output_table(all_model_fit))

if (nrow(all_model_coefficients) > 0) {
  print_section("DiD Coefficients")
  print(format_output_table(all_model_coefficients))
}

if (nrow(all_model_coefficients_post_from_minus1) > 0) {
  print_section("DiD Coefficients (Post From -1)")
  print(format_output_table(all_model_coefficients_post_from_minus1))
}

if (nrow(oct7_group_fit) > 0) {
  print_section("October 7 DiD Model Fit (By Group)")
  print(format_output_table(oct7_group_fit))
}

print_section("Output Files")
cat("Saved DiD-style outputs to: ", normalizePath(output_directory), "\n", sep = "")
cat("- summaries/regression_summary.txt\n")
cat("- post_from_0/*\n")
cat("- post_from_minus1/*\n")
if (nrow(all_model_coefficients) > 0) {
  cat("- post_from_0/did_coefficients_by_model.png\n")
}
if (nrow(all_model_coefficients_post_from_minus1) > 0) {
  cat("- post_from_minus1/did_coefficients_by_model.png\n")
}
if (nrow(oct7_coefficient) > 0) {
  cat("- oct7/post_from_0/did_coefs_0710.csv\n")
  cat("- oct7/post_from_0/did_sample_summary_0710.csv\n")
}
if (nrow(oct7_coefficient_post_from_minus1) > 0) {
  cat("- oct7/post_from_minus1/did_coefs_0710.csv\n")
  cat("- oct7/post_from_minus1/did_sample_summary_0710.csv\n")
}
if (nrow(oct7_group_coefficients) > 0) {
  cat("- oct7/post_from_0/did_coefs_0710_by_group.csv\n")
  cat("- oct7/post_from_0/did_model_fit_0710_by_group.csv\n")
  cat("- oct7/post_from_0/did_sample_summary_0710_by_group.csv\n")
  cat("- oct7/post_from_0/did_coefficients_0710_by_group.png\n")
}
if (nrow(oct7_group_coefficients_post_from_minus1) > 0) {
  cat("- oct7/post_from_minus1/did_coefs_0710_by_group.csv\n")
  cat("- oct7/post_from_minus1/did_model_fit_0710_by_group.csv\n")
  cat("- oct7/post_from_minus1/did_sample_summary_0710_by_group.csv\n")
  cat("- oct7/post_from_minus1/did_coefficients_0710_by_group.png\n")
}
