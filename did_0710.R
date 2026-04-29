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
# Effective specification used in this project:
#   log(Spending[i,t]) = alpha_i + beta * PostEvent[i,t] + u[i,t]
#
# alpha_i  -> entity FE (entity_name; data_source FE absorbs platform shifts)
# Multi-event (stacked) panels also add event_id FE.
#
# Note on gamma_t (calendar-week FE): the textbook canonical TWFE-DiD spec is
#   log(Spending[i,t]) = alpha_i + gamma_t + beta * PostEvent[i,t] + u[i,t]
# In our stacked design every entity in every event window is treated by that
# event; there is no never-treated control at the same calendar week. Inside
# each window PostEvent is a deterministic function of (event_id, calendar
# week), so adding week_start_sunday FE on top of event_id FE absorbs PostEvent
# entirely (beta -> ~1e-11, VCOV not positive semi-definite). Following standard
# practice in the stacked-DiD literature, we omit gamma_t for stacked specs.
# A dedicated demonstration regression that DOES include gamma_t is computed
# once in this script and saved under
#   analysis_outputs_did/gamma_t_demonstration/
# so the paper can show the broken output that motivated dropping gamma_t.
#
# Note on the dependent variable: the model uses log(Spending), not
# log(1 + Spending). Weeks with zero spend are filtered out before estimation
# (log(0) is undefined). This is a deliberate spec change from the earlier
# log1p() version; row counts dropped per panel are reported on stdout.
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

# Convert a log-scale coefficient to its multiplicative percent change.
# pct_change = (exp(estimate) - 1) * 100. Used because the dependent variable
# is log(weekly_spend_ils), so beta = -1.31 means exp(-1.31) - 1 = -73%, NOT
# -131% (which is impossible).
percent_change_from_log <- function(estimate_values) {
  ifelse(is.na(estimate_values), NA_real_, (exp(estimate_values) - 1) * 100)
}

significance_stars <- function(p_values) {
  dplyr::case_when(
    is.na(p_values) ~ "",
    p_values < 0.01 ~ "***",
    p_values < 0.05 ~ "**",
    p_values < 0.10 ~ "*",
    TRUE ~ ""
  )
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

  if (column_name == "pct_change") {
    formatted_values[finite_values] <- paste0(
      ifelse(numeric_values[finite_values] >= 0, "+", ""),
      formatC(numeric_values[finite_values], format = "f", digits = 1),
      "%"
    )
    return(formatted_values)
  }

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

.fmt_paper_number <- function(x, digits = 3L) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !is.finite(x)) return("")
  formatC(x, format = "f", digits = digits)
}

write_paper_style_model_section <- function(model_label,
                                            coefficients_table,
                                            fit_table,
                                            summary_connection,
                                            model_index = NULL) {
  rule_width <- 73L
  header <- if (is.null(model_index)) {
    sprintf("%s", model_label)
  } else {
    sprintf("(%d) %s", model_index, model_label)
  }
  writeLines("", con = summary_connection)
  writeLines(header, con = summary_connection)
  writeLines(strrep("-", rule_width), con = summary_connection)

  column_header <- sprintf(
    "%-22s %10s %10s %12s %9s %4s",
    "Variable", "Estimate", "Std. Err.", "Pct change", "p-value", "sig"
  )
  writeLines(column_header, con = summary_connection)

  if (is.null(coefficients_table) || nrow(coefficients_table) == 0) {
    writeLines("(no coefficients available -- model failed or insufficient data)",
               con = summary_connection)
  } else {
    for (row_index in seq_len(nrow(coefficients_table))) {
      row <- coefficients_table[row_index, ]

      variable_label <- if ("term" %in% names(row)) {
        as.character(row$term)
      } else {
        "(unknown)"
      }

      pct_text <- if (!is.null(row$pct_change) && is.finite(row$pct_change)) {
        sign_prefix <- if (row$pct_change >= 0) "+" else ""
        paste0(sign_prefix, formatC(row$pct_change, format = "f", digits = 1), "%")
      } else { "" }

      p_value_text <- if (is.finite(row$p.value)) {
        if (row$p.value < 0.001) "<0.001" else formatC(row$p.value, format = "f", digits = 3)
      } else { "" }

      signif_text <- if (is.null(row$signif) || is.na(row$signif)) "" else as.character(row$signif)

      writeLines(
        sprintf("%-22s %10s %10s %12s %9s %4s",
                variable_label,
                .fmt_paper_number(row$estimate, 3L),
                .fmt_paper_number(row$std.error, 3L),
                pct_text,
                p_value_text,
                signif_text),
        con = summary_connection
      )
    }
  }

  writeLines(strrep("-", rule_width), con = summary_connection)

  if (!is.null(fit_table) && nrow(fit_table) > 0) {
    fit_row <- fit_table[1, ]
    n_used_text <- if (is.numeric(fit_row$used_rows) && is.finite(fit_row$used_rows)) {
      format(round(fit_row$used_rows), big.mark = ",")
    } else { "NA" }
    writeLines(
      sprintf("N (used): %s    R^2: %s    Adj. R^2: %s    Within R^2: %s",
              n_used_text,
              .fmt_paper_number(fit_row$r2, 3L),
              .fmt_paper_number(fit_row$adjusted_r2, 3L),
              .fmt_paper_number(fit_row$within_r2, 3L)),
      con = summary_connection
    )
  }
  invisible(NULL)
}

write_paper_style_summary <- function(model_names,
                                      coefficients_table,
                                      fit_table,
                                      summary_connection) {
  for (model_index in seq_along(model_names)) {
    current_model_name <- model_names[model_index]

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

    write_paper_style_model_section(
      model_label = current_model_name,
      coefficients_table = current_coefficients,
      fit_table = current_fit,
      summary_connection = summary_connection,
      model_index = model_index
    )
  }
}

write_paper_style_header <- function(summary_connection,
                                     title,
                                     spec_lines,
                                     dependent_variable_note,
                                     extra_notes = character()) {
  rule_width <- 73L
  writeLines(strrep("=", rule_width), con = summary_connection)
  writeLines(title, con = summary_connection)
  writeLines(strrep("=", rule_width), con = summary_connection)
  writeLines(sprintf("Generated: %s", as.character(Sys.time())), con = summary_connection)
  writeLines("", con = summary_connection)

  writeLines("Specification", con = summary_connection)
  writeLines(strrep("-", rule_width), con = summary_connection)
  for (line in spec_lines) writeLines(line, con = summary_connection)
  writeLines("", con = summary_connection)

  writeLines("How to read the coefficients", con = summary_connection)
  writeLines(strrep("-", rule_width), con = summary_connection)
  writeLines("Estimates are reported in LOG-units (the dependent variable is log(spend)).", con = summary_connection)
  writeLines("A coefficient beta corresponds to a multiplicative effect of exp(beta) on", con = summary_connection)
  writeLines("weekly spend. The 'pct change' column is exactly (exp(beta) - 1) * 100, i.e.", con = summary_connection)
  writeLines("the percent change in weekly spend implied by the coefficient.", con = summary_connection)
  writeLines("", con = summary_connection)
  writeLines("Examples (so the magnitudes are unambiguous):", con = summary_connection)
  writeLines("    beta = +0.10  ->  +10.5%   (= exp(+0.10) - 1)", con = summary_connection)
  writeLines("    beta = -0.10  ->   -9.5%   (= exp(-0.10) - 1)", con = summary_connection)
  writeLines("    beta = -1.00  ->  -63.2%   (= exp(-1.00) - 1)", con = summary_connection)
  writeLines("    beta = -1.31  ->  -73.0%   (= exp(-1.31) - 1)", con = summary_connection)
  writeLines("Coefficients are NOT bounded by [-1, +1] -- a value of -1.31 means a 73%", con = summary_connection)
  writeLines("reduction in spend, not a 131% reduction (which would be impossible).", con = summary_connection)
  writeLines("", con = summary_connection)

  writeLines("Significance markers", con = summary_connection)
  writeLines(strrep("-", rule_width), con = summary_connection)
  writeLines("    ***  p < 0.01    **  p < 0.05    *  p < 0.10", con = summary_connection)
  writeLines("Standard errors clustered by entity_name. p-values < 0.001 are shown as", con = summary_connection)
  writeLines("'<0.001' rather than rounded to 0.000.", con = summary_connection)
  writeLines("", con = summary_connection)

  writeLines("Dependent variable / sample notes", con = summary_connection)
  writeLines(strrep("-", rule_width), con = summary_connection)
  for (line in c(dependent_variable_note, extra_notes)) {
    writeLines(line, con = summary_connection)
  }
  writeLines("", con = summary_connection)
}

read_weekly_spend_file <- function(file_path) {
  dataset <- readr::read_csv(file_path, show_col_types = FALSE)
  names(dataset) <- trimws(names(dataset))
  dataset
}

run_did_model <- function(model_data, include_gamma_t = FALSE) {
  if (nrow(model_data) < 10) {
    return(NULL)
  }
  if (dplyr::n_distinct(model_data$entity_name) < 2) {
    return(NULL)
  }
  if (dplyr::n_distinct(model_data$post_event) < 2) {
    return(NULL)
  }

  # Effective specification used in this project:
  #   log(Spending_{i,t}) = alpha_i + beta * PostEvent_{i,t} + u_{i,t}
  # alpha_i  -> entity_name FE (and data_source FE for platform-level shifts)
  # Multi-event (stacked) panels also add event_id FE.
  #
  # Why we DO NOT include gamma_t (calendar-week FE) in the stacked specs:
  # In our stacked design every entity in every event window is treated by that
  # event; there is no never-treated control group at the same calendar week.
  # Within each event window PostEvent = 1[relative_week >= 0] is a deterministic
  # function of (event_id, week_start_sunday), so adding week_start_sunday FE on
  # top of event_id FE absorbs PostEvent entirely: beta is driven to ~1e-11 and
  # fixest reports a 'VCOV matrix is not positive semi-definite' warning. The
  # 'with gamma_t' demonstration regression in this script reproduces that
  # breakdown for the paper.
  #
  # Single-event runs always drop gamma_t (PostEvent is a function of calendar
  # week alone within one event window, so calendar-week FE absorbs it).
  use_gamma_t <- include_gamma_t && dplyr::n_distinct(model_data$event_id) > 1

  fe_terms <- if (dplyr::n_distinct(model_data$event_id) > 1) {
    if (use_gamma_t) {
      "entity_name + data_source + event_id + week_start_sunday"
    } else {
      "entity_name + data_source + event_id"
    }
  } else {
    "entity_name + data_source"
  }

  model_formula <- as.formula(sprintf("log_weekly_spend ~ post_event | %s", fe_terms))

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
    dplyr::mutate(
      model_name = model_name_label,
      pct_change = percent_change_from_log(estimate),
      signif = significance_stars(p.value)
    ) %>%
    dplyr::select(
      model_name,
      term,
      estimate,
      std.error,
      statistic,
      p.value,
      conf.low,
      conf.high,
      pct_change,
      signif
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
  oct7_post_from_minus1 = file.path(output_directory, "oct7", "post_from_minus1"),
  gamma_t_demonstration = file.path(output_directory, "gamma_t_demonstration")
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
    post_event = dplyr::if_else(relative_week >= 0L, 1L, 0L)
  ) %>%
  dplyr::filter(relative_week >= -analysis_window_weeks, relative_week <= analysis_window_weeks)

# Dependent variable matches the agreed spec: log(Spending), not log(1 + Spending).
# log(0) is undefined, so weeks with zero spend are dropped before estimation.
event_window_zero_or_negative_rows <- sum(event_window_panel$weekly_spend_ils <= 0, na.rm = TRUE)
event_window_panel <- event_window_panel %>%
  dplyr::filter(weekly_spend_ils > 0) %>%
  dplyr::mutate(log_weekly_spend = log(weekly_spend_ils))

cat(sprintf(
  "DiD event-window panel: dropped %s rows with weekly_spend_ils <= 0 (log undefined).\n",
  format(event_window_zero_or_negative_rows, big.mark = ",")
))

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

# -------------------------
# gamma_t demonstration regression (kept for the seminar paper)
# -------------------------
# This intentionally fits the canonical TWFE DiD spec WITH calendar-week FE
# on the all_entities_all_events stacked panel:
#   log_y ~ post_event |
#       entity_name + data_source + event_id + week_start_sunday
# Preserved so the paper can show the broken output (beta absorbed to
# numerical zero, VCOV warning) that motivated dropping gamma_t for stacked
# DiD specs (Option 2 design decision).
gamma_t_demo_model <- run_did_model(event_window_panel, include_gamma_t = TRUE)
gamma_t_demo_coefficient <- extract_post_event_coefficient(
  gamma_t_demo_model,
  "all_entities_all_events__with_gamma_t"
)
gamma_t_demo_fit <- extract_model_fit(
  gamma_t_demo_model,
  "all_entities_all_events__with_gamma_t",
  nrow(event_window_panel)
)

gamma_t_baseline_compare <- all_model_coefficients %>%
  dplyr::filter(model_name == "all_entities_all_events") %>%
  dplyr::mutate(model_name = "all_entities_all_events__without_gamma_t (paper baseline)")

gamma_t_compare_table <- dplyr::bind_rows(
  gamma_t_baseline_compare,
  gamma_t_demo_coefficient
)

if (nrow(all_model_coefficients) > 0) {
  combined_did_plot <- plot_did_coefficients(
    coefficients_table = all_model_coefficients,
    plot_title = "DiD-Style Post-Event Estimates Across Model Splits",
    plot_subtitle = "DV: log(weekly spend). PostEvent = 1 for relative_week >= 0"
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
    plot_subtitle = "DV: log(weekly spend). PostEvent = 1 for relative_week >= -1"
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
      plot_subtitle = "DV: log(weekly spend). Split by entity group"
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
      plot_subtitle = "DV: log(weekly spend). PostEvent = 1 for relative_week >= -1"
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

# Save the gamma_t demonstration outputs (kept for the seminar paper).
write_clean_csv(
  gamma_t_demo_coefficient,
  file.path(output_paths$gamma_t_demonstration, "did_coefficient_with_gamma_t.csv")
)
write_clean_csv(
  gamma_t_demo_fit,
  file.path(output_paths$gamma_t_demonstration, "did_model_fit_with_gamma_t.csv")
)
write_clean_csv(
  gamma_t_compare_table,
  file.path(output_paths$gamma_t_demonstration, "did_coefficient_comparison.csv")
)
writeLines(
  c(
    "gamma_t demonstration -- DiD",
    "============================",
    "",
    "Panel: all_entities_all_events, +/- 2 weeks around every political/terror event.",
    "PostEvent = 1 when relative_week >= 0.",
    "",
    "Two regressions are reported here side-by-side in",
    "did_coefficient_comparison.csv:",
    "",
    "  (A) without_gamma_t  (the spec used in the rest of analysis_outputs_did/)",
    "      log_y ~ post_event | entity_name + data_source + event_id",
    "",
    "  (B) with_gamma_t     (the textbook canonical TWFE-DiD spec we tried first)",
    "      log_y ~ post_event |",
    "          entity_name + data_source + event_id + week_start_sunday",
    "",
    "What to point at in the paper:",
    "",
    "  * In (B) beta is driven to ~1e-11 (numerical zero) and fixest emits",
    "    'The VCOV matrix is not positive semi-definite and was fixed'.",
    "  * In (A) beta is meaningfully sized and its standard error is",
    "    well-behaved.",
    "",
    "Why this happens:",
    "",
    "  In our stacked DiD design every entity in every event window is",
    "  'treated' by that event; there is no never-treated control group at",
    "  the same calendar week. Inside each window PostEvent is a deterministic",
    "  function of (event_id, week_start_sunday). Once entity FE + event_id FE",
    "  + week FE are included, PostEvent has no within-FE variation and beta",
    "  is not identified. Following standard practice in the stacked-DiD",
    "  literature, the rest of this script omits gamma_t. The October 7",
    "  single-event DiD models drop gamma_t for a related stricter reason:",
    "  with one event, calendar week ↔ relative week one-to-one and",
    "  PostEvent is a function of calendar week alone.",
    "",
    "This folder is a record of the design decision and the empirical",
    "evidence behind it. Do not delete."
  ),
  con = file.path(output_paths$gamma_t_demonstration, "README.txt")
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

write_paper_style_header(
  summary_connection = summary_connection,
  title = "DiD-Style Post-Event Regression Summary",
  spec_lines = c(
    "log(Spending_{i,t}) = alpha_i + beta * PostEvent_{i,t} + u_{i,t}",
    "",
    "  alpha_i      : entity_name FE (and data_source FE for platform shifts)",
    "  PostEvent    : 1 if relative_week >= 0 within the +/-W event window, else 0",
    "                 (robustness panel also reports a PostEvent that switches on at",
    "                  relative_week = -1)",
    "  Multi-event (stacked) panels also add event_id FE.",
    "  Single-event runs (Oct 7) use entity_name + data_source FE only.",
    sprintf("  Event window: +/- %s weeks.", analysis_window_weeks),
    "  Standard errors clustered by entity_name."
  ),
  dependent_variable_note = "Dependent variable: log(weekly_spend_ils). Weeks with spend <= 0 are dropped (log undefined).",
  extra_notes = c(
    "Note on gamma_t: the textbook canonical spec adds calendar-week FE",
    "(gamma_t = week_start_sunday). In our stacked design every entity is in the",
    "+/-W window of every event, so PostEvent has no within-FE variation once",
    "gamma_t is added (beta -> ~1e-11, VCOV not positive semi-definite). Following",
    "standard stacked-DiD practice we omit gamma_t for stacked specs. A",
    "demonstration run with gamma_t included is preserved under",
    "gamma_t_demonstration/ for the paper."
  )
)

writeLines("Design overview (PostEvent = 1 from relative_week >= 0)", con = summary_connection)
writeLines(strrep("-", 73L), con = summary_connection)
write_formatted_table(did_design_overview, summary_connection)
writeLines("", con = summary_connection)

writeLines("Sample summary by model (PostEvent = 1 from relative_week >= 0)", con = summary_connection)
writeLines(strrep("-", 73L), con = summary_connection)
write_formatted_table(all_model_sample_summary, summary_connection)
writeLines("", con = summary_connection)

writeLines("Design overview (PostEvent = 1 from relative_week >= -1, robustness)", con = summary_connection)
writeLines(strrep("-", 73L), con = summary_connection)
write_formatted_table(did_design_overview_post_from_minus1, summary_connection)
writeLines("", con = summary_connection)

writeLines("Sample summary by model (PostEvent = 1 from relative_week >= -1, robustness)", con = summary_connection)
writeLines(strrep("-", 73L), con = summary_connection)
write_formatted_table(all_model_sample_summary_post_from_minus1, summary_connection)
writeLines("", con = summary_connection)

writeLines("", con = summary_connection)
writeLines(strrep("=", 73L), con = summary_connection)
writeLines("DiD models -- PostEvent = 1 from relative_week >= 0", con = summary_connection)
writeLines(strrep("=", 73L), con = summary_connection)
write_paper_style_summary(
  model_names = model_results$model_name,
  coefficients_table = all_model_coefficients,
  fit_table = all_model_fit,
  summary_connection = summary_connection
)

writeLines("", con = summary_connection)
writeLines(strrep("=", 73L), con = summary_connection)
writeLines("DiD models -- PostEvent = 1 from relative_week >= -1 (robustness)", con = summary_connection)
writeLines(strrep("=", 73L), con = summary_connection)
write_paper_style_summary(
  model_names = model_results_post_from_minus1$model_name,
  coefficients_table = all_model_coefficients_post_from_minus1,
  fit_table = all_model_fit_post_from_minus1,
  summary_connection = summary_connection
)

if (nrow(oct7_coefficient) > 0) {
  writeLines("", con = summary_connection)
  writeLines(strrep("=", 73L), con = summary_connection)
  writeLines("Dedicated October 7 DiD (single event) -- PostEvent from rel_week >= 0", con = summary_connection)
  writeLines(strrep("=", 73L), con = summary_connection)
  write_paper_style_model_section(
    model_label = "oct7_event",
    coefficients_table = oct7_coefficient,
    fit_table = if (!is.null(oct7_model)) extract_model_fit(oct7_model, "oct7_event", nrow(oct7_event_window)) else tibble::tibble(),
    summary_connection = summary_connection
  )
}

if (nrow(oct7_coefficient_post_from_minus1) > 0) {
  writeLines("", con = summary_connection)
  writeLines(strrep("=", 73L), con = summary_connection)
  writeLines("Dedicated October 7 DiD (single event) -- PostEvent from rel_week >= -1", con = summary_connection)
  writeLines(strrep("=", 73L), con = summary_connection)
  write_paper_style_model_section(
    model_label = "oct7_event",
    coefficients_table = oct7_coefficient_post_from_minus1,
    fit_table = if (!is.null(oct7_model_post_from_minus1)) extract_model_fit(oct7_model_post_from_minus1, "oct7_event", nrow(oct7_event_window_post_from_minus1)) else tibble::tibble(),
    summary_connection = summary_connection
  )
}

if (nrow(oct7_group_results) > 0) {
  writeLines("", con = summary_connection)
  writeLines(strrep("=", 73L), con = summary_connection)
  writeLines("October 7 DiD by entity group -- PostEvent from rel_week >= 0", con = summary_connection)
  writeLines(strrep("=", 73L), con = summary_connection)
  write_paper_style_summary(
    model_names = oct7_group_results$model_name,
    coefficients_table = oct7_group_coefficients,
    fit_table = oct7_group_fit,
    summary_connection = summary_connection
  )
}

if (nrow(oct7_group_results_post_from_minus1) > 0) {
  writeLines("", con = summary_connection)
  writeLines(strrep("=", 73L), con = summary_connection)
  writeLines("October 7 DiD by entity group -- PostEvent from rel_week >= -1 (robustness)", con = summary_connection)
  writeLines(strrep("=", 73L), con = summary_connection)
  write_paper_style_summary(
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
