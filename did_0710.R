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
#
# Script organization:
#   1. Shared helpers for parsing, formatting, regression output, and plotting
#   2. Input/output configuration and explicit 2020-2025 analysis window
#   3. Real and placebo stacked event-window panels
#   4. Main DiD, robustness, October 7, and gamma_t demonstration models
#   5. Reproducible output writing and concise console summary

required_packages <- c(
  "readr", "dplyr", "tidyr", "lubridate", "stringr",
  "ggplot2", "fixest", "broom", "purrr", "tibble"
)

# -------------------------
# Package setup
# -------------------------

install_missing_packages <- function(package_names) {
  missing_packages <- package_names[!sapply(package_names, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    install.packages(missing_packages, repos = "https://cloud.r-project.org")
  }
}

install_missing_packages(required_packages)
invisible(lapply(required_packages, library, character.only = TRUE))

# -------------------------
# General parsing/date helpers
# -------------------------

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

# -------------------------
# Output formatting helpers
# -------------------------

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

# -------------------------
# Paper-style text/table helpers
# -------------------------

.fmt_paper_number <- function(x, digits = 3L) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !is.finite(x)) {
    return("")
  }
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
      con = summary_connection
    )
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
      } else {
        ""
      }

      p_value_text <- if (is.finite(row$p.value)) {
        if (row$p.value < 0.001) "<0.001" else formatC(row$p.value, format = "f", digits = 3)
      } else {
        ""
      }

      signif_text <- if (is.null(row$signif) || is.na(row$signif)) "" else as.character(row$signif)

      writeLines(
        sprintf(
          "%-22s %10s %10s %12s %9s %4s",
          variable_label,
          .fmt_paper_number(row$estimate, 3L),
          .fmt_paper_number(row$std.error, 3L),
          pct_text,
          p_value_text,
          signif_text
        ),
        con = summary_connection
      )
    }
  }

  writeLines(strrep("-", rule_width), con = summary_connection)

  if (!is.null(fit_table) && nrow(fit_table) > 0) {
    fit_row <- fit_table[1, ]
    n_used_text <- if (is.numeric(fit_row$used_rows) && is.finite(fit_row$used_rows)) {
      format(round(fit_row$used_rows), big.mark = ",")
    } else {
      "NA"
    }
    writeLines(
      sprintf(
        "N (used): %s    R^2: %s    Adj. R^2: %s    Within R^2: %s",
        n_used_text,
        .fmt_paper_number(fit_row$r2, 3L),
        .fmt_paper_number(fit_row$adjusted_r2, 3L),
        .fmt_paper_number(fit_row$within_r2, 3L)
      ),
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

write_markdown_table <- function(dataframe, file_path, digits = 3L) {
  formatted_table <- format_output_table(dataframe, digits = digits) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = ~ dplyr::if_else(is.na(.x), "", as.character(.x))
      )
    )

  if (nrow(formatted_table) == 0) {
    writeLines("No rows.", con = file_path)
    return(invisible(NULL))
  }

  header_line <- paste0("| ", paste(names(formatted_table), collapse = " | "), " |")
  separator_line <- paste0("| ", paste(rep("---", ncol(formatted_table)), collapse = " | "), " |")
  row_lines <- apply(formatted_table, 1, function(row_values) {
    paste0("| ", paste(row_values, collapse = " | "), " |")
  })

  writeLines(c(header_line, separator_line, row_lines), con = file_path)
  invisible(NULL)
}

pretty_model_label <- function(model_name) {
  dplyr::case_when(
    model_name == "all_entities_all_events" ~ "All entities x all events",
    model_name == "political_parties_all_events" ~ "Political parties x all events",
    model_name == "other_orgs_people_all_events" ~ "Other orgs/people x all events",
    model_name == "all_entities_political_events" ~ "All entities x political events",
    model_name == "all_entities_terror_events" ~ "All entities x terror events",
    model_name == "political_parties_political_events" ~ "Political parties x political events",
    model_name == "political_parties_terror_events" ~ "Political parties x terror events",
    model_name == "other_orgs_people_political_events" ~ "Other orgs/people x political events",
    model_name == "other_orgs_people_terror_events" ~ "Other orgs/people x terror events",
    model_name == "oct7_all_entities" ~ "Oct 7 x all entities",
    model_name == "oct7_political_parties" ~ "Oct 7 x political parties",
    model_name == "oct7_other_orgs_people" ~ "Oct 7 x other orgs/people",
    TRUE ~ model_name
  )
}

# -------------------------
# Publication matrix helpers
# -------------------------

format_estimate_with_stars <- function(estimate, p_value, digits = 3L) {
  ifelse(
    is.na(estimate),
    NA_character_,
    paste0(formatC(round(estimate, digits = digits), format = "f", digits = digits), significance_stars(p_value))
  )
}

format_standard_error_for_paper <- function(std_error, digits = 3L) {
  ifelse(
    is.na(std_error),
    NA_character_,
    paste0("(", formatC(round(std_error, digits = digits), format = "f", digits = digits), ")")
  )
}

paper_entity_label <- function(entity_group_filter) {
  dplyr::case_when(
    entity_group_filter == "all" ~ "All entities",
    entity_group_filter == "political_party" ~ "Political parties",
    entity_group_filter == "other_org_or_person" ~ "Organizations/people",
    TRUE ~ entity_group_filter
  )
}

paper_event_label <- function(event_type_filter) {
  dplyr::case_when(
    event_type_filter == "all" ~ "All events",
    event_type_filter == "political" ~ "Political events",
    event_type_filter == "terror" ~ "Terror events",
    TRUE ~ event_type_filter
  )
}

build_did_key_results_matrix <- function(coefficients_table,
                                         model_fit_table,
                                         model_specifications_table) {
  entity_order <- c("All entities", "Political parties", "Organizations/people")
  event_order <- c("All events", "Political events", "Terror events")

  coefficients_compact <- coefficients_table %>%
    dplyr::mutate(
      estimate_display = format_estimate_with_stars(estimate, p.value),
      std_error_display = format_standard_error_for_paper(std.error)
    ) %>%
    dplyr::select(model_name, estimate_display, std_error_display, p.value)

  fit_compact <- model_fit_table %>%
    dplyr::select(model_name, used_rows, r2, within_r2)

  model_specifications_table %>%
    dplyr::mutate(
      entity_label = paper_entity_label(entity_group_filter),
      event_label = paper_event_label(event_type_filter)
    ) %>%
    dplyr::left_join(coefficients_compact, by = "model_name") %>%
    dplyr::left_join(fit_compact, by = "model_name") %>%
    dplyr::mutate(
      entity_label = factor(entity_label, levels = entity_order),
      event_label = factor(event_label, levels = event_order)
    ) %>%
    dplyr::arrange(entity_label, event_label)
}

latex_escape <- function(text_values) {
  text_values <- gsub("\\\\", "\\\\textbackslash{}", text_values)
  text_values <- gsub("([#$%&_{}])", "\\\\\\1", text_values, perl = TRUE)
  text_values
}

write_latex_matrix_table <- function(matrix_table,
                                     file_path,
                                     caption,
                                     label,
                                     coefficient_label,
                                     note_text) {
  entity_labels <- levels(matrix_table$entity_label)
  event_labels <- levels(matrix_table$event_label)

  get_cell <- function(entity_label, event_label, value_column) {
    value <- matrix_table %>%
      dplyr::filter(entity_label == !!entity_label, event_label == !!event_label) %>%
      dplyr::pull(.data[[value_column]])

    if (length(value) == 0 || is.na(value[1])) "" else as.character(value[1])
  }

  table_lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    paste0("\\caption{", latex_escape(caption), "}"),
    paste0("\\label{", label, "}"),
    "\\begin{tabular}{lccc}",
    "\\hline\\hline",
    paste0(" & ", paste(latex_escape(event_labels), collapse = " & "), " \\\\"),
    "\\hline"
  )

  for (entity_label in entity_labels) {
    estimate_cells <- vapply(
      event_labels,
      function(event_label) get_cell(entity_label, event_label, "estimate_display"),
      character(1)
    )
    se_cells <- vapply(
      event_labels,
      function(event_label) get_cell(entity_label, event_label, "std_error_display"),
      character(1)
    )
    table_lines <- c(
      table_lines,
      paste0(latex_escape(entity_label), " & ", paste(estimate_cells, collapse = " & "), " \\\\"),
      paste0(" & ", paste(se_cells, collapse = " & "), " \\\\")
    )
  }

  table_lines <- c(
    table_lines,
    "\\hline",
    paste0("\\multicolumn{4}{l}{\\footnotesize ", latex_escape(coefficient_label), "}\\\\"),
    paste0("\\multicolumn{4}{l}{\\footnotesize Notes: ", latex_escape(note_text), "}\\\\"),
    "\\hline\\hline",
    "\\end{tabular}",
    "\\end{table}"
  )

  writeLines(table_lines, con = file_path)
  invisible(NULL)
}

html_escape <- function(text_values) {
  text_values <- gsub("&", "&amp;", text_values, fixed = TRUE)
  text_values <- gsub("<", "&lt;", text_values, fixed = TRUE)
  text_values <- gsub(">", "&gt;", text_values, fixed = TRUE)
  text_values <- gsub('"', "&quot;", text_values, fixed = TRUE)
  text_values
}

write_html_matrix_table <- function(matrix_table,
                                    file_path,
                                    title,
                                    subtitle,
                                    coefficient_label,
                                    note_text) {
  entity_labels <- levels(matrix_table$entity_label)
  event_labels <- levels(matrix_table$event_label)

  get_row <- function(entity_label, event_label) {
    matrix_table %>%
      dplyr::filter(entity_label == !!entity_label, event_label == !!event_label) %>%
      dplyr::slice_head(n = 1)
  }

  body_lines <- unlist(lapply(entity_labels, function(entity_label) {
    cells <- unlist(lapply(event_labels, function(event_label) {
      row <- get_row(entity_label, event_label)
      estimate_text <- if (nrow(row) == 0 || is.na(row$estimate_display)) "" else row$estimate_display
      se_text <- if (nrow(row) == 0 || is.na(row$std_error_display)) "" else row$std_error_display
      paste0(
        "<td><div class=\"estimate\">", html_escape(estimate_text),
        "</div><div class=\"se\">", html_escape(se_text), "</div></td>"
      )
    }))

    paste0("<tr><th scope=\"row\">", html_escape(entity_label), "</th>", paste(cells, collapse = ""), "</tr>")
  }))

  html_lines <- c(
    "<!doctype html>",
    "<html lang=\"en\">",
    "<head>",
    "<meta charset=\"utf-8\">",
    "<style>",
    "body { font-family: Georgia, 'Times New Roman', serif; color: #111; margin: 36px; }",
    ".table-wrap { max-width: 920px; margin: 0 auto; }",
    "h1 { font-size: 18px; text-align: center; font-weight: 400; margin: 0 0 4px; }",
    ".subtitle { text-align: center; font-size: 13px; margin: 0 0 18px; }",
    "table { width: 100%; border-collapse: collapse; border-top: 2px solid #111; border-bottom: 2px solid #111; }",
    "thead th { border-bottom: 1px solid #111; font-weight: 600; padding: 8px 10px; text-align: center; }",
    "tbody th { text-align: left; font-weight: 400; padding: 8px 10px; }",
    "td { text-align: center; padding: 8px 10px; vertical-align: top; }",
    ".estimate { font-variant-numeric: tabular-nums; }",
    ".se { font-size: 12px; margin-top: 2px; font-variant-numeric: tabular-nums; }",
    ".note { font-size: 12px; line-height: 1.35; margin-top: 12px; }",
    "</style>",
    "</head>",
    "<body>",
    "<div class=\"table-wrap\">",
    paste0("<h1>", html_escape(title), "</h1>"),
    paste0("<p class=\"subtitle\">", html_escape(subtitle), "</p>"),
    "<table>",
    "<thead>",
    paste0("<tr><th></th>", paste0("<th>", html_escape(event_labels), "</th>", collapse = ""), "</tr>"),
    "</thead>",
    "<tbody>",
    body_lines,
    "</tbody>",
    "</table>",
    paste0("<p class=\"note\"><em>", html_escape(coefficient_label), "</em></p>"),
    paste0("<p class=\"note\"><em>Notes:</em> ", html_escape(note_text), "</p>"),
    "</div>",
    "</body>",
    "</html>"
  )

  writeLines(html_lines, con = file_path)
  invisible(NULL)
}

# -------------------------
# DiD publication/comparison table helpers
# -------------------------

build_did_publication_table <- function(
  coefficients_table,
  fit_table,
  sample_summary_table,
  model_order,
  post_event_definition_label
) {
  coefficient_summary <- coefficients_table %>%
    dplyr::mutate(
      estimate_display = format_estimate_with_stars(estimate, p.value),
      std_error_display = ifelse(
        is.na(std.error),
        NA_character_,
        paste0("(", formatC(round(std.error, digits = 3L), format = "f", digits = 3L), ")")
      )
    ) %>%
    dplyr::select(model_name, estimate_display, std_error_display, p.value)

  sample_summary_compact <- sample_summary_table %>%
    dplyr::group_by(model_name) %>%
    dplyr::summarise(
      entities = max(entities, na.rm = TRUE),
      events = max(events, na.rm = TRUE),
      .groups = "drop"
    )

  model_summary <- tibble::tibble(model_name = model_order) %>%
    dplyr::left_join(coefficient_summary, by = "model_name") %>%
    dplyr::left_join(fit_table, by = "model_name") %>%
    dplyr::left_join(sample_summary_compact, by = "model_name") %>%
    dplyr::mutate(
      column_label = pretty_model_label(model_name)
    )

  row_definitions <- tibble::tribble(
    ~row_label, ~value_column,
    "PostEvent", "estimate_display",
    "Std. Error", "std_error_display",
    "P-value", "p.value",
    "Observations", "used_rows",
    "Entities", "entities",
    "Events", "events",
    "R-squared", "r2",
    "Within R-squared", "within_r2"
  )

  table_rows <- purrr::map_dfr(seq_len(nrow(row_definitions)), function(row_index) {
    value_column <- row_definitions$value_column[[row_index]]
    raw_values <- model_summary[[value_column]]
    formatted_values <- if (is.numeric(raw_values)) {
      format_output_column(raw_values, value_column, digits = 3L)
    } else {
      as.character(raw_values)
    }

    tibble::tibble(
      row_label = row_definitions$row_label[[row_index]],
      column_label = model_summary$column_label,
      value = formatted_values
    )
  })

  metadata_rows <- tibble::tribble(
    ~row_label, ~value,
    "Entity fixed effects", "Yes",
    "Data-source fixed effects", "Yes",
    "Event fixed effects", "Yes",
    "Event window", paste0("+/- ", analysis_window_weeks, " weeks"),
    "PostEvent definition", post_event_definition_label
  ) %>%
    tidyr::crossing(column_label = model_summary$column_label) %>%
    dplyr::select(row_label, column_label, value)

  dplyr::bind_rows(table_rows, metadata_rows) %>%
    tidyr::pivot_wider(names_from = column_label, values_from = value) %>%
    dplyr::rename(Statistic = row_label)
}

build_did_comparison_table <- function(
  real_coefficients,
  real_fit,
  real_sample_summary,
  placebo_coefficients,
  placebo_fit,
  placebo_sample_summary
) {
  sample_summary_compact <- function(sample_summary_table) {
    sample_summary_table %>%
      dplyr::group_by(model_name) %>%
      dplyr::summarise(
        entities = max(entities, na.rm = TRUE),
        events = max(events, na.rm = TRUE),
        .groups = "drop"
      )
  }

  real_sample <- sample_summary_compact(real_sample_summary) %>%
    dplyr::rename(real_entities = entities, real_events = events)
  placebo_sample <- sample_summary_compact(placebo_sample_summary) %>%
    dplyr::rename(placebo_entities = entities, placebo_events = events)

  tibble::tibble(model_name = model_specifications$model_name) %>%
    dplyr::mutate(model_label = pretty_model_label(model_name)) %>%
    dplyr::left_join(
      real_coefficients %>%
        dplyr::rename_with(~ paste0("real_", .x), c(estimate, std.error, p.value, conf.low, conf.high)),
      by = "model_name"
    ) %>%
    dplyr::left_join(
      placebo_coefficients %>%
        dplyr::rename_with(~ paste0("placebo_", .x), c(estimate, std.error, p.value, conf.low, conf.high)),
      by = "model_name"
    ) %>%
    dplyr::left_join(
      real_fit %>%
        dplyr::select(model_name, used_rows, r2, within_r2) %>%
        dplyr::rename(
          real_used_rows = used_rows,
          real_r2 = r2,
          real_within_r2 = within_r2
        ),
      by = "model_name"
    ) %>%
    dplyr::left_join(
      placebo_fit %>%
        dplyr::select(model_name, used_rows, r2, within_r2) %>%
        dplyr::rename(
          placebo_used_rows = used_rows,
          placebo_r2 = r2,
          placebo_within_r2 = within_r2
        ),
      by = "model_name"
    ) %>%
    dplyr::left_join(real_sample, by = "model_name") %>%
    dplyr::left_join(placebo_sample, by = "model_name") %>%
    dplyr::mutate(
      real_estimate_display = format_estimate_with_stars(real_estimate, real_p.value),
      placebo_estimate_display = format_estimate_with_stars(placebo_estimate, placebo_p.value)
    ) %>%
    dplyr::select(
      model_name,
      model_label,
      real_estimate_display,
      real_std.error,
      real_p.value,
      real_conf.low,
      real_conf.high,
      real_used_rows,
      real_entities,
      real_events,
      real_within_r2,
      placebo_estimate_display,
      placebo_std.error,
      placebo_p.value,
      placebo_conf.low,
      placebo_conf.high,
      placebo_used_rows,
      placebo_entities,
      placebo_events,
      placebo_within_r2
    )
}

# -------------------------
# Placebo/input helpers
# -------------------------

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

load_placebo_events_from_repo <- function(
  placebo_file_path,
  real_events_table,
  min_gap_weeks = 3L
) {
  if (!file.exists(placebo_file_path)) {
    return(NULL)
  }

  placebo_file <- readr::read_csv(placebo_file_path, show_col_types = FALSE)
  names(placebo_file) <- trimws(names(placebo_file))

  required_placebo_columns <- c("event_date", "event_type_group")
  missing_columns <- setdiff(required_placebo_columns, names(placebo_file))
  if (length(missing_columns) > 0) {
    stop(
      "Placebo file missing columns: ",
      paste(missing_columns, collapse = ", "),
      ". File: ",
      placebo_file_path
    )
  }

  placebo_events <- placebo_file %>%
    dplyr::transmute(
      event_date = parse_date_flexible(event_date),
      event_type_group = as.character(event_type_group)
    ) %>%
    dplyr::filter(!is.na(event_date), event_type_group %in% c("political", "terror")) %>%
    dplyr::arrange(event_date)

  if (nrow(placebo_events) == 0) {
    stop("Placebo file has no valid rows after parsing: ", placebo_file_path)
  }

  placebo_events <- placebo_events %>%
    dplyr::mutate(
      event_week_start_sunday = event_date,
      event_type_raw = paste0("placebo_", event_type_group),
      event_name = paste0("placebo_event_", dplyr::row_number()),
      event_details = "Predefined placebo week from repository list",
      event_id = dplyr::row_number()
    ) %>%
    dplyr::select(
      event_date,
      event_type_raw,
      event_type_group,
      event_name,
      event_details,
      event_id,
      event_week_start_sunday
    )

  real_event_weeks <- unique(real_events_table$event_week_start_sunday)
  minimum_gap <- min(sapply(placebo_events$event_week_start_sunday, function(candidate_week) {
    min(abs(as.integer(candidate_week - real_event_weeks) / 7))
  }))

  if (minimum_gap <= min_gap_weeks) {
    stop(
      "Placebo list violates minimum gap from real events. Min gap observed: ",
      minimum_gap,
      " weeks, required: > ",
      min_gap_weeks,
      " weeks."
    )
  }

  placebo_events
}

read_weekly_spend_file <- function(file_path) {
  dataset <- readr::read_csv(file_path, show_col_types = FALSE)
  names(dataset) <- trimws(names(dataset))
  dataset
}

# -------------------------
# DiD model and plotting helpers
# -------------------------

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

analysis_start_week <- as.Date("2020-01-05")
analysis_end_week <- as.Date("2025-12-28")
analysis_start_date <- as.Date("2020-01-01")
analysis_end_date <- as.Date("2025-12-31")

dir.create(output_directory, showWarnings = FALSE, recursive = TRUE)

output_paths <- list(
  summaries = file.path(output_directory, "summaries"),
  tables = file.path(output_directory, "tables"),
  post_from_0 = file.path(output_directory, "post_from_0"),
  post_from_minus1 = file.path(output_directory, "post_from_minus1"),
  placebo_post_from_0 = file.path(output_directory, "placebo", "post_from_0"),
  placebo_post_from_minus1 = file.path(output_directory, "placebo", "post_from_minus1"),
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
cat("Analysis weeks    : ", as.character(analysis_start_week), " to ", as.character(analysis_end_week), "\n", sep = "")

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
  dplyr::filter(week_start_sunday >= analysis_start_week, week_start_sunday <= analysis_end_week) %>%
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
  ) %>%
  dplyr::filter(
    event_date >= analysis_start_date,
    event_date <= analysis_end_date,
    event_week_start_sunday >= analysis_start_week,
    event_week_start_sunday <= analysis_end_week
  ) %>%
  dplyr::mutate(event_id = dplyr::row_number())

if (nrow(events_table) == 0) {
  stop("No valid political/terror events found in events file.")
}

repo_placebo_dates_path <- "./placebo_events_2020_2025.csv"
placebo_window_start <- as.Date("2020-01-05")
placebo_window_end <- as.Date("2025-12-28")
placebo_events_table <- load_placebo_events_from_repo(
  placebo_file_path = repo_placebo_dates_path,
  real_events_table = events_table,
  min_gap_weeks = analysis_window_weeks + 1L
)

if (is.null(placebo_events_table)) {
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

placebo_event_window_panel <- tidyr::crossing(
  weekly_spend_panel %>%
    dplyr::select(data_source, entity_name, entity_group, week_start_sunday, weekly_spend_ils),
  placebo_events_table %>%
    dplyr::select(event_id, event_date, event_week_start_sunday, event_type_group, event_name)
) %>%
  dplyr::mutate(
    relative_week = as.integer((week_start_sunday - event_week_start_sunday) / 7),
    post_event = dplyr::if_else(relative_week >= 0L, 1L, 0L),
    log_weekly_spend = log1p(weekly_spend_ils)
  ) %>%
  dplyr::filter(relative_week >= -analysis_window_weeks, relative_week <= analysis_window_weeks)

placebo_did_design_overview <- placebo_event_window_panel %>%
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

placebo_event_window_panel_post_from_minus1 <- placebo_event_window_panel %>%
  dplyr::mutate(
    post_event = dplyr::if_else(relative_week >= -1L, 1L, 0L)
  )

placebo_did_design_overview_post_from_minus1 <- placebo_event_window_panel_post_from_minus1 %>%
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

placebo_model_results <- fit_did_specs(
  input_panel = placebo_event_window_panel,
  specifications = model_specifications
)

placebo_model_results_post_from_minus1 <- fit_did_specs(
  input_panel = placebo_event_window_panel_post_from_minus1,
  specifications = model_specifications
)

placebo_model_coefficients <- dplyr::bind_rows(placebo_model_results$coefficient)
placebo_model_fit <- dplyr::bind_rows(placebo_model_results$fit)
placebo_model_sample_summary <- dplyr::bind_rows(placebo_model_results$sample_summary)

placebo_model_coefficients_post_from_minus1 <- dplyr::bind_rows(
  placebo_model_results_post_from_minus1$coefficient
)
placebo_model_fit_post_from_minus1 <- dplyr::bind_rows(placebo_model_results_post_from_minus1$fit)
placebo_model_sample_summary_post_from_minus1 <- dplyr::bind_rows(
  placebo_model_results_post_from_minus1$sample_summary
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

if (nrow(placebo_model_coefficients) > 0) {
  placebo_combined_did_plot <- plot_did_coefficients(
    coefficients_table = placebo_model_coefficients,
    plot_title = "Placebo DiD-Style Post-Event Estimates Across Model Splits",
    plot_subtitle = "DV: log(1 + weekly spend). PostEvent = 1 for placebo relative_week >= 0"
  )

  ggplot2::ggsave(
    filename = file.path(output_paths$placebo_post_from_0, "did_coefficients_by_model.png"),
    plot = placebo_combined_did_plot,
    width = 11,
    height = 6.5,
    dpi = 300
  )
}

if (nrow(placebo_model_coefficients_post_from_minus1) > 0) {
  placebo_combined_did_post_from_minus1_plot <- plot_did_coefficients(
    coefficients_table = placebo_model_coefficients_post_from_minus1,
    plot_title = "Placebo DiD-Style Post-Event Estimates Across Model Splits",
    plot_subtitle = "DV: log(1 + weekly spend). PostEvent = 1 for placebo relative_week >= -1"
  )

  ggplot2::ggsave(
    filename = file.path(output_paths$placebo_post_from_minus1, "did_coefficients_by_model.png"),
    plot = placebo_combined_did_post_from_minus1_plot,
    width = 11,
    height = 6.5,
    dpi = 300
  )
}

did_paper_table_post_from_0 <- build_did_publication_table(
  coefficients_table = all_model_coefficients,
  fit_table = all_model_fit,
  sample_summary_table = all_model_sample_summary,
  model_order = model_specifications$model_name,
  post_event_definition_label = "1 if relative_week >= 0"
)

did_paper_table_post_from_minus1 <- build_did_publication_table(
  coefficients_table = all_model_coefficients_post_from_minus1,
  fit_table = all_model_fit_post_from_minus1,
  sample_summary_table = all_model_sample_summary_post_from_minus1,
  model_order = model_specifications$model_name,
  post_event_definition_label = "1 if relative_week >= -1"
)

placebo_did_paper_table_post_from_0 <- build_did_publication_table(
  coefficients_table = placebo_model_coefficients,
  fit_table = placebo_model_fit,
  sample_summary_table = placebo_model_sample_summary,
  model_order = model_specifications$model_name,
  post_event_definition_label = "1 if placebo relative_week >= 0"
)

placebo_did_paper_table_post_from_minus1 <- build_did_publication_table(
  coefficients_table = placebo_model_coefficients_post_from_minus1,
  fit_table = placebo_model_fit_post_from_minus1,
  sample_summary_table = placebo_model_sample_summary_post_from_minus1,
  model_order = model_specifications$model_name,
  post_event_definition_label = "1 if placebo relative_week >= -1"
)

did_comparison_post_from_0 <- build_did_comparison_table(
  real_coefficients = all_model_coefficients,
  real_fit = all_model_fit,
  real_sample_summary = all_model_sample_summary,
  placebo_coefficients = placebo_model_coefficients,
  placebo_fit = placebo_model_fit,
  placebo_sample_summary = placebo_model_sample_summary
)

did_comparison_post_from_minus1 <- build_did_comparison_table(
  real_coefficients = all_model_coefficients_post_from_minus1,
  real_fit = all_model_fit_post_from_minus1,
  real_sample_summary = all_model_sample_summary_post_from_minus1,
  placebo_coefficients = placebo_model_coefficients_post_from_minus1,
  placebo_fit = placebo_model_fit_post_from_minus1,
  placebo_sample_summary = placebo_model_sample_summary_post_from_minus1
)

did_key_results_post_from_0 <- build_did_key_results_matrix(
  coefficients_table = all_model_coefficients,
  model_fit_table = all_model_fit,
  model_specifications_table = model_specifications
)

did_key_results_post_from_minus1 <- build_did_key_results_matrix(
  coefficients_table = all_model_coefficients_post_from_minus1,
  model_fit_table = all_model_fit_post_from_minus1,
  model_specifications_table = model_specifications
)

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
write_clean_csv(placebo_events_table, file.path(output_paths$placebo_post_from_0, "placebo_events_dates.csv"))
write_clean_csv(placebo_did_design_overview, file.path(output_paths$placebo_post_from_0, "did_design_overview.csv"))
write_clean_csv(placebo_model_sample_summary, file.path(output_paths$placebo_post_from_0, "did_sample_summary_by_model.csv"))
write_clean_csv(placebo_model_coefficients, file.path(output_paths$placebo_post_from_0, "did_coefficients_by_model.csv"))
write_clean_csv(placebo_model_fit, file.path(output_paths$placebo_post_from_0, "did_model_fit.csv"))
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
write_clean_csv(
  placebo_did_design_overview_post_from_minus1,
  file.path(output_paths$placebo_post_from_minus1, "did_design_overview.csv")
)
write_clean_csv(
  placebo_model_sample_summary_post_from_minus1,
  file.path(output_paths$placebo_post_from_minus1, "did_sample_summary_by_model.csv")
)
write_clean_csv(
  placebo_model_coefficients_post_from_minus1,
  file.path(output_paths$placebo_post_from_minus1, "did_coefficients_by_model.csv")
)
write_clean_csv(
  placebo_model_fit_post_from_minus1,
  file.path(output_paths$placebo_post_from_minus1, "did_model_fit.csv")
)
write_clean_csv(did_paper_table_post_from_0, file.path(output_paths$tables, "did_paper_table_post_from_0.csv"))
write_markdown_table(did_paper_table_post_from_0, file.path(output_paths$tables, "did_paper_table_post_from_0.md"))
write_clean_csv(did_paper_table_post_from_minus1, file.path(output_paths$tables, "did_paper_table_post_from_minus1.csv"))
write_markdown_table(
  did_paper_table_post_from_minus1,
  file.path(output_paths$tables, "did_paper_table_post_from_minus1.md")
)
write_clean_csv(did_key_results_post_from_0, file.path(output_paths$tables, "did_key_results_post_from_0.csv"))
write_latex_matrix_table(
  matrix_table = did_key_results_post_from_0,
  file_path = file.path(output_paths$tables, "did_key_results_post_from_0.tex"),
  caption = "Difference-in-differences estimates by entity group and event type",
  label = "tab:did_key_results_post_from_0",
  coefficient_label = "Cells report the PostEvent coefficient; clustered standard errors are in parentheses.",
  note_text = paste0(
    "Dependent variable is log weekly spending. PostEvent equals 1 for relative week >= 0. ",
    "All models include entity and data-source fixed effects; stacked models also include event fixed effects. ",
    "Sample is restricted to Sunday-start weeks from ", analysis_start_week, " through ", analysis_end_week, ". ",
    "*** p < 0.01, ** p < 0.05, * p < 0.10."
  )
)
write_html_matrix_table(
  matrix_table = did_key_results_post_from_0,
  file_path = file.path(output_paths$tables, "did_key_results_post_from_0.html"),
  title = "Difference-in-Differences Estimates by Entity Group and Event Type",
  subtitle = "PostEvent coefficient; standard errors in parentheses",
  coefficient_label = "Cells report the PostEvent coefficient.",
  note_text = paste0(
    "Dependent variable is log weekly spending. PostEvent equals 1 for relative week >= 0. ",
    "All models include entity and data-source fixed effects; stacked models also include event fixed effects. ",
    "Sample is restricted to Sunday-start weeks from ", analysis_start_week, " through ", analysis_end_week, ". ",
    "*** p < 0.01, ** p < 0.05, * p < 0.10."
  )
)
write_clean_csv(
  did_key_results_post_from_minus1,
  file.path(output_paths$tables, "did_key_results_post_from_minus1.csv")
)
write_latex_matrix_table(
  matrix_table = did_key_results_post_from_minus1,
  file_path = file.path(output_paths$tables, "did_key_results_post_from_minus1.tex"),
  caption = "Difference-in-differences estimates by entity group and event type: post from -1",
  label = "tab:did_key_results_post_from_minus1",
  coefficient_label = "Cells report the PostEvent coefficient; clustered standard errors are in parentheses.",
  note_text = paste0(
    "Dependent variable is log weekly spending. PostEvent equals 1 for relative week >= -1. ",
    "All models include entity and data-source fixed effects; stacked models also include event fixed effects. ",
    "Sample is restricted to Sunday-start weeks from ", analysis_start_week, " through ", analysis_end_week, ". ",
    "*** p < 0.01, ** p < 0.05, * p < 0.10."
  )
)
write_html_matrix_table(
  matrix_table = did_key_results_post_from_minus1,
  file_path = file.path(output_paths$tables, "did_key_results_post_from_minus1.html"),
  title = "Difference-in-Differences Estimates by Entity Group and Event Type",
  subtitle = "Robustness: PostEvent equals 1 from relative week -1",
  coefficient_label = "Cells report the PostEvent coefficient.",
  note_text = paste0(
    "Dependent variable is log weekly spending. PostEvent equals 1 for relative week >= -1. ",
    "All models include entity and data-source fixed effects; stacked models also include event fixed effects. ",
    "Sample is restricted to Sunday-start weeks from ", analysis_start_week, " through ", analysis_end_week, ". ",
    "*** p < 0.01, ** p < 0.05, * p < 0.10."
  )
)
write_clean_csv(
  placebo_did_paper_table_post_from_0,
  file.path(output_paths$tables, "placebo_did_paper_table_post_from_0.csv")
)
write_markdown_table(
  placebo_did_paper_table_post_from_0,
  file.path(output_paths$tables, "placebo_did_paper_table_post_from_0.md")
)
write_clean_csv(
  placebo_did_paper_table_post_from_minus1,
  file.path(output_paths$tables, "placebo_did_paper_table_post_from_minus1.csv")
)
write_markdown_table(
  placebo_did_paper_table_post_from_minus1,
  file.path(output_paths$tables, "placebo_did_paper_table_post_from_minus1.md")
)
write_clean_csv(did_comparison_post_from_0, file.path(output_paths$tables, "did_real_vs_placebo_post_from_0.csv"))
write_markdown_table(
  did_comparison_post_from_0,
  file.path(output_paths$tables, "did_real_vs_placebo_post_from_0.md")
)
write_clean_csv(
  did_comparison_post_from_minus1,
  file.path(output_paths$tables, "did_real_vs_placebo_post_from_minus1.csv")
)
write_markdown_table(
  did_comparison_post_from_minus1,
  file.path(output_paths$tables, "did_real_vs_placebo_post_from_minus1.md")
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
    paste0(
      "Analysis sample: Sunday-start weeks ",
      as.character(analysis_start_week),
      " through ",
      as.character(analysis_end_week),
      "."
    ),
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

writeLines("\n--- placebo_did_design_overview ---", con = summary_connection)
write_formatted_table(placebo_did_design_overview, summary_connection)
writeLines("", con = summary_connection)

writeLines("--- placebo_did_sample_summary_by_model ---", con = summary_connection)
write_formatted_table(placebo_model_sample_summary, summary_connection)
writeLines("", con = summary_connection)

writeLines("\n--- placebo_did_design_overview_post_from_minus1 ---", con = summary_connection)
write_formatted_table(placebo_did_design_overview_post_from_minus1, summary_connection)
writeLines("", con = summary_connection)

writeLines("--- placebo_did_sample_summary_by_model_post_from_minus1 ---", con = summary_connection)
write_formatted_table(placebo_model_sample_summary_post_from_minus1, summary_connection)
writeLines("", con = summary_connection)

writeLines("\n--- placebo_did_models_post_from_0 ---", con = summary_connection)
write_model_summary_sections(
  model_names = placebo_model_results$model_name,
  coefficients_table = placebo_model_coefficients,
  fit_table = placebo_model_fit,
  summary_connection = summary_connection
)

writeLines("\n--- placebo_did_models_post_from_minus1 ---", con = summary_connection)
write_model_summary_sections(
  model_names = placebo_model_results_post_from_minus1$model_name,
  coefficients_table = placebo_model_coefficients_post_from_minus1,
  fit_table = placebo_model_fit_post_from_minus1,
  summary_connection = summary_connection
)

writeLines("\n--- did_real_vs_placebo_post_from_0 ---", con = summary_connection)
write_formatted_table(did_comparison_post_from_0, summary_connection)

writeLines("\n--- did_real_vs_placebo_post_from_minus1 ---", con = summary_connection)
write_formatted_table(did_comparison_post_from_minus1, summary_connection)

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

if (nrow(placebo_model_coefficients) > 0) {
  print_section("Placebo DiD Coefficients")
  print(format_output_table(placebo_model_coefficients))
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
cat("- tables/*\n")
cat("- post_from_0/*\n")
cat("- post_from_minus1/*\n")
cat("- placebo/post_from_0/*\n")
cat("- placebo/post_from_minus1/*\n")
if (nrow(all_model_coefficients) > 0) {
  cat("- post_from_0/did_coefficients_by_model.png\n")
}
if (nrow(all_model_coefficients_post_from_minus1) > 0) {
  cat("- post_from_minus1/did_coefficients_by_model.png\n")
}
if (nrow(placebo_model_coefficients) > 0) {
  cat("- placebo/post_from_0/did_coefficients_by_model.png\n")
}
if (nrow(placebo_model_coefficients_post_from_minus1) > 0) {
  cat("- placebo/post_from_minus1/did_coefficients_by_model.png\n")
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
