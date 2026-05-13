rm(list = ls())
gc()
cat("\014")

# Force a UTF-8 locale so Hebrew strings carry the correct Encoding() tag
# and survive readr / grid / ragg without being escaped to "<d7><..>" bytes.
local({
  utf8_candidates <- c("en_US.UTF-8", "C.UTF-8", "en_US.utf8", "C.utf8")
  for (candidate in utf8_candidates) {
    if (tryCatch(!identical(Sys.setlocale("LC_ALL", candidate), ""), error = function(e) FALSE)) break
  }
})

# =====================================================
# Weekly Spending Analysis + Event-Study Regressions
# =====================================================
# Goal:
# 1) Provide readable descriptive statistics
# 2) Run event-study regressions split by:
#    - political parties vs. other organizations/people
#    - political events vs. terror events
#
# Effective event-study specification used for the seminar paper:
#   log(Spending_{i,t}) = alpha_i + sum_k beta_k * D_{i,k} + u_{i,t}
#
# alpha_i  -> entity FE (entity_name; data_source FE absorbs platform shifts)
# D_{i,k}  -> relative-week dummies around the event week (k = -W..W),
#             baseline week omitted (default k = 0; robustness uses k = -1).
# Multi-event (stacked) panels also add event_id FE.
#
# Note on gamma_t (calendar-week FE): the original textbook spec written in
# the seminar plan was
#   log(Spending_{i,t}) = alpha_i + gamma_t + sum_k beta_k * D_{i,k} + u_{i,t}
# We empirically established that in our stacked design (every entity is in
# the +/-W window of every event) week_start_sunday FE absorbs the variation
# that identifies beta_k -- standard errors blow up by ~300x, fixest warns
# the VCOV is not positive semi-definite, and the relative-week coefficients
# collapse to a single linear-in-k pattern (e.g., -0.193, -0.097, +0.097,
# +0.193) which is uninformative. Following standard practice in the stacked
# event-study literature, we therefore omit gamma_t for the stacked specs.
#
# A dedicated demonstration regression that DOES include gamma_t is computed
# once in this script and saved under
#   outputs/analysis/event_study/gamma_t_demonstration/
# so the paper can show the broken output that motivated dropping gamma_t.
#
# Note on the dependent variable: the model uses log(Spending), not
# log(1 + Spending). Weeks with zero spend are filtered out before estimation
# (log(0) is undefined). This is a deliberate spec change from the earlier
# log1p() version; row counts dropped per panel are reported on stdout.
#
# Run in RStudio or command line:
# Rscript scripts/05_event_study.R [google_csv] [meta_csv] [events_csv] [output_dir] [window_weeks]

required_packages <- c(
  "readr", "dplyr", "tidyr", "lubridate", "stringr",
  "ggplot2", "fixest", "broom", "purrr", "tibble", "scales"
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
# Estimates are log-differences; the actual effect on weekly_spend_ils is
# exp(estimate). pct_change = (exp(estimate) - 1) * 100.
percent_change_from_log <- function(estimate_values) {
  ifelse(is.na(estimate_values), NA_real_, (exp(estimate_values) - 1) * 100)
}

# Match R's default significance codes:
# *** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.10.
significance_stars <- function(p_values) {
  dplyr::case_when(
    is.na(p_values) ~ "",
    p_values < 0.001 ~ "***",
    p_values < 0.01 ~ "**",
    p_values < 0.05 ~ "*",
    p_values < 0.10 ~ ".",
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

# Format a single number for a paper-style cell (fixed decimal width, NA -> "").
.fmt_paper_number <- function(x, digits = 3L) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !is.finite(x)) return("")
  formatC(x, format = "f", digits = digits)
}

# Render one model as a paper-style block:
#
#   (k) <model_label>
#   ---------------------------------------------------------------------
#                            Estimate    Std. Err.   pct change   p-value sig
#   relative_week = -2          0.071        0.031        +7.4%     0.023  **
#   ...
#   ---------------------------------------------------------------------
#   N (stacked event-window obs. used): <used_rows>    R^2: <r2>    Adj. R^2: <adjusted_r2>    Within R^2: <within_r2>
#
# Works for event-study coefficients (rows keyed by `relative_week`) and DiD
# coefficients (rows keyed by `term`).
write_paper_style_model_section <- function(model_label,
                                            coefficients_table,
                                            fit_table,
                                            summary_connection,
                                            model_index = NULL,
                                            include_baseline_marker = NULL) {
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
    writeLines("(no coefficients available)", con = summary_connection)
  } else {
    coef_rows <- coefficients_table

    if ("relative_week" %in% names(coef_rows)) {
      coef_rows <- coef_rows %>% dplyr::arrange(relative_week)
    }

    if (!is.null(include_baseline_marker) && "relative_week" %in% names(coef_rows)) {
      baseline_label <- sprintf("relative_week = %+d   (baseline, omitted; estimate fixed at 0)", include_baseline_marker)
    } else {
      baseline_label <- NULL
    }

    rendered_baseline <- FALSE
    for (row_index in seq_len(nrow(coef_rows))) {
      row <- coef_rows[row_index, ]

      variable_label <- if ("relative_week" %in% names(row)) {
        sprintf("relative_week = %+d", as.integer(row$relative_week))
      } else if ("term" %in% names(row)) {
        as.character(row$term)
      } else {
        "(unknown)"
      }

      if (!rendered_baseline && !is.null(baseline_label) && "relative_week" %in% names(row)) {
        if (as.integer(row$relative_week) > include_baseline_marker) {
          writeLines(baseline_label, con = summary_connection)
          rendered_baseline <- TRUE
        }
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

      coefficient_line <- sprintf("%-22s %10s %10s %12s %9s %4s",
                                  variable_label,
                                  .fmt_paper_number(row$estimate, 3L),
                                  .fmt_paper_number(row$std.error, 3L),
                                  pct_text,
                                  p_value_text,
                                  signif_text)
      writeLines(sub("\\s+$", "", coefficient_line), con = summary_connection)
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
      sprintf("N (stacked event-window obs. used): %s    R^2: %s    Adj. R^2: %s    Within R^2: %s",
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
                                      summary_connection,
                                      include_baseline_marker = NULL) {
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
      model_index = model_index,
      include_baseline_marker = include_baseline_marker
    )
  }
}

# Common header and reading-instructions block written at the top of every
# regression_summary.txt produced by the scripts.
write_paper_style_header <- function(summary_connection,
                                     title,
                                     spec_lines,
                                     dependent_variable_note,
                                     extra_notes = character()) {
  rule_width <- 73L
  writeLines(strrep("=", rule_width), con = summary_connection)
  writeLines(title, con = summary_connection)
  writeLines(strrep("=", rule_width), con = summary_connection)
  writeLines("Generated by event_study.R from the input files listed in this run.", con = summary_connection)
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
  writeLines("    ***  p < 0.001    **  p < 0.01    *  p < 0.05    .  p < 0.10", con = summary_connection)
  writeLines("Standard errors are clustered by entity_name. p-values < 0.001 are shown as", con = summary_connection)
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

format_table_count <- function(values) {
  ifelse(
    is.na(values),
    "",
    format(round(as.numeric(values)), big.mark = ",", scientific = FALSE, trim = TRUE)
  )
}

format_table_r2 <- function(values) {
  ifelse(
    is.na(values),
    "",
    formatC(round(as.numeric(values), digits = 3L), format = "f", digits = 3L)
  )
}

xml_escape <- function(text_values) {
  text_values <- as.character(text_values)
  text_values <- gsub("&", "&amp;", text_values, fixed = TRUE)
  text_values <- gsub("<", "&lt;", text_values, fixed = TRUE)
  text_values <- gsub(">", "&gt;", text_values, fixed = TRUE)
  text_values <- gsub('"', "&quot;", text_values, fixed = TRUE)
  text_values <- gsub("'", "&apos;", text_values, fixed = TRUE)
  text_values
}

xlsx_column_name <- function(column_number) {
  letters_out <- character()
  while (column_number > 0L) {
    remainder <- (column_number - 1L) %% 26L
    letters_out <- c(LETTERS[remainder + 1L], letters_out)
    column_number <- (column_number - 1L) %/% 26L
  }
  paste0(letters_out, collapse = "")
}

write_basic_xlsx <- function(dataframe, file_path, sheet_name = "Table") {
  output_directory <- dirname(file_path)
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  output_path <- file.path(normalizePath(output_directory, mustWork = TRUE), basename(file_path))

  temp_directory <- tempfile("xlsx_export_")
  dir.create(temp_directory, recursive = TRUE)
  on.exit(unlink(temp_directory, recursive = TRUE), add = TRUE)

  dir.create(file.path(temp_directory, "_rels"), recursive = TRUE)
  dir.create(file.path(temp_directory, "xl", "_rels"), recursive = TRUE)
  dir.create(file.path(temp_directory, "xl", "worksheets"), recursive = TRUE)

  sheet_name <- substr(sheet_name, 1L, 31L)
  sheet_values <- rbind(names(dataframe), as.matrix(dataframe))
  row_xml <- vapply(seq_len(nrow(sheet_values)), function(row_index) {
    cell_xml <- vapply(seq_len(ncol(sheet_values)), function(column_index) {
      value <- sheet_values[row_index, column_index]
      cell_reference <- paste0(xlsx_column_name(column_index), row_index)
      if (is.na(value) || identical(value, "")) {
        paste0("<c r=\"", cell_reference, "\"/>")
      } else {
        paste0(
          "<c r=\"", cell_reference, "\" t=\"inlineStr\"><is><t>",
          xml_escape(value),
          "</t></is></c>"
        )
      }
    }, character(1))

    paste0("<row r=\"", row_index, "\">", paste(cell_xml, collapse = ""), "</row>")
  }, character(1))

  writeLines(
    c(
      "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
      "<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">",
      "<Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/>",
      "<Default Extension=\"xml\" ContentType=\"application/xml\"/>",
      "<Override PartName=\"/xl/workbook.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml\"/>",
      "<Override PartName=\"/xl/worksheets/sheet1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml\"/>",
      "</Types>"
    ),
    con = file.path(temp_directory, "[Content_Types].xml")
  )

  writeLines(
    c(
      "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
      "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">",
      "<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\" Target=\"xl/workbook.xml\"/>",
      "</Relationships>"
    ),
    con = file.path(temp_directory, "_rels", ".rels")
  )

  writeLines(
    c(
      "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
      "<workbook xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">",
      "<sheets>",
      paste0("<sheet name=\"", xml_escape(sheet_name), "\" sheetId=\"1\" r:id=\"rId1\"/>"),
      "</sheets>",
      "</workbook>"
    ),
    con = file.path(temp_directory, "xl", "workbook.xml")
  )

  writeLines(
    c(
      "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
      "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">",
      "<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet\" Target=\"worksheets/sheet1.xml\"/>",
      "</Relationships>"
    ),
    con = file.path(temp_directory, "xl", "_rels", "workbook.xml.rels")
  )

  writeLines(
    c(
      "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>",
      "<worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\">",
      "<sheetData>",
      row_xml,
      "</sheetData>",
      "</worksheet>"
    ),
    con = file.path(temp_directory, "xl", "worksheets", "sheet1.xml")
  )

  if (file.exists(output_path)) {
    unlink(output_path)
  }

  old_working_directory <- getwd()
  on.exit(setwd(old_working_directory), add = TRUE)
  setwd(temp_directory)

  zip_binary <- Sys.which("zip")
  if (nzchar(zip_binary)) {
    status <- system2(zip_binary, args = c("-q", "-r", output_path, "."))
    if (!identical(status, 0L)) {
      stop("Failed to write xlsx file: ", output_path)
    }
  } else {
    utils::zip(zipfile = output_path, files = list.files(".", recursive = TRUE, all.files = TRUE))
  }

  invisible(output_path)
}

write_latex_panel_summary <- function(dataframe, file_path, caption, label, note_text) {
  table_lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    paste0("\\caption{", latex_escape(caption), "}"),
    paste0("\\label{", label, "}"),
    "\\begin{tabular}{lccc}",
    "\\hline\\hline",
    paste0(paste(latex_escape(names(dataframe)), collapse = " & "), " \\\\"),
    "\\hline"
  )

  for (row_index in seq_len(nrow(dataframe))) {
    row_values <- as.character(dataframe[row_index, , drop = TRUE])
    empty_data_cells <- all(is.na(row_values[-1]) | row_values[-1] == "")
    if (empty_data_cells) {
      table_lines <- c(
        table_lines,
        paste0("\\multicolumn{4}{l}{", latex_escape(row_values[[1]]), "} \\\\")
      )
    } else {
      table_lines <- c(
        table_lines,
        paste0(paste(latex_escape(row_values), collapse = " & "), " \\\\")
      )
    }
  }

  table_lines <- c(
    table_lines,
    "\\hline",
    paste0("\\multicolumn{4}{l}{\\footnotesize ", latex_escape(note_text), "} \\\\"),
    "\\hline\\hline",
    "\\end{tabular}",
    "\\end{table}"
  )

  writeLines(table_lines, con = file_path)
  invisible(NULL)
}

build_regression_panel_summary_he <- function(matrix_table,
                                              coefficient_row_label,
                                              full_sample_rows_by_entity) {
  output_columns <- c(
    "המשתנה התלוי: log(הוצאה שבועית)",
    "(1) כל המפרסמים",
    "(2) מפלגות",
    "(3) גופים פרטיים"
  )
  entity_labels <- c("All entities", "Political parties", "Organizations/people")
  event_panels <- tibble::tribble(
    ~panel_label, ~event_label, ~r2_label, ~n_label, ~unique_label,
    "פאנל א': כל האירועים", "All events", "R2 - כל האירועים",
    "N - תצפיות חלון-אירוע מוערמות - כל האירועים",
    "שורות שבועיות ייחודיות בחלון - כל האירועים",
    "פאנל ב': אירועים פוליטיים", "Political events", "R2 - אירועים פוליטיים",
    "N - תצפיות חלון-אירוע מוערמות - אירועים פוליטיים",
    "שורות שבועיות ייחודיות בחלון - אירועים פוליטיים",
    "פאנל ג': אירועי טרור", "Terror events", "R2 - אירועי טרור",
    "N - תצפיות חלון-אירוע מוערמות - אירועי טרור",
    "שורות שבועיות ייחודיות בחלון - אירועי טרור"
  )

  make_row <- function(row_label, values = c("", "", "")) {
    row <- as.data.frame(
      as.list(c(row_label, values)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    names(row) <- output_columns
    row
  }

  get_values <- function(event_label, value_column, formatter = as.character) {
    raw_values <- vapply(entity_labels, function(entity_label) {
      selected_row <- matrix_table %>%
        dplyr::filter(entity_label == !!entity_label, event_label == !!event_label) %>%
        dplyr::slice_head(n = 1)

      if (nrow(selected_row) == 0 || !value_column %in% names(selected_row)) {
        return(NA_character_)
      }

      as.character(selected_row[[value_column]][[1]])
    }, character(1))

    formatter(raw_values)
  }

  panel_rows <- purrr::map_dfr(seq_len(nrow(event_panels)), function(panel_index) {
    panel <- event_panels[panel_index, ]
    dplyr::bind_rows(
      make_row(panel$panel_label),
      make_row(coefficient_row_label, get_values(panel$event_label, "estimate_display")),
      make_row("(טעות תקן)", get_values(panel$event_label, "std_error_display"))
    )
  })

  controls_rows <- dplyr::bind_rows(
    make_row("בקרות ונתוני מודל"),
    purrr::map_dfr(seq_len(nrow(event_panels)), function(panel_index) {
      panel <- event_panels[panel_index, ]
      make_row(panel$r2_label, get_values(panel$event_label, "r2", format_table_r2))
    }),
    make_row("Fixed Effects", c("Yes", "Yes", "Yes")),
    make_row("שורות שבועיות במדגם התיאורי", format_table_count(full_sample_rows_by_entity[entity_labels])),
    purrr::map_dfr(seq_len(nrow(event_panels)), function(panel_index) {
      panel <- event_panels[panel_index, ]
      dplyr::bind_rows(
        make_row(panel$n_label, get_values(panel$event_label, "used_rows", format_table_count)),
        make_row(panel$unique_label, get_values(panel$event_label, "unique_weekly_rows", format_table_count))
      )
    })
  )

  dplyr::bind_rows(panel_rows, controls_rows)
}

write_panel_summary_outputs <- function(summary_table,
                                        output_base_path,
                                        title,
                                        subtitle,
                                        latex_caption,
                                        latex_label,
                                        note_text) {
  readr::write_csv(summary_table, paste0(output_base_path, ".csv"), na = "")
  write_markdown_table(summary_table, paste0(output_base_path, ".md"))
  write_html_presentation_table(
    summary_table,
    paste0(output_base_path, ".html"),
    title = title,
    subtitle = subtitle
  )
  write_latex_panel_summary(
    summary_table,
    paste0(output_base_path, ".tex"),
    caption = latex_caption,
    label = latex_label,
    note_text = note_text
  )
  write_basic_xlsx(summary_table, paste0(output_base_path, ".xlsx"), sheet_name = "summary")
}

pretty_model_label <- function(entity_group, event_scope) {
  paste(
    dplyr::case_when(
      entity_group == "all_entities" ~ "All entities",
      entity_group == "political_party" ~ "Political parties",
      entity_group == "other_org_or_person" ~ "Other orgs/people",
      TRUE ~ entity_group
    ),
    "x",
    dplyr::case_when(
      event_scope == "all_events" ~ "all events",
      event_scope == "political" ~ "political events",
      event_scope == "terror" ~ "terror events",
      TRUE ~ event_scope
    )
  )
}

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

build_event_study_key_results_matrix <- function(coefficients_table,
                                                 model_fit_table,
                                                 model_specifications_table,
                                                 target_relative_week = 1L) {
  entity_order <- c("All entities", "Political parties", "Organizations/people")
  event_order <- c("All events", "Political events", "Terror events")

  coefficients_compact <- coefficients_table %>%
    dplyr::filter(relative_week == target_relative_week) %>%
    dplyr::mutate(
      estimate_display = format_estimate_with_stars(estimate, p.value),
      std_error_display = format_standard_error_for_paper(std.error)
    ) %>%
    dplyr::select(model_name, estimate_display, std_error_display, p.value)

  fit_compact <- model_fit_table %>%
    dplyr::select(
      model_name,
      input_rows,
      used_rows,
      unique_weekly_rows,
      stacked_extra_rows,
      r2,
      within_r2
    )

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

write_latex_simple_table <- function(dataframe, file_path, caption, label, note_text) {
  table_lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    paste0("\\caption{", latex_escape(caption), "}"),
    paste0("\\label{", label, "}"),
    "\\begin{tabular}{lll}",
    "\\hline\\hline",
    paste0(paste(latex_escape(names(dataframe)), collapse = " & "), " \\\\"),
    "\\hline"
  )

  for (row_index in seq_len(nrow(dataframe))) {
    row_values <- as.character(dataframe[row_index, , drop = TRUE])
    table_lines <- c(
      table_lines,
      paste0(paste(latex_escape(row_values), collapse = " & "), " \\\\")
    )
  }

  table_lines <- c(
    table_lines,
    "\\hline",
    paste0("\\multicolumn{3}{l}{\\footnotesize Notes: ", latex_escape(note_text), "}\\\\"),
    "\\hline\\hline",
    "\\end{tabular}",
    "\\end{table}"
  )

  writeLines(table_lines, con = file_path)
  invisible(NULL)
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

write_html_presentation_table <- function(dataframe,
                                          file_path,
                                          title,
                                          subtitle = NULL,
                                          direction = "rtl") {
  display_table <- dataframe %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = ~ dplyr::if_else(is.na(.x), "", as.character(.x))
      )
    )

  is_number_like <- function(cell_value) {
    cell_value <- trimws(cell_value)
    cell_value != "" &&
      grepl("^[-()0-9,.*+<>% ]+$", cell_value)
  }

  header_cells <- paste0("<th>", html_escape(names(display_table)), "</th>", collapse = "")
  body_lines <- if (nrow(display_table) == 0) {
    paste0("<tr><td colspan=\"", ncol(display_table), "\">No rows.</td></tr>")
  } else {
    apply(display_table, 1, function(row_values) {
      empty_data_cells <- all(trimws(row_values[-1]) == "")
      if (empty_data_cells) {
        return(
          paste0(
            "<tr class=\"panel-row\"><th colspan=\"",
            ncol(display_table),
            "\">",
            html_escape(row_values[[1]]),
            "</th></tr>"
          )
        )
      }

      row_cells <- vapply(seq_along(row_values), function(column_index) {
        tag_name <- if (column_index == 1L) "th" else "td"
        class_attribute <- if (column_index > 1L && is_number_like(row_values[[column_index]])) {
          " class=\"num\""
        } else {
          ""
        }
        paste0(
          "<",
          tag_name,
          class_attribute,
          ">",
          html_escape(row_values[[column_index]]),
          "</",
          tag_name,
          ">"
        )
      }, character(1))

      paste0("<tr>", paste(row_cells, collapse = ""), "</tr>")
    })
  }

  subtitle_line <- if (is.null(subtitle) || is.na(subtitle) || subtitle == "") {
    character()
  } else {
    paste0("<p class=\"subtitle\">", html_escape(subtitle), "</p>")
  }

  html_lines <- c(
    "<!doctype html>",
    paste0("<html lang=\"he\" dir=\"", html_escape(direction), "\">"),
    "<head>",
    "<meta charset=\"utf-8\">",
    "<style>",
    "body { font-family: Arial, 'Noto Sans Hebrew', sans-serif; color: #111; margin: 32px; background: #fff; }",
    ".table-wrap { max-width: 980px; margin: 0 auto; }",
    "h1 { font-size: 18px; text-align: center; font-weight: 700; margin: 0 0 8px; }",
    ".subtitle { text-align: center; font-size: 13px; margin: 0 0 18px; color: #555; }",
    "table { width: 100%; border-collapse: collapse; border-top: 2px solid #111; border-bottom: 2px solid #111; font-size: 14px; direction: rtl; }",
    "thead th { border-bottom: 1.5px solid #111; font-weight: 700; padding: 10px 8px; text-align: center; }",
    "tbody td, tbody th { border-bottom: 1px solid #e5e5e5; padding: 10px 8px; text-align: center; vertical-align: middle; }",
    "tbody th:first-child { text-align: right; font-weight: 600; }",
    "tbody tr:last-child td, tbody tr:last-child th { border-bottom: 0; }",
    "td.num { direction: ltr; unicode-bidi: isolate; }",
    ".panel-row th { background: #f3f4f6; border-top: 1.5px solid #111; font-weight: 700; text-align: right; }",
    "td, th { font-variant-numeric: tabular-nums; }",
    "</style>",
    "</head>",
    "<body>",
    "<div class=\"table-wrap\">",
    paste0("<h1>", html_escape(title), "</h1>"),
    subtitle_line,
    "<table>",
    "<thead>",
    paste0("<tr>", header_cells, "</tr>"),
    "</thead>",
    "<tbody>",
    body_lines,
    "</tbody>",
    "</table>",
    "</div>",
    "</body>",
    "</html>"
  )

  writeLines(html_lines, con = file_path)
  invisible(NULL)
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

safe_correlation_test <- function(x, y) {
  valid_rows <- stats::complete.cases(x, y)
  x <- x[valid_rows]
  y <- y[valid_rows]

  if (length(x) < 3 || stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(list(correlation = NA_real_, p_value = NA_real_, observations = length(x)))
  }

  test_result <- suppressWarnings(stats::cor.test(x, y, method = "pearson"))
  list(
    correlation = unname(test_result$estimate[[1]]),
    p_value = unname(test_result$p.value),
    observations = length(x)
  )
}

build_correlation_comparison_table <- function(real_summary, placebo_summary) {
  real_table <- real_summary %>%
    dplyr::rename(
      real_correlation = correlation_weekly_spend_vs_event_count,
      real_p.value = p.value,
      real_weeks_in_sample = weeks_in_sample,
      real_weeks_with_any_event = weeks_with_any_event,
      real_average_weekly_spend_ils = average_weekly_spend_ils,
      real_average_weekly_event_count = average_weekly_event_count
    )

  placebo_table <- placebo_summary %>%
    dplyr::rename(
      placebo_correlation = correlation_weekly_spend_vs_event_count,
      placebo_p.value = p.value,
      placebo_weeks_in_sample = weeks_in_sample,
      placebo_weeks_with_any_event = weeks_with_any_event,
      placebo_average_weekly_spend_ils = average_weekly_spend_ils,
      placebo_average_weekly_event_count = average_weekly_event_count
    )

  tibble::tibble(
    entity_group = c("all_entities", "political_party", "other_org_or_person")
  ) %>%
    tidyr::crossing(event_scope = c("all_events", "political", "terror")) %>%
    dplyr::left_join(real_table, by = c("entity_group", "event_scope")) %>%
    dplyr::left_join(placebo_table, by = c("entity_group", "event_scope")) %>%
    dplyr::mutate(model_label = pretty_model_label(entity_group, event_scope)) %>%
    dplyr::select(
      entity_group,
      event_scope,
      model_label,
      real_correlation,
      real_p.value,
      real_weeks_in_sample,
      real_weeks_with_any_event,
      placebo_correlation,
      placebo_p.value,
      placebo_weeks_in_sample,
      placebo_weeks_with_any_event
    )
}

build_correlation_publication_table <- function(comparison_table) {
  table_rows <- comparison_table %>%
    dplyr::mutate(column_label = model_label) %>%
    dplyr::select(
      column_label,
      real_correlation,
      real_p.value,
      placebo_correlation,
      placebo_p.value,
      real_weeks_in_sample
    )

  row_definitions <- tibble::tribble(
    ~row_label, ~value_column,
    "Real-event correlation", "real_correlation",
    "Real-event p-value", "real_p.value",
    "Placebo correlation", "placebo_correlation",
    "Placebo p-value", "placebo_p.value",
    "Weeks in sample", "real_weeks_in_sample"
  )

  purrr::map_dfr(seq_len(nrow(row_definitions)), function(row_index) {
    value_column <- row_definitions$value_column[[row_index]]

    tibble::tibble(
      row_label = row_definitions$row_label[[row_index]],
      column_label = table_rows$column_label,
      value = table_rows[[value_column]]
    )
  }) %>%
    tidyr::pivot_wider(names_from = column_label, values_from = value) %>%
    dplyr::rename(Statistic = row_label)
}

add_baseline_coefficient_row <- function(coefficients_table, model_name_label, baseline_relative_week = 0L) {
  baseline_row <- tibble::tibble(
    model_name = model_name_label,
    relative_week = baseline_relative_week,
    estimate = 0,
    std.error = NA_real_,
    statistic = NA_real_,
    p.value = NA_real_,
    conf.low = 0,
    conf.high = 0,
    pct_change = 0,
    signif = ""
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

run_event_study_model <- function(model_data, reference_week = 0L, include_gamma_t = FALSE) {
  if (nrow(model_data) < 10) {
    return(NULL)
  }
  if (dplyr::n_distinct(model_data$entity_name) < 2 || dplyr::n_distinct(model_data$relative_week) < 2) {
    return(NULL)
  }

  # Effective specification used in this project:
  #   log(Spending_{i,t}) = alpha_i + sum_k beta_k * D_{i,k} + u_{i,t}
  # alpha_i  -> entity_name FE (and data_source FE to absorb platform-level shifts)
  # Multi-event (stacked) panels also add event_id FE.
  #
  # Why we DO NOT include gamma_t (calendar-week FE) in the stacked specs:
  # In our design every entity is observed in the +/-W window of every event
  # (the panel is built by crossing(spend, events)), so on any calendar week
  # the relative-week dummies are nearly collinear with calendar-week FE.
  # Adding week_start_sunday FE absorbs the variation that identifies beta_k:
  # standard errors blow up ~300x, fixest reports a non-positive-semi-definite
  # VCOV, and the four event-study coefficients collapse to a single linear-in-k
  # direction (e.g., -0.193, -0.097, +0.097, +0.193). The 'with gamma_t'
  # demonstration regression in this script reproduces that breakdown for the
  # paper.
  #
  # Single-event runs always drop gamma_t (calendar week is one-to-one with
  # relative week within one event window).
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

  model_formula <- as.formula(sprintf(
    "log_weekly_spend ~ i(relative_week, ref = %s) | %s",
    reference_week,
    fe_terms
  ))

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
      model_name = model_name_label,
      pct_change = percent_change_from_log(estimate),
      signif = significance_stars(p.value)
    ) %>%
    dplyr::select(
      model_name,
      relative_week,
      estimate,
      std.error,
      statistic,
      p.value,
      conf.low,
      conf.high,
      pct_change,
      signif
    ) %>%
    dplyr::arrange(relative_week)
}

summarize_model_input <- function(input_data, fallback_input_rows) {
  if (is.null(input_data) || nrow(input_data) == 0) {
    return(
      tibble::tibble(
        input_rows = fallback_input_rows,
        unique_weekly_rows = NA_real_,
        stacked_extra_rows = NA_real_,
        input_entities = NA_real_,
        input_events = NA_real_,
        first_week = as.Date(NA),
        last_week = as.Date(NA)
      )
    )
  }

  unique_weekly_rows <- if ("spend_row_id" %in% names(input_data)) {
    dplyr::n_distinct(input_data$spend_row_id)
  } else {
    NA_real_
  }

  tibble::tibble(
    input_rows = nrow(input_data),
    unique_weekly_rows = unique_weekly_rows,
    stacked_extra_rows = ifelse(is.na(unique_weekly_rows), NA_real_, nrow(input_data) - unique_weekly_rows),
    input_entities = dplyr::n_distinct(input_data$entity_name),
    input_events = dplyr::n_distinct(input_data$event_id),
    first_week = min(input_data$week_start_sunday, na.rm = TRUE),
    last_week = max(input_data$week_start_sunday, na.rm = TRUE)
  )
}

extract_model_fit <- function(model_object, model_name_label, input_row_count, input_data = NULL) {
  input_summary <- summarize_model_input(input_data, input_row_count)

  if (is.null(model_object)) {
    return(
      dplyr::bind_cols(
        tibble::tibble(
        model_name = model_name_label,
        model_status = "failed_or_insufficient_data",
        used_rows = NA_real_,
        r2 = NA_real_,
        within_r2 = NA_real_,
        adjusted_r2 = NA_real_
        ),
        input_summary
      )
    )
  }

  dplyr::bind_cols(
    tibble::tibble(
    model_name = model_name_label,
    model_status = "ok",
    used_rows = nobs(model_object),
    r2 = safe_fitstat(model_object, "r2"),
    within_r2 = safe_fitstat(model_object, "wr2"),
    adjusted_r2 = safe_fitstat(model_object, "ar2")
    ),
    input_summary
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
    dplyr::group_modify(function(.x, .y) {
      correlation_test <- safe_correlation_test(.x$total_weekly_spend_ils, .x$weekly_event_count)

      tibble::tibble(
        correlation_weekly_spend_vs_event_count = correlation_test$correlation,
        p.value = correlation_test$p_value,
        weeks_in_sample = correlation_test$observations,
        weeks_with_any_event = sum(.x$weekly_event_count > 0, na.rm = TRUE),
        average_weekly_spend_ils = mean(.x$total_weekly_spend_ils, na.rm = TRUE),
        average_weekly_event_count = mean(.x$weekly_event_count, na.rm = TRUE)
      )
    }) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(entity_group, event_scope)

  list(
    correlation_panel = correlation_panel_local,
    correlation_panel_long = correlation_panel_long_local,
    correlation_summary = correlation_summary_local
  )
}

filter_complete_placebo_windows <- function(placebo_events_table,
                                            analysis_start_week,
                                            analysis_end_week,
                                            window_weeks) {
  earliest_valid_week <- analysis_start_week + lubridate::weeks(window_weeks)
  latest_valid_week <- analysis_end_week - lubridate::weeks(window_weeks)

  invalid_placebo_events <- placebo_events_table %>%
    dplyr::filter(
      event_week_start_sunday < earliest_valid_week |
        event_week_start_sunday > latest_valid_week
  )

  if (nrow(invalid_placebo_events) > 0) {
    stop(
      "Placebo file contains ",
      nrow(invalid_placebo_events),
      " event(s) outside the required +/-",
      window_weeks,
      " week buffer inside the analysis sample. Regenerate placebo_events_2020_2025.csv. Invalid weeks: ",
      paste(invalid_placebo_events$event_week_start_sunday, collapse = ", "),
      call. = FALSE
    )
  }

  placebo_events_table %>%
    dplyr::filter(
      event_week_start_sunday >= earliest_valid_week,
      event_week_start_sunday <= latest_valid_week
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
    stop(
      "Missing canonical placebo event file: ",
      placebo_file_path,
      ". Run `python3 scripts/03_generate_placebo_events.py` before running the analysis.",
      call. = FALSE
    )
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
    )

  invalid_rows <- placebo_events %>%
    dplyr::filter(is.na(event_date) | !event_type_group %in% c("political", "terror"))

  if (nrow(invalid_rows) > 0) {
    stop(
      "Placebo file contains invalid rows. Required: parseable event_date and event_type_group in political/terror. File: ",
      placebo_file_path,
      call. = FALSE
    )
  }

  if (nrow(placebo_events) == 0) {
    stop("Placebo file has no valid rows after parsing: ", placebo_file_path)
  }

  duplicate_dates <- placebo_events %>%
    dplyr::count(event_date) %>%
    dplyr::filter(n > 1)

  if (nrow(duplicate_dates) > 0) {
    stop(
      "Placebo file contains duplicate event_date values: ",
      paste(duplicate_dates$event_date, collapse = ", "),
      call. = FALSE
    )
  }

  non_sunday_dates <- placebo_events %>%
    dplyr::filter(format(event_date, "%w") != "0")

  if (nrow(non_sunday_dates) > 0) {
    stop(
      "Placebo file contains non-Sunday dates: ",
      paste(non_sunday_dates$event_date, collapse = ", "),
      call. = FALSE
    )
  }

  placebo_events <- placebo_events %>%
    dplyr::arrange(event_date) %>%
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
      "Placebo file violates minimum distance from real events. Min gap observed: ",
      minimum_gap,
      " weeks; required: > ",
      min_gap_weeks,
      " weeks. Regenerate placebo_events_2020_2025.csv.",
      call. = FALSE
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
# Inputs and configuration
# -------------------------
script_arguments <- commandArgs(trailingOnly = TRUE)

default_google_path <- resolve_default_path(c(
  "./data/processed/second_cleaning/weekly_party_spend_google.csv",
  "./data/processed/cleaned_data/weekly_party_spend_google.csv"
))
default_meta_path <- resolve_default_path(c(
  "./data/processed/second_cleaning/weekly_party_spend_meta.csv",
  "./data/processed/cleaned_data/weekly_party_spend_meta.csv"
))
default_events_path <- resolve_default_path(c(
  "./data/raw/events/Consolidated List of Terror and Political incidents 2020-2025 v4.csv"
))

google_data_path <- if (length(script_arguments) >= 1) script_arguments[1] else default_google_path
meta_data_path <- if (length(script_arguments) >= 2) script_arguments[2] else default_meta_path
events_data_path <- if (length(script_arguments) >= 3) script_arguments[3] else default_events_path
output_directory <- if (length(script_arguments) >= 4) script_arguments[4] else "./outputs/analysis"
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
  descriptive = file.path(output_directory, "descriptive"),
  correlations_real = file.path(output_directory, "correlations", "real_events"),
  correlations_placebo = file.path(output_directory, "correlations", "placebo_events"),
  event_study_baseline_0 = file.path(output_directory, "event_study", "baseline_0"),
  event_study_baseline_0_figures = file.path(output_directory, "event_study", "baseline_0", "figures_by_model"),
  event_study_baseline_minus1 = file.path(output_directory, "event_study", "baseline_minus1"),
  event_study_baseline_minus1_figures = file.path(output_directory, "event_study", "baseline_minus1", "figures_by_model"),
  placebo_event_study_baseline_0 = file.path(output_directory, "placebo_event_study", "baseline_0"),
  placebo_event_study_baseline_0_figures = file.path(output_directory, "placebo_event_study", "baseline_0", "figures_by_model"),
  placebo_event_study_baseline_minus1 = file.path(output_directory, "placebo_event_study", "baseline_minus1"),
  placebo_event_study_baseline_minus1_figures = file.path(output_directory, "placebo_event_study", "baseline_minus1", "figures_by_model"),
  oct7_baseline_0 = file.path(output_directory, "oct7_event_study", "baseline_0"),
  oct7_baseline_minus1 = file.path(output_directory, "oct7_event_study", "baseline_minus1"),
  gamma_t_demonstration = file.path(output_directory, "event_study", "gamma_t_demonstration")
)
invisible(lapply(output_paths, dir.create, showWarnings = FALSE, recursive = TRUE))

legacy_output_paths <- file.path(
  output_directory,
  c(
    "descriptive_overall.csv",
    "descriptive_by_group.csv",
    "descriptive_by_year.csv",
    "descriptive_by_year_and_group.csv",
    "descriptive_pre_post_oct7.csv",
    "correlation_summary.csv",
    "correlation_coefficients_heatmap.png",
    "correlation_scatter_panels.png",
    "placebo_correlation_summary.csv",
    "placebo_correlation_coefficients_heatmap.png",
    "placebo_correlation_scatter_panels.png",
    "event_study_coefficients_by_model.csv",
    "event_study_model_fit.csv",
    "event_study_coefficients_by_model_ref_minus1.csv",
    "event_study_model_fit_ref_minus1.csv",
    "placebo_event_study_coefficients_by_model.csv",
    "placebo_event_study_model_fit.csv",
    "placebo_event_study_coefficients_by_model_ref_minus1.csv",
    "placebo_event_study_model_fit_ref_minus1.csv",
    "regression_summary.txt",
    "event_study_figure_all_models.png",
    "event_study_figure_all_models_ref_minus1.png",
    "placebo_event_study_figure_all_models.png",
    "placebo_event_study_figure_all_models_ref_minus1.png",
    "event_study_coefs_oct7.csv",
    "event_study_coefs_oct7_by_group.csv",
    "event_study_coefs_oct7_ref_minus1.csv",
    "event_study_coefs_oct7_by_group_ref_minus1.csv",
    "event_study_coefs_oct7_all_party_org.csv",
    "event_study_model_fit_oct7_by_group.csv",
    "event_study_model_fit_oct7_by_group_ref_minus1.csv",
    "event_study_figure_oct7.png",
    "event_study_figure_oct7_by_group.png",
    "event_study_figure_oct7_ref_minus1.png",
    "event_study_figure_oct7_by_group_ref_minus1.png",
    "event_study_figures",
    "event_study_figures_ref_minus1",
    "placebo_event_study_figures",
    "placebo_event_study_figures_ref_minus1",
    file.path("tables", "event_study_key_results_relative_week_plus1.csv"),
    file.path("tables", "event_study_key_results_relative_week_plus1.tex"),
    file.path("tables", "event_study_key_results_relative_week_plus1.html"),
    file.path("tables", "event_study_key_results_relative_week_plus1.png"),
    file.path("tables", "event_study_key_results_relative_week_plus1.pdf")
  )
)
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

weekly_spend_raw <- dplyr::bind_rows(google_weekly_spend, meta_weekly_spend)

weekly_spend_panel <- weekly_spend_raw %>%
  dplyr::transmute(
    data_source = as.character(source),
    entity_name = as.character(party_name),
    week_start_sunday = parse_date_flexible(week_start_sunday),
    source_week_index_since_2020 = if ("week_index_since_2020" %in% names(weekly_spend_raw)) {
      as.integer(week_index_since_2020)
    } else {
      NA_integer_
    },
    weekly_spend_ils = as.numeric(total_spend_week),
    source_avg_spend_per_day_week = if ("avg_spend_per_day_week" %in% names(weekly_spend_raw)) {
      as.numeric(avg_spend_per_day_week)
    } else {
      NA_real_
    },
    entity_class_raw = as.character(class),
    entity_group = normalize_entity_group(as.character(class)),
    currency = as.character(currency)
  ) %>%
  dplyr::filter(!is.na(week_start_sunday), !is.na(weekly_spend_ils)) %>%
  dplyr::filter(week_start_sunday >= analysis_start_week, week_start_sunday <= analysis_end_week) %>%
  dplyr::mutate(
    week_index_since_2020 = dplyr::if_else(
      is.na(source_week_index_since_2020),
      as.integer((week_start_sunday - analysis_start_week) / 7) + 1L,
      source_week_index_since_2020
    ),
    avg_spend_per_day_week = dplyr::if_else(
      is.na(source_avg_spend_per_day_week),
      weekly_spend_ils / 7,
      source_avg_spend_per_day_week
    ),
    calendar_year = lubridate::year(week_start_sunday),
    spend_row_id = dplyr::row_number()
  ) %>%
  dplyr::select(-source_week_index_since_2020, -source_avg_spend_per_day_week)

if (nrow(weekly_spend_panel) == 0) {
  stop("No valid weekly spending rows after cleaning.")
}

# -------------------------
# Load and clean events data
# -------------------------
raw_events <- readr::read_csv(events_data_path, show_col_types = FALSE)
names(raw_events) <- trimws(names(raw_events))

required_event_columns <- c("Date", "Type")
missing_event_columns <- setdiff(required_event_columns, names(raw_events))
if (length(missing_event_columns) > 0) {
  stop("Events file missing columns: ", paste(missing_event_columns, collapse = ", "))
}
if (!"Article" %in% names(raw_events)) {
  raw_events$Article <- if ("Name" %in% names(raw_events)) raw_events$Name else NA_character_
}
if (!"Details" %in% names(raw_events)) {
  raw_events$Details <- NA_character_
}

events_table <- raw_events %>%
  dplyr::transmute(
    event_date = parse_date_flexible(Date),
    event_type_raw = as.character(Type),
    event_type_group = dplyr::case_when(
      stringr::str_detect(tolower(Type), "terror|security") ~ "terror",
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
    analysis_start_week = analysis_start_week,
    analysis_end_week = analysis_end_week,
    total_spend_ils = sum(weekly_spend_ils, na.rm = TRUE),
    average_weekly_row_spend_ils = mean(weekly_spend_ils, na.rm = TRUE),
    median_weekly_row_spend_ils = median(weekly_spend_ils, na.rm = TRUE),
    sd_weekly_row_spend_ils = stats::sd(weekly_spend_ils, na.rm = TRUE),
    min_weekly_row_spend_ils = min(weekly_spend_ils, na.rm = TRUE),
    p25_weekly_row_spend_ils = as.numeric(stats::quantile(weekly_spend_ils, 0.25, na.rm = TRUE)),
    p75_weekly_row_spend_ils = as.numeric(stats::quantile(weekly_spend_ils, 0.75, na.rm = TRUE)),
    max_weekly_row_spend_ils = max(weekly_spend_ils, na.rm = TRUE),
    total_rows = dplyr::n(),
    total_week_rows = dplyr::n_distinct(week_start_sunday),
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
    sd_weekly_row_spend_ils = stats::sd(weekly_spend_ils, na.rm = TRUE),
    min_weekly_row_spend_ils = min(weekly_spend_ils, na.rm = TRUE),
    p25_weekly_row_spend_ils = as.numeric(stats::quantile(weekly_spend_ils, 0.25, na.rm = TRUE)),
    p75_weekly_row_spend_ils = as.numeric(stats::quantile(weekly_spend_ils, 0.75, na.rm = TRUE)),
    max_weekly_row_spend_ils = max(weekly_spend_ils, na.rm = TRUE),
    rows = dplyr::n(),
    week_rows = dplyr::n_distinct(week_start_sunday),
    entities = dplyr::n_distinct(entity_name),
    first_week = min(week_start_sunday, na.rm = TRUE),
    last_week = max(week_start_sunday, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(total_spend_ils))

yearly_spend_stats <- weekly_spend_panel %>%
  dplyr::group_by(calendar_year) %>%
  dplyr::summarise(
    total_spend_ils = sum(weekly_spend_ils, na.rm = TRUE),
    average_weekly_row_spend_ils = mean(weekly_spend_ils, na.rm = TRUE),
    median_weekly_row_spend_ils = median(weekly_spend_ils, na.rm = TRUE),
    sd_weekly_row_spend_ils = stats::sd(weekly_spend_ils, na.rm = TRUE),
    min_weekly_row_spend_ils = min(weekly_spend_ils, na.rm = TRUE),
    p25_weekly_row_spend_ils = as.numeric(stats::quantile(weekly_spend_ils, 0.25, na.rm = TRUE)),
    p75_weekly_row_spend_ils = as.numeric(stats::quantile(weekly_spend_ils, 0.75, na.rm = TRUE)),
    max_weekly_row_spend_ils = max(weekly_spend_ils, na.rm = TRUE),
    rows = dplyr::n(),
    week_rows = dplyr::n_distinct(week_start_sunday),
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
    median_weekly_row_spend_ils = median(weekly_spend_ils, na.rm = TRUE),
    sd_weekly_row_spend_ils = stats::sd(weekly_spend_ils, na.rm = TRUE),
    min_weekly_row_spend_ils = min(weekly_spend_ils, na.rm = TRUE),
    p25_weekly_row_spend_ils = as.numeric(stats::quantile(weekly_spend_ils, 0.25, na.rm = TRUE)),
    p75_weekly_row_spend_ils = as.numeric(stats::quantile(weekly_spend_ils, 0.75, na.rm = TRUE)),
    max_weekly_row_spend_ils = max(weekly_spend_ils, na.rm = TRUE),
    rows = dplyr::n(),
    week_rows = dplyr::n_distinct(week_start_sunday),
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
    median_weekly_row_spend_ils = median(weekly_spend_ils, na.rm = TRUE),
    sd_weekly_row_spend_ils = stats::sd(weekly_spend_ils, na.rm = TRUE),
    min_weekly_row_spend_ils = min(weekly_spend_ils, na.rm = TRUE),
    p25_weekly_row_spend_ils = as.numeric(stats::quantile(weekly_spend_ils, 0.25, na.rm = TRUE)),
    p75_weekly_row_spend_ils = as.numeric(stats::quantile(weekly_spend_ils, 0.75, na.rm = TRUE)),
    max_weekly_row_spend_ils = max(weekly_spend_ils, na.rm = TRUE),
    rows = dplyr::n(),
    week_rows = dplyr::n_distinct(week_start_sunday),
    entities = dplyr::n_distinct(entity_name),
    .groups = "drop"
  ) %>%
  dplyr::arrange(period_vs_oct7, entity_group)

full_sample_rows_by_entity <- c(
  "All entities" = nrow(weekly_spend_panel),
  "Political parties" = sum(weekly_spend_panel$entity_group == "political_party", na.rm = TRUE),
  "Organizations/people" = sum(weekly_spend_panel$entity_group == "other_org_or_person", na.rm = TRUE)
)

entity_group_label_he <- function(entity_group_values) {
  dplyr::case_when(
    entity_group_values == "other_org_or_person" ~ "גוף פרטי/אזרחי",
    entity_group_values == "political_party" ~ "מפלגה ממוסדת",
    TRUE ~ as.character(entity_group_values)
  )
}

format_integer_he <- function(values) {
  truncated_values <- sign(values) * floor(abs(values))

  ifelse(
    is.na(values),
    NA_character_,
    format(truncated_values, big.mark = ",", scientific = FALSE, trim = TRUE)
  )
}

format_ils_he <- function(values) {
  ifelse(
    is.na(values),
    NA_character_,
    paste0(format_integer_he(values), " ש\"ח")
  )
}

format_ils_decimal_he <- function(values, digits = 1L) {
  ifelse(
    is.na(values),
    NA_character_,
    paste0(
      formatC(
        round(as.numeric(values), digits = digits),
        format = "f",
        digits = digits,
        big.mark = ","
      ),
      " ש\"ח"
    )
  )
}

format_date_he <- function(values) {
  ifelse(
    is.na(values),
    NA_character_,
    format(as.Date(values), "%d/%m/%Y")
  )
}

format_pct_he <- function(values) {
  truncated_values <- sign(values) * floor(abs(values) * 10) / 10

  ifelse(
    is.na(values),
    "-",
    paste0(
      ifelse(values >= 0, "+ ", ""),
      formatC(truncated_values, format = "f", digits = 1),
      "%"
    )
  )
}

format_million_gap_he <- function(values) {
  truncated_millions <- floor(abs(values) / 100000) / 10

  formatted_gap <- ifelse(
    is.na(values),
    NA_character_,
    paste0(
      ifelse(values >= 0, "+", "-"),
      formatC(truncated_millions, format = "f", digits = 1),
      " מיליון ש\"ח"
    )
  )

  dplyr::if_else(
    !is.na(values) & abs(values) < 1000000,
    paste0(formatted_gap, " (שוויון כמעט מוחלט)"),
    formatted_gap
  )
}

build_sample_statistics_table_he <- function(overall_stats) {
  stats_row <- overall_stats %>% dplyr::slice(1)

  tibble::tribble(
    ~"מדד סטטיסטי", ~"ערך", ~"הסבר קצר",
    "תקופת הניתוח",
    paste0(format_date_he(stats_row$analysis_start_week), " - ", format_date_he(stats_row$analysis_end_week)),
    "תאריכי ההתחלה והסיום של פאנל הנתונים",
    "סך תצפיות (N)",
    format_integer_he(stats_row$total_rows),
    "מספר השורות הכולל במסד הנתונים המסונן",
    "סך ישויות (מפרסמים)",
    format_integer_he(stats_row$total_entities),
    "מספר המפלגות והגופים האזרחיים במדגם",
    "סך שבועות (T)",
    format_integer_he(stats_row$total_week_rows),
    "סך השבועות שנמדדו",
    "סך הוצאה כוללת",
    format_ils_he(round(stats_row$total_spend_ils)),
    "סכום ההוצאות המצטבר בכלל המדגם",
    "ממוצע הוצאה שבועית",
    format_ils_decimal_he(stats_row$average_weekly_row_spend_ils, digits = 1L),
    "ההוצאה הממוצעת לשורה שבועית במדגם",
    "חציון הוצאה שבועית",
    format_ils_he(round(stats_row$median_weekly_row_spend_ils)),
    "הערך החציוני של ההוצאה השבועית",
    "סטיית תקן",
    format_ils_decimal_he(stats_row$sd_weekly_row_spend_ils, digits = 1L),
    "מדד פיזור ההוצאות במדגם",
    "מינימום הוצאה",
    format_ils_decimal_he(stats_row$min_weekly_row_spend_ils, digits = 2L),
    "סכום ההוצאה השבועי המינימלי שנרשם",
    "P25 אחוזון 25",
    format_ils_decimal_he(stats_row$p25_weekly_row_spend_ils, digits = 1L),
    "הרבעון התחתון של ההוצאות",
    "P75 אחוזון 75",
    format_ils_decimal_he(stats_row$p75_weekly_row_spend_ils, digits = 1L),
    "הרבעון העליון של ההוצאות",
    "מקסימום הוצאה",
    format_ils_he(round(stats_row$max_weekly_row_spend_ils)),
    "סכום ההוצאה השבועי המקסימלי שנרשם"
  )
}

write_sample_statistics_png <- function(dataframe, file_path, title) {
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)

  width_px <- 1200L
  height_px <- 1180L
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(
      filename = file_path,
      width = width_px,
      height = height_px,
      units = "px",
      res = 150,
      background = "#f6f8ff"
    )
  } else {
    grDevices::png(
      filename = file_path,
      width = width_px,
      height = height_px,
      res = 150,
      bg = "#f6f8ff"
    )
  }
  on.exit(grDevices::dev.off(), add = TRUE)

  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(fill = "#f6f8ff", col = NA))

  left <- 0.035
  right <- 0.965
  top <- 0.955
  bottom <- 0.045
  table_width <- right - left
  row_count <- nrow(dataframe)
  title_height <- 0.082
  header_height <- 0.058
  row_height <- (top - bottom - title_height - header_height) / row_count
  header_y <- top - title_height
  body_top <- header_y - header_height

  grid::grid.roundrect(
    x = (left + right) / 2,
    y = (top + bottom) / 2,
    width = table_width,
    height = top - bottom,
    r = grid::unit(0.018, "npc"),
    gp = grid::gpar(fill = "white", col = "#dbe4f4", lwd = 1)
  )
  grid::grid.roundrect(
    x = (left + right) / 2,
    y = top - title_height / 2,
    width = table_width,
    height = title_height,
    r = grid::unit(0.018, "npc"),
    gp = grid::gpar(fill = "#1f356f", col = "#1f356f", lwd = 0)
  )
  grid::grid.rect(
    x = (left + right) / 2,
    y = top - title_height + 0.006,
    width = table_width,
    height = 0.012,
    gp = grid::gpar(fill = "#1f356f", col = NA)
  )
  grid::grid.text(
    title,
    x = 0.5,
    y = top - title_height / 2,
    gp = grid::gpar(col = "white", fontsize = 15, fontface = "bold")
  )

  grid::grid.rect(
    x = (left + right) / 2,
    y = header_y - header_height / 2,
    width = table_width,
    height = header_height,
    gp = grid::gpar(fill = "#2f4a91", col = NA)
  )

  x_metric <- left + table_width * 0.87
  x_value <- left + table_width * 0.55
  x_note <- left + table_width * 0.04

  grid::grid.text("מדד סטטיסטי", x = x_metric, y = header_y - header_height / 2,
                  just = c("right", "center"),
                  gp = grid::gpar(col = "white", fontsize = 11, fontface = "bold"))
  grid::grid.text("ערך", x = x_value, y = header_y - header_height / 2,
                  just = c("center", "center"),
                  gp = grid::gpar(col = "white", fontsize = 11, fontface = "bold"))
  grid::grid.text("הסבר קצר", x = x_note, y = header_y - header_height / 2,
                  just = c("left", "center"),
                  gp = grid::gpar(col = "white", fontsize = 11, fontface = "bold"))

  for (row_index in seq_len(row_count)) {
    y_center <- body_top - row_height * (row_index - 0.5)
    fill_color <- if (row_index %% 2 == 0) "#eef3fb" else "#ffffff"

    grid::grid.rect(
      x = (left + right) / 2,
      y = y_center,
      width = table_width,
      height = row_height,
      gp = grid::gpar(fill = fill_color, col = "#dbe4f4", lwd = 0.7)
    )

    value_text <- as.character(dataframe[row_index, "ערך", drop = TRUE])
    value_text <- gsub(" ש\"ח", " ₪", value_text, fixed = TRUE)
    if (row_index == 1L) {
      value_text <- gsub(" - ", " -\n", value_text, fixed = TRUE)
    }

    grid::grid.text(
      as.character(dataframe[row_index, "מדד סטטיסטי", drop = TRUE]),
      x = x_metric,
      y = y_center,
      just = c("right", "center"),
      gp = grid::gpar(col = "#1c2f61", fontsize = 11.5, fontface = "bold")
    )
    grid::grid.text(
      value_text,
      x = x_value,
      y = y_center,
      just = c("center", "center"),
      gp = grid::gpar(col = "#2d57b8", fontsize = 12, fontface = "bold", lineheight = 0.9)
    )
    grid::grid.text(
      as.character(dataframe[row_index, "הסבר קצר", drop = TRUE]),
      x = x_note,
      y = y_center,
      just = c("left", "center"),
      gp = grid::gpar(col = "#7180a1", fontsize = 9.5)
    )
  }

  invisible(file_path)
}

# Presentation-ready Hebrew descriptive tables for the seminar paper/slides.
# The audit/source tables remain descriptive_by_group.csv,
# descriptive_by_year.csv, and descriptive_by_year_and_group.csv.
sample_statistics_he <- build_sample_statistics_table_he(overall_spend_stats)

descriptive_entity_type_summary_he <- spend_stats_by_group %>%
  dplyr::mutate(
    entity_group = factor(entity_group, levels = c("other_org_or_person", "political_party"))
  ) %>%
  dplyr::arrange(entity_group) %>%
  dplyr::transmute(
    "סוג גוף" = entity_group_label_he(as.character(entity_group)),
    "סך הוצאה כוללת" = format_ils_he(total_spend_ils),
    "ממוצע שבועי" = format_ils_he(average_weekly_row_spend_ils),
    "חציון שבועי" = format_ils_he(median_weekly_row_spend_ils),
    "סטיית תקן" = format_ils_he(sd_weekly_row_spend_ils),
    "מקסימום לשבוע" = format_ils_he(max_weekly_row_spend_ils),
    "סך תצפיות (N)" = format_integer_he(rows),
    "מס' גופים" = format_integer_he(entities)
  )

descriptive_yearly_summary_he <- yearly_spend_stats %>%
  dplyr::transmute(
    "שנה" = as.character(calendar_year),
    "סך הוצאה כוללת" = format_ils_he(total_spend_ils),
    "שינוי משנה קודמת" = format_pct_he(yoy_total_change_pct),
    "ממוצע שבועי" = format_ils_he(average_weekly_row_spend_ils),
    "חציון שבועי" = format_ils_he(median_weekly_row_spend_ils),
    "סטיית תקן" = format_ils_he(sd_weekly_row_spend_ils),
    "מקסימום לשבוע" = format_ils_he(max_weekly_row_spend_ils),
    "מס' גופים פעילים" = format_integer_he(entities)
  )

descriptive_yearly_group_gap_he <- yearly_spend_stats_by_group %>%
  dplyr::select(calendar_year, entity_group, total_spend_ils) %>%
  tidyr::pivot_wider(
    names_from = entity_group,
    values_from = total_spend_ils,
    values_fill = 0
  ) %>%
  dplyr::mutate(
    civic_minus_party_gap_ils = other_org_or_person - political_party
  ) %>%
  dplyr::arrange(calendar_year) %>%
  dplyr::transmute(
    "שנה" = as.character(calendar_year),
    "סך הוצאות - גוף אזרחי/פרטי" = format_ils_he(other_org_or_person),
    "סך הוצאות - מפלגה ממוסדת" = format_ils_he(political_party),
    "פער (אזרחי פחות מפלגתי)" = format_million_gap_he(civic_minus_party_gap_ils)
  )

descriptive_yearly_group_spend_plot_data <- yearly_spend_stats_by_group %>%
  dplyr::mutate(
    entity_group_he = factor(
      entity_group_label_he(entity_group),
      levels = c("גוף פרטי/אזרחי", "מפלגה ממוסדת")
    ),
    spend_label = paste0(formatC(total_spend_ils / 1000000, format = "f", digits = 1), "M"),
    label_y = total_spend_ils + dplyr::case_when(
      entity_group == "other_org_or_person" ~ 450000,
      entity_group == "political_party" & calendar_year == 2021 ~ -550000,
      TRUE ~ 350000
    )
  )

descriptive_yearly_group_spend_plot <- ggplot2::ggplot(
  descriptive_yearly_group_spend_plot_data,
  ggplot2::aes(
    x = calendar_year,
    y = total_spend_ils,
    color = entity_group_he,
    group = entity_group_he
  )
) +
  ggplot2::geom_line(linewidth = 1.2) +
  ggplot2::geom_point(size = 3.5) +
  ggplot2::geom_text(
    ggplot2::aes(y = label_y, label = spend_label),
    size = 3.2,
    fontface = "bold",
    show.legend = FALSE
  ) +
  ggplot2::scale_x_continuous(breaks = seq(2020, 2025, by = 1)) +
  ggplot2::scale_y_continuous(
    labels = scales::label_number(suffix = " M", scale = 1e-6),
    expand = ggplot2::expansion(mult = c(0.08, 0.12))
  ) +
  ggplot2::scale_color_manual(
    values = c("גוף פרטי/אזרחי" = "#1f77b4", "מפלגה ממוסדת" = "#ff7f0e")
  ) +
  ggplot2::labs(
    title = "הוצאות פרסום ממומן בישראל לפי סוג ישות",
    subtitle = "2020-2025, מבוסס על קבצי הניקוי השני ומסוכם לפי שנים קלנדריות",
    x = "שנה קלנדרית",
    y = "סך הוצאות (במיליוני שקלים)",
    color = "סוג ישות"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = ggplot2::element_text(size = 11, hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA),
    legend.background = ggplot2::element_rect(fill = "white", color = NA)
  )

# -------------------------
# Event-study dataset (all events)
# -------------------------
event_window_panel <- tidyr::crossing(
  weekly_spend_panel %>%
    dplyr::select(
      data_source,
      spend_row_id,
      entity_name,
      entity_group,
      week_start_sunday,
      week_index_since_2020,
      weekly_spend_ils,
      avg_spend_per_day_week,
      currency
    ),
  events_table %>%
    dplyr::select(event_id, event_date, event_week_start_sunday, event_type_group, event_name)
) %>%
  dplyr::mutate(
    relative_week = as.integer((week_start_sunday - event_week_start_sunday) / 7)
  ) %>%
  dplyr::filter(relative_week >= -analysis_window_weeks, relative_week <= analysis_window_weeks)

# Dependent variable matches the agreed spec: log(Spending), not log(1 + Spending).
# log(0) is undefined, so weeks with zero spend are dropped before estimation.
event_window_zero_or_negative_rows <- sum(event_window_panel$weekly_spend_ils <= 0, na.rm = TRUE)
event_window_panel <- event_window_panel %>%
  dplyr::filter(weekly_spend_ils > 0) %>%
  dplyr::mutate(log_weekly_spend = log(weekly_spend_ils))

cat(sprintf(
  "Real-events panel: dropped %s rows with weekly_spend_ils <= 0 (log undefined).\n",
  format(event_window_zero_or_negative_rows, big.mark = ",")
))

if (nrow(event_window_panel) == 0) {
  stop("No rows in event window panel. Check event dates and weekly dates.")
}

event_study_design_overview <- event_window_panel %>%
  dplyr::summarise(
    event_window_weeks = analysis_window_weeks,
    full_descriptive_weekly_rows = nrow(weekly_spend_panel),
    stacked_event_window_rows = dplyr::n(),
    unique_weekly_rows_in_windows = dplyr::n_distinct(spend_row_id),
    extra_rows_from_stacking = stacked_event_window_rows - unique_weekly_rows_in_windows,
    total_entities = dplyr::n_distinct(entity_name),
    total_events = dplyr::n_distinct(event_id),
    first_week = min(week_start_sunday, na.rm = TRUE),
    last_week = max(week_start_sunday, na.rm = TRUE),
    out_of_analysis_window_rows = sum(
      week_start_sunday < analysis_start_week | week_start_sunday > analysis_end_week,
      na.rm = TRUE
    )
  )

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

fit_event_study_specs <- function(input_panel, specifications, reference_week = 0L) {
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
      model = purrr::map(model_data, run_event_study_model, reference_week = reference_week),
      coefficients = purrr::map2(model, model_name, extract_relative_week_coefficients),
      fit = purrr::pmap(list(model, model_name, input_rows, model_data), extract_model_fit)
    )
}

model_results <- fit_event_study_specs(
  input_panel = event_window_panel,
  specifications = model_specifications,
  reference_week = 0L
)

model_results_ref_minus1 <- fit_event_study_specs(
  input_panel = event_window_panel,
  specifications = model_specifications,
  reference_week = -1L
)

all_model_coefficients <- dplyr::bind_rows(model_results$coefficients)
all_model_fit <- dplyr::bind_rows(model_results$fit)

# -------------------------
# gamma_t demonstration regression (kept for the seminar paper)
# -------------------------
# This intentionally fits the canonical TWFE event-study spec WITH calendar-week
# FE on the all_entities_all_events panel:
#   log_y ~ i(relative_week, ref = 0) | entity + data_source + event_id + week_start_sunday
# It is preserved so the paper can show the broken output that motivated dropping
# gamma_t for the stacked specs (Option 2 design decision).
gamma_t_demo_model <- run_event_study_model(
  event_window_panel,
  reference_week = 0L,
  include_gamma_t = TRUE
)
gamma_t_demo_coefficients <- extract_relative_week_coefficients(
  gamma_t_demo_model,
  "all_entities_all_events__with_gamma_t"
)
gamma_t_demo_fit <- extract_model_fit(
  gamma_t_demo_model,
  "all_entities_all_events__with_gamma_t",
  nrow(event_window_panel),
  event_window_panel
)

gamma_t_baseline_compare <- all_model_coefficients %>%
  dplyr::filter(model_name == "all_entities_all_events") %>%
  dplyr::mutate(model_name = "all_entities_all_events__without_gamma_t (paper baseline)")

gamma_t_compare_table <- dplyr::bind_rows(
  gamma_t_baseline_compare,
  gamma_t_demo_coefficients
)

all_model_coefficients_ref_minus1 <- dplyr::bind_rows(model_results_ref_minus1$coefficients)
all_model_fit_ref_minus1 <- dplyr::bind_rows(model_results_ref_minus1$fit)

event_study_key_results_baseline_minus1 <- build_event_study_key_results_matrix(
  coefficients_table = all_model_coefficients_ref_minus1,
  model_fit_table = all_model_fit_ref_minus1,
  model_specifications_table = model_specifications,
  target_relative_week = 1L
)

event_study_panel_summary_baseline_minus1 <- build_regression_panel_summary_he(
  matrix_table = event_study_key_results_baseline_minus1,
  coefficient_row_label = "מקדם שבוע +1 (β)",
  full_sample_rows_by_entity = full_sample_rows_by_entity
)

# -------------------------
# Save event-study figures for every model split
# -------------------------
event_study_figures_directory <- output_paths$event_study_baseline_0_figures

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
    plot_subtitle = "DV: log(weekly spend). Baseline: relative_week = 0"
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
      subtitle = "DV: log(weekly spend). Baseline: relative_week = 0",
      x = "Weeks relative to event week",
      y = "Estimated change vs baseline week (95% CI)"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  ggplot2::ggsave(
    filename = file.path(output_paths$event_study_baseline_0, "event_study_figure_all_models.png"),
    plot = combined_event_study_plot,
    width = 14,
    height = 10,
    dpi = 300
  )
}

event_study_figures_ref_minus1_directory <- output_paths$event_study_baseline_minus1_figures

event_study_coefficients_ref_minus1_for_plot <- tibble::tibble()
if (nrow(all_model_coefficients_ref_minus1) > 0) {
  event_study_coefficients_ref_minus1_for_plot <- purrr::map_dfr(
    split(all_model_coefficients_ref_minus1, all_model_coefficients_ref_minus1$model_name),
    ~ add_baseline_coefficient_row(.x, unique(.x$model_name)[1], baseline_relative_week = -1L)
  )

  for (row_index in seq_len(nrow(model_results_ref_minus1))) {
    current_model_name <- model_results_ref_minus1$model_name[[row_index]]
    current_coefficients <- event_study_coefficients_ref_minus1_for_plot %>%
      dplyr::filter(model_name == current_model_name)

    if (nrow(current_coefficients) == 0) {
      next
    }

    current_plot <- plot_event_study_coefficients(
      coefficients_table = current_coefficients,
      plot_title = paste("Event Study:", current_model_name),
      plot_subtitle = "DV: log(weekly spend). Baseline: relative_week = -1"
    )

    ggplot2::ggsave(
      filename = file.path(event_study_figures_ref_minus1_directory, paste0(current_model_name, ".png")),
      plot = current_plot,
      width = 9,
      height = 5,
      dpi = 300
    )
  }

  combined_event_study_ref_minus1_plot <- ggplot2::ggplot(
    event_study_coefficients_ref_minus1_for_plot,
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
      subtitle = "DV: log(weekly spend). Baseline: relative_week = -1",
      x = "Weeks relative to event week",
      y = "Estimated change vs baseline week (95% CI)"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  ggplot2::ggsave(
    filename = file.path(output_paths$event_study_baseline_minus1, "event_study_figure_all_models.png"),
    plot = combined_event_study_ref_minus1_plot,
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
    filename = file.path(output_paths$correlations_real, "correlation_coefficients_heatmap.png"),
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
    filename = file.path(output_paths$correlations_real, "correlation_scatter_panels.png"),
    plot = correlation_scatter_plot,
    width = 11.5,
    height = 6.2,
    dpi = 300
  )
}

# -------------------------
# Placebo events (single canonical root CSV)
# -------------------------
repo_placebo_dates_path <- "./data/generated/placebo_events_2020_2025.csv"
placebo_boundary_buffer_weeks <- analysis_window_weeks + 1L
placebo_events_table <- load_placebo_events_from_repo(
  placebo_file_path = repo_placebo_dates_path,
  real_events_table = events_table,
  min_gap_weeks = placebo_boundary_buffer_weeks
)

placebo_events_table <- filter_complete_placebo_windows(
  placebo_events_table = placebo_events_table,
  analysis_start_week = analysis_start_week,
  analysis_end_week = analysis_end_week,
  window_weeks = placebo_boundary_buffer_weeks
)

placebo_weekly_event_counts_wide <- build_event_count_wide(placebo_events_table)
placebo_correlation_outputs <- build_correlation_outputs(
  placebo_weekly_event_counts_wide,
  weekly_spend_totals_for_correlation
)
placebo_correlation_summary <- placebo_correlation_outputs$correlation_summary
placebo_correlation_panel_long <- placebo_correlation_outputs$correlation_panel_long
correlation_comparison_table <- build_correlation_comparison_table(
  real_summary = correlation_summary,
  placebo_summary = placebo_correlation_summary
)
correlation_publication_table <- build_correlation_publication_table(correlation_comparison_table)

placebo_event_window_panel <- tidyr::crossing(
  weekly_spend_panel %>%
    dplyr::select(data_source, spend_row_id, entity_name, entity_group, week_start_sunday, weekly_spend_ils),
  placebo_events_table %>%
    dplyr::select(event_id, event_date, event_week_start_sunday, event_type_group, event_name)
) %>%
  dplyr::mutate(
    relative_week = as.integer((week_start_sunday - event_week_start_sunday) / 7)
  ) %>%
  dplyr::filter(relative_week >= -analysis_window_weeks, relative_week <= analysis_window_weeks)

placebo_window_zero_or_negative_rows <- sum(placebo_event_window_panel$weekly_spend_ils <= 0, na.rm = TRUE)
placebo_event_window_panel <- placebo_event_window_panel %>%
  dplyr::filter(weekly_spend_ils > 0) %>%
  dplyr::mutate(log_weekly_spend = log(weekly_spend_ils))

cat(sprintf(
  "Placebo-events panel: dropped %s rows with weekly_spend_ils <= 0 (log undefined).\n",
  format(placebo_window_zero_or_negative_rows, big.mark = ",")
))

placebo_event_study_design_overview <- placebo_event_window_panel %>%
  dplyr::summarise(
    event_window_weeks = analysis_window_weeks,
    full_descriptive_weekly_rows = nrow(weekly_spend_panel),
    stacked_event_window_rows = dplyr::n(),
    unique_weekly_rows_in_windows = dplyr::n_distinct(spend_row_id),
    extra_rows_from_stacking = stacked_event_window_rows - unique_weekly_rows_in_windows,
    total_entities = dplyr::n_distinct(entity_name),
    total_events = dplyr::n_distinct(event_id),
    first_week = min(week_start_sunday, na.rm = TRUE),
    last_week = max(week_start_sunday, na.rm = TRUE),
    out_of_analysis_window_rows = sum(
      week_start_sunday < analysis_start_week | week_start_sunday > analysis_end_week,
      na.rm = TRUE
    )
  )

placebo_model_results <- fit_event_study_specs(
  input_panel = placebo_event_window_panel,
  specifications = model_specifications,
  reference_week = 0L
)

placebo_model_results_ref_minus1 <- fit_event_study_specs(
  input_panel = placebo_event_window_panel,
  specifications = model_specifications,
  reference_week = -1L
)

placebo_model_coefficients <- dplyr::bind_rows(placebo_model_results$coefficients)
placebo_model_fit <- dplyr::bind_rows(placebo_model_results$fit)

placebo_model_coefficients_ref_minus1 <- dplyr::bind_rows(placebo_model_results_ref_minus1$coefficients)
placebo_model_fit_ref_minus1 <- dplyr::bind_rows(placebo_model_results_ref_minus1$fit)

placebo_figures_directory <- output_paths$placebo_event_study_baseline_0_figures

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
      plot_subtitle = "DV: log(weekly spend). Baseline: relative_week = 0"
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
      subtitle = "DV: log(weekly spend). Baseline: relative_week = 0",
      x = "Weeks relative to placebo event week",
      y = "Estimated change vs baseline week (95% CI)"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  ggplot2::ggsave(
    filename = file.path(output_paths$placebo_event_study_baseline_0, "placebo_event_study_figure_all_models.png"),
    plot = combined_placebo_plot,
    width = 14,
    height = 10,
    dpi = 300
  )
}

placebo_figures_ref_minus1_directory <- output_paths$placebo_event_study_baseline_minus1_figures

placebo_coefficients_ref_minus1_for_plot <- tibble::tibble()
if (nrow(placebo_model_coefficients_ref_minus1) > 0) {
  placebo_coefficients_ref_minus1_for_plot <- purrr::map_dfr(
    split(placebo_model_coefficients_ref_minus1, placebo_model_coefficients_ref_minus1$model_name),
    ~ add_baseline_coefficient_row(.x, unique(.x$model_name)[1], baseline_relative_week = -1L)
  )

  for (row_index in seq_len(nrow(placebo_model_results_ref_minus1))) {
    current_model_name <- placebo_model_results_ref_minus1$model_name[[row_index]]
    current_coefficients <- placebo_coefficients_ref_minus1_for_plot %>%
      dplyr::filter(model_name == current_model_name)

    if (nrow(current_coefficients) == 0) {
      next
    }

    current_plot <- plot_event_study_coefficients(
      coefficients_table = current_coefficients,
      plot_title = paste("Placebo Event Study:", current_model_name),
      plot_subtitle = "DV: log(weekly spend). Baseline: relative_week = -1"
    )

    ggplot2::ggsave(
      filename = file.path(placebo_figures_ref_minus1_directory, paste0(current_model_name, ".png")),
      plot = current_plot,
      width = 9,
      height = 5,
      dpi = 300
    )
  }

  combined_placebo_ref_minus1_plot <- ggplot2::ggplot(
    placebo_coefficients_ref_minus1_for_plot,
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
      subtitle = "DV: log(weekly spend). Baseline: relative_week = -1",
      x = "Weeks relative to placebo event week",
      y = "Estimated change vs baseline week (95% CI)"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  ggplot2::ggsave(
    filename = file.path(output_paths$placebo_event_study_baseline_minus1, "placebo_event_study_figure_all_models.png"),
    plot = combined_placebo_ref_minus1_plot,
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
    filename = file.path(output_paths$correlations_placebo, "placebo_correlation_coefficients_heatmap.png"),
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
    filename = file.path(output_paths$correlations_placebo, "placebo_correlation_scatter_panels.png"),
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
oct7_plot <- NULL
oct7_coefficients_ref_minus1_for_plot <- tibble::tibble()
oct7_model_ref_minus1 <- NULL
oct7_ref_minus1_plot <- NULL

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
      plot_subtitle = "DV: log(weekly spend). Baseline: relative_week = 0 (week starts 2023-10-08)"
    )

    ggplot2::ggsave(
      filename = file.path(output_paths$oct7_baseline_0, "event_study_figure_oct7.png"),
      plot = oct7_plot,
      width = 9,
      height = 5,
      dpi = 300
    )
  }

  oct7_model_ref_minus1 <- run_event_study_model(oct7_event_window, reference_week = -1L)

  if (!is.null(oct7_model_ref_minus1)) {
    oct7_coefficients_ref_minus1 <- extract_relative_week_coefficients(
      oct7_model_ref_minus1,
      "oct7_event"
    )
    oct7_coefficients_ref_minus1_for_plot <- add_baseline_coefficient_row(
      oct7_coefficients_ref_minus1,
      "oct7_event",
      baseline_relative_week = -1L
    )

    oct7_ref_minus1_plot <- plot_event_study_coefficients(
      coefficients_table = oct7_coefficients_ref_minus1_for_plot,
      plot_title = "Event Study Around 2023-10-07",
      plot_subtitle = "DV: log(weekly spend). Baseline: relative_week = -1"
    )

    ggplot2::ggsave(
      filename = file.path(output_paths$oct7_baseline_minus1, "event_study_figure_oct7.png"),
      plot = oct7_ref_minus1_plot,
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
oct7_group_results_ref_minus1 <- tibble::tibble()
oct7_group_coefficients_ref_minus1 <- tibble::tibble()
oct7_group_fit_ref_minus1 <- tibble::tibble()
oct7_group_coefficients_ref_minus1_for_plot <- tibble::tibble()

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
      fit = purrr::pmap(list(model, model_name, input_rows, model_data), extract_model_fit)
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
        subtitle = "DV: log(weekly spend). Baseline: relative_week = 0 (week starts 2023-10-08)",
        x = "Weeks relative to event week",
        y = "Estimated change vs baseline week (95% CI)",
        color = "Entity group"
      ) +
      ggplot2::theme_minimal(base_size = 12)

    ggplot2::ggsave(
      filename = file.path(output_paths$oct7_baseline_0, "event_study_figure_oct7_by_group.png"),
      plot = oct7_group_plot,
      width = 9,
      height = 5,
      dpi = 300
    )
  }

  oct7_group_results_ref_minus1 <- oct7_group_specifications %>%
    dplyr::mutate(
      model_data = purrr::map(entity_group_filter, function(group_filter) {
        if (group_filter == "all") {
          oct7_event_window
        } else {
          oct7_event_window %>% dplyr::filter(entity_group == group_filter)
        }
      }),
      input_rows = purrr::map_int(model_data, nrow),
      model = purrr::map(model_data, run_event_study_model, reference_week = -1L),
      coefficients = purrr::map2(model, model_name, extract_relative_week_coefficients),
      fit = purrr::pmap(list(model, model_name, input_rows, model_data), extract_model_fit)
    )

  oct7_group_coefficients_ref_minus1 <- dplyr::bind_rows(oct7_group_results_ref_minus1$coefficients)
  oct7_group_fit_ref_minus1 <- dplyr::bind_rows(oct7_group_results_ref_minus1$fit)

  if (nrow(oct7_group_coefficients_ref_minus1) > 0) {
    oct7_group_coefficients_ref_minus1_for_plot <- purrr::map_dfr(
      split(oct7_group_coefficients_ref_minus1, oct7_group_coefficients_ref_minus1$model_name),
      ~ add_baseline_coefficient_row(.x, unique(.x$model_name)[1], baseline_relative_week = -1L)
    ) %>%
      dplyr::mutate(
        entity_group = dplyr::case_when(
          model_name == "oct7_all_entities" ~ "all_entities",
          model_name == "oct7_political_parties" ~ "political_party",
          model_name == "oct7_other_orgs_people" ~ "other_org_or_person",
          TRUE ~ model_name
        )
      )
  }

  if (nrow(oct7_group_coefficients_ref_minus1_for_plot) > 0) {
    oct7_group_ref_minus1_plot <- ggplot2::ggplot(
      oct7_group_coefficients_ref_minus1_for_plot,
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
        subtitle = "DV: log(weekly spend). Baseline: relative_week = -1",
        x = "Weeks relative to event week",
        y = "Estimated change vs baseline week (95% CI)",
        color = "Entity group"
      ) +
      ggplot2::theme_minimal(base_size = 12)

    ggplot2::ggsave(
      filename = file.path(output_paths$oct7_baseline_minus1, "event_study_figure_oct7_by_group.png"),
      plot = oct7_group_ref_minus1_plot,
      width = 9,
      height = 5,
      dpi = 300
    )
  }
}

# -------------------------
# Save outputs
# -------------------------
write_clean_csv(overall_spend_stats, file.path(output_paths$descriptive, "descriptive_overall.csv"))
write_clean_csv(spend_stats_by_group, file.path(output_paths$descriptive, "descriptive_by_group.csv"))
write_clean_csv(yearly_spend_stats, file.path(output_paths$descriptive, "descriptive_by_year.csv"))
write_clean_csv(yearly_spend_stats_by_group, file.path(output_paths$descriptive, "descriptive_by_year_and_group.csv"))
write_clean_csv(pre_post_oct7_stats, file.path(output_paths$descriptive, "descriptive_pre_post_oct7.csv"))
readr::write_csv(
  sample_statistics_he,
  file.path(output_paths$tables, "sample_statistics_he.csv"),
  na = ""
)
write_markdown_table(
  sample_statistics_he,
  file.path(output_paths$tables, "sample_statistics_he.md")
)
write_html_presentation_table(
  sample_statistics_he,
  file.path(output_paths$tables, "sample_statistics_he.html"),
  title = "טבלה 1: סטטיסטיקה תיאורית - נתוני הוצאות פרסום שבועיות (2020-2025)",
  subtitle = "מבוסס על קבצי הניקוי השני; יחידת הניתוח היא שורת גוף-פלטפורמה-שבוע"
)
write_latex_simple_table(
  sample_statistics_he,
  file.path(output_paths$tables, "sample_statistics_he.tex"),
  caption = "סטטיסטיקה תיאורית - נתוני הוצאות פרסום שבועיות (2020-2025)",
  label = "tab:sample_statistics_he",
  note_text = "הטבלה מבוססת על קבצי הניקוי השני. יחידת הניתוח היא שורת גוף-פלטפורמה-שבוע בטווח השבועות 2020-01-05 עד 2025-12-28."
)
write_sample_statistics_png(
  sample_statistics_he,
  file.path(output_paths$tables, "sample_statistics_he.png"),
  title = "טבלה 1: סטטיסטיקה תיאורית - נתוני הוצאות פרסום שבועיות (2020-2025)"
)
write_sample_statistics_png(
  sample_statistics_he,
  file.path(output_paths$tables, "statistics_table.png"),
  title = "טבלה 1: סטטיסטיקה תיאורית - נתוני הוצאות פרסום שבועיות (2020-2025)"
)
readr::write_csv(
  descriptive_entity_type_summary_he,
  file.path(output_paths$tables, "descriptive_entity_type_summary_he.csv"),
  na = ""
)
write_markdown_table(
  descriptive_entity_type_summary_he,
  file.path(output_paths$tables, "descriptive_entity_type_summary_he.md")
)
write_html_presentation_table(
  descriptive_entity_type_summary_he,
  file.path(output_paths$tables, "descriptive_entity_type_summary_he.html"),
  title = "טבלה א: התפלגות הוצאות לפי סוג ישות (2020-2025)",
  subtitle = "מבוסס על קבצי הניקוי השני; יחידת הניתוח היא הוצאה שבועית לפי גוף ופלטפורמה"
)
readr::write_csv(
  descriptive_yearly_summary_he,
  file.path(output_paths$tables, "descriptive_yearly_summary_he.csv"),
  na = ""
)
write_markdown_table(
  descriptive_yearly_summary_he,
  file.path(output_paths$tables, "descriptive_yearly_summary_he.md")
)
write_html_presentation_table(
  descriptive_yearly_summary_he,
  file.path(output_paths$tables, "descriptive_yearly_summary_he.html"),
  title = "טבלה ב: התפלגות ההוצאות בפרסום לפי שנים קלנדריות (כלל המדגם)",
  subtitle = "הוצאות שבועיות בש\"ח, לפי שבועות Sunday-start בשנים 2020-2025"
)
readr::write_csv(
  descriptive_yearly_group_gap_he,
  file.path(output_paths$tables, "descriptive_yearly_group_gap_he.csv"),
  na = ""
)
write_markdown_table(
  descriptive_yearly_group_gap_he,
  file.path(output_paths$tables, "descriptive_yearly_group_gap_he.md")
)
write_html_presentation_table(
  descriptive_yearly_group_gap_he,
  file.path(output_paths$tables, "descriptive_yearly_group_gap_he.html"),
  title = "טבלה ג: התפלגות ההוצאות בפרסום מפלגתי מול אזרחי (השוואה שנתית)",
  subtitle = "פער חיובי מציין הוצאה גבוהה יותר של גופים אזרחיים/פרטיים לעומת מפלגות ממוסדות"
)
ggplot2::ggsave(
  filename = file.path(output_paths$descriptive, "descriptive_yearly_group_spend_line.png"),
  plot = descriptive_yearly_group_spend_plot,
  width = 9,
  height = 5,
  dpi = 300,
  bg = "white"
)
ggplot2::ggsave(
  filename = file.path(output_paths$descriptive, "descriptive_yearly_group_spend_line.pdf"),
  plot = descriptive_yearly_group_spend_plot,
  width = 9,
  height = 5,
  bg = "white"
)
write_clean_csv(correlation_summary, file.path(output_paths$correlations_real, "correlation_summary.csv"))
write_clean_csv(placebo_correlation_summary, file.path(output_paths$correlations_placebo, "placebo_correlation_summary.csv"))
write_clean_csv(correlation_comparison_table, file.path(output_paths$tables, "correlation_real_vs_placebo.csv"))
write_markdown_table(correlation_comparison_table, file.path(output_paths$tables, "correlation_real_vs_placebo.md"))
write_clean_csv(correlation_publication_table, file.path(output_paths$tables, "correlation_paper_table.csv"))
write_markdown_table(correlation_publication_table, file.path(output_paths$tables, "correlation_paper_table.md"))
write_clean_csv(event_study_design_overview, file.path(output_paths$event_study_baseline_0, "event_study_design_overview.csv"))
write_clean_csv(event_study_design_overview, file.path(output_paths$event_study_baseline_minus1, "event_study_design_overview.csv"))
write_clean_csv(
  placebo_event_study_design_overview,
  file.path(output_paths$placebo_event_study_baseline_0, "placebo_event_study_design_overview.csv")
)
write_clean_csv(
  placebo_event_study_design_overview,
  file.path(output_paths$placebo_event_study_baseline_minus1, "placebo_event_study_design_overview.csv")
)
write_clean_csv(
  event_study_key_results_baseline_minus1,
  file.path(output_paths$tables, "event_study_key_results_baseline_minus1.csv")
)
write_panel_summary_outputs(
  summary_table = event_study_panel_summary_baseline_minus1,
  output_base_path = file.path(output_paths$tables, "event_study_panel_summary_baseline_minus1"),
  title = "טבלת סיכום - Event Study",
  subtitle = "N בטבלת הרגרסיה הוא מספר תצפיות חלון-אירוע מוערמות; שורות המדגם התיאורי מוצגות בנפרד",
  latex_caption = "טבלת סיכום - Event Study",
  latex_label = "tab:event_study_panel_summary_baseline_minus1",
  note_text = "המקדמים מבוססים על קבצי התוצאות האמיתיים של event study, עם שבוע -1 כשבוע הייחוס. N הוא מספר תצפיות חלון-אירוע מוערמות לאחר אומדן המודל; שורות שבועיות ייחודיות והמדגם התיאורי מדווחות בנפרד. טעויות תקן מקובצות לפי מפרסם."
)
write_latex_matrix_table(
  matrix_table = event_study_key_results_baseline_minus1,
  file_path = file.path(output_paths$tables, "event_study_key_results_baseline_minus1.tex"),
  caption = "Event-study estimates by entity group and event type",
  label = "tab:event_study_key_results_baseline_minus1",
  coefficient_label = "Cells report the coefficient for relative week +1, estimated with relative week -1 as the omitted baseline; clustered standard errors are in parentheses.",
  note_text = paste0(
    "Dependent variable is log weekly spending. The omitted baseline is relative week -1. ",
    "All models include entity and data-source fixed effects; stacked models also include event fixed effects. ",
    "Sample is restricted to Sunday-start weeks from ", analysis_start_week, " through ", analysis_end_week, ". ",
    "*** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.10."
  )
)
write_html_matrix_table(
  matrix_table = event_study_key_results_baseline_minus1,
  file_path = file.path(output_paths$tables, "event_study_key_results_baseline_minus1.html"),
  title = "Event-Study Estimates by Entity Group and Event Type",
  subtitle = "Relative week +1 coefficient, using relative week -1 as the baseline; standard errors in parentheses",
  coefficient_label = "Cells report the coefficient for relative week +1, estimated with relative week -1 as the omitted baseline.",
  note_text = paste0(
    "Dependent variable is log weekly spending. The omitted baseline is relative week -1. ",
    "All models include entity and data-source fixed effects; stacked models also include event fixed effects. ",
    "Sample is restricted to Sunday-start weeks from ", analysis_start_week, " through ", analysis_end_week, ". ",
    "*** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.10."
  )
)

write_clean_csv(all_model_coefficients, file.path(output_paths$event_study_baseline_0, "event_study_coefficients_by_model.csv"))
write_clean_csv(all_model_fit, file.path(output_paths$event_study_baseline_0, "event_study_model_fit.csv"))
write_clean_csv(
  all_model_coefficients_ref_minus1,
  file.path(output_paths$event_study_baseline_minus1, "event_study_coefficients_by_model.csv")
)
write_clean_csv(
  all_model_fit_ref_minus1,
  file.path(output_paths$event_study_baseline_minus1, "event_study_model_fit.csv")
)
write_clean_csv(placebo_model_coefficients, file.path(output_paths$placebo_event_study_baseline_0, "placebo_event_study_coefficients_by_model.csv"))
write_clean_csv(placebo_model_fit, file.path(output_paths$placebo_event_study_baseline_0, "placebo_event_study_model_fit.csv"))
write_clean_csv(
  placebo_model_coefficients_ref_minus1,
  file.path(output_paths$placebo_event_study_baseline_minus1, "placebo_event_study_coefficients_by_model.csv")
)
write_clean_csv(
  placebo_model_fit_ref_minus1,
  file.path(output_paths$placebo_event_study_baseline_minus1, "placebo_event_study_model_fit.csv")
)

# Save the gamma_t demonstration outputs (kept for the seminar paper).
write_clean_csv(
  gamma_t_demo_coefficients,
  file.path(output_paths$gamma_t_demonstration, "event_study_coefficients_with_gamma_t.csv")
)
write_clean_csv(
  gamma_t_demo_fit,
  file.path(output_paths$gamma_t_demonstration, "event_study_model_fit_with_gamma_t.csv")
)
write_clean_csv(
  gamma_t_compare_table,
  file.path(output_paths$gamma_t_demonstration, "event_study_coefficients_comparison.csv")
)
writeLines(
  c(
    "gamma_t demonstration -- event study",
    "====================================",
    "",
    "Panel: all_entities_all_events, +/- 2 weeks around every political/terror event.",
    "",
    "Two regressions are reported here side-by-side in",
    "event_study_coefficients_comparison.csv:",
    "",
    "  (A) without_gamma_t  (the spec used in the rest of outputs/analysis/)",
    "      log_y ~ i(relative_week, ref = 0) | entity_name + data_source + event_id",
    "",
    "  (B) with_gamma_t     (the textbook canonical TWFE spec we tried first)",
    "      log_y ~ i(relative_week, ref = 0) |",
    "          entity_name + data_source + event_id + week_start_sunday",
    "",
    "What to point at in the paper:",
    "",
    "  * In (B) the four relative-week coefficients are forced into a single",
    "    linear-in-k pattern: beta_{-2} = -beta_{+2} and beta_{-1} = -beta_{+1},",
    "    with |beta_{+-2}| = 2 |beta_{+-1}|. This is the regression telling us",
    "    only one direction of variation (the linear trend in k) survives the FE",
    "    structure.",
    "  * Standard errors in (B) are inflated by ~300x relative to (A), and",
    "    fixest emits a 'VCOV matrix is not positive semi-definite' warning.",
    "  * In (A) the coefficients are not constrained to be linear in k and the",
    "    standard errors are well-behaved (~0.025-0.030).",
    "",
    "Why this happens:",
    "",
    "  In our stacked design every entity is observed inside the +/-W window of",
    "  every event (the panel is built by crossing(spend, events)), so on any",
    "  given calendar week the relative-week dummies and week_start_sunday FE",
    "  are nearly collinear. Calendar-week FE absorbs the variation that would",
    "  otherwise identify beta_k. Standard practice in the stacked-event-study",
    "  literature is to omit gamma_t in this case, which is what the rest of",
    "  the script does. The single-event October 7 models drop gamma_t for a",
    "  related but stricter reason: with one event, calendar week is one-to-one",
    "  with relative week.",
    "",
    "This folder is a record of the design decision and the empirical",
    "evidence behind it. Do not delete."
  ),
  con = file.path(output_paths$gamma_t_demonstration, "README.txt")
)

if (nrow(oct7_coefficients_for_plot) > 0) {
  write_clean_csv(oct7_coefficients_for_plot, file.path(output_paths$oct7_baseline_0, "event_study_coefs_oct7.csv"))
}
if (nrow(oct7_coefficients_ref_minus1_for_plot) > 0) {
  write_clean_csv(
    oct7_coefficients_ref_minus1_for_plot,
    file.path(output_paths$oct7_baseline_minus1, "event_study_coefs_oct7.csv")
  )
}
if (nrow(oct7_group_coefficients) > 0) {
  write_clean_csv(oct7_group_coefficients, file.path(output_paths$oct7_baseline_0, "event_study_coefs_oct7_by_group.csv"))
}
if (nrow(oct7_group_fit) > 0) {
  write_clean_csv(oct7_group_fit, file.path(output_paths$oct7_baseline_0, "event_study_model_fit_oct7_by_group.csv"))
}
if (nrow(oct7_group_coefficients_ref_minus1) > 0) {
  write_clean_csv(
    oct7_group_coefficients_ref_minus1,
    file.path(output_paths$oct7_baseline_minus1, "event_study_coefs_oct7_by_group.csv")
  )
}
if (nrow(oct7_group_fit_ref_minus1) > 0) {
  write_clean_csv(
    oct7_group_fit_ref_minus1,
    file.path(output_paths$oct7_baseline_minus1, "event_study_model_fit_oct7_by_group.csv")
  )
}

# Legacy October 7 outputs kept under outputs/oct7_legacy/ for backward
# compatibility with earlier seminar drafts. These remain reproducible from
# this script; the richer canonical outputs live under outputs/analysis/.
# The legacy event_study_figure.png was removed because it was byte-identical
# to outputs/analysis/oct7_event_study/baseline_minus1/event_study_figure_oct7.png.
legacy_oct7_dir <- file.path(dirname(output_directory), "oct7_legacy")
dir.create(legacy_oct7_dir, recursive = TRUE, showWarnings = FALSE)

if (nrow(oct7_event) == 1 && exists("oct7_event_window") && nrow(oct7_event_window) > 0) {
  legacy_data_window <- oct7_event_window %>%
    dplyr::transmute(
      source = data_source,
      party_name = entity_name,
      week_start_sunday,
      week_index_since_2020,
      total_spend_week = weekly_spend_ils,
      avg_spend_per_day_week,
      currency,
      rel_week = relative_week,
      y = log_weekly_spend
    ) %>%
    dplyr::arrange(source, party_name, week_start_sunday)

  readr::write_csv(legacy_data_window, file.path(legacy_oct7_dir, "data_window.csv"), na = "")
}

if (nrow(oct7_coefficients_ref_minus1_for_plot) > 0) {
  legacy_oct7_coefficients <- oct7_coefficients_ref_minus1_for_plot %>%
    dplyr::transmute(
      term = dplyr::if_else(
        relative_week == -1L,
        "relative_week::(-1) (baseline)",
        paste0("relative_week::", relative_week)
      ),
      estimate,
      std.error,
      statistic,
      p.value,
      conf.low,
      conf.high,
      rel_week = relative_week
    ) %>%
    dplyr::arrange(rel_week)

  readr::write_csv(legacy_oct7_coefficients, file.path(legacy_oct7_dir, "event_study_coefs.csv"), na = "")
}

if (!is.null(oct7_model_ref_minus1)) {
  legacy_summary_lines <- c(
    "Event Study around 07/10/2023",
    paste("Event week start (Sunday): ", as.character(oct7_event$event_week_start_sunday)),
    sprintf("Window: rel_week in [-%s, %s], baseline = -1", analysis_window_weeks, analysis_window_weeks),
    "",
    capture.output(print(summary(oct7_model_ref_minus1)))
  )

  writeLines(legacy_summary_lines, con = file.path(legacy_oct7_dir, "regression_summary.txt"))
}

# Write textual summaries for easy reading in RStudio and externally
summary_file_path <- file.path(output_paths$summaries, "regression_summary.txt")
summary_connection <- file(summary_file_path, open = "wt")
on.exit(close(summary_connection), add = TRUE)

write_paper_style_header(
  summary_connection = summary_connection,
  title = "Event-Study Regression Summary",
  spec_lines = c(
    "log(Spending_{i,t}) = alpha_i + sum_k beta_k * D_{i,k} + u_{i,t}",
    "",
    "  alpha_i  : entity_name FE (and data_source FE for platform shifts)",
    "  D_{i,k}  : relative-week dummy (relative_week == k); baseline week omitted",
    "  Multi-event (stacked) panels also add event_id FE.",
    "  Single-event runs (Oct 7) use entity_name + data_source FE only.",
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
    "(gamma_t = week_start_sunday FE). In our stacked design every entity is",
    "inside the +/-W window of every event, so calendar-week FE absorbs the",
    "variation that identifies beta_k -- SEs inflate ~300x and the",
    "relative-week coefficients collapse to a linear-in-k pattern. We therefore",
    "omit gamma_t for stacked specs (standard stacked-event-study practice).",
    "A single demonstration run with gamma_t is preserved under",
    "event_study/gamma_t_demonstration/ for the paper.",
    "Count note: descriptive tables count the cleaned weekly spend panel",
    "(entity-platform-week rows). Regression N counts stacked event-window",
    "observations after crossing weekly rows with events and keeping +/-W weeks;",
    "the scripts also report unique weekly rows in model-fit and panel-summary files."
  )
)

writeLines("Correlations (real events)", con = summary_connection)
writeLines(strrep("-", 73L), con = summary_connection)
write_formatted_table(correlation_summary, summary_connection)
writeLines("", con = summary_connection)

writeLines("Correlations (placebo events)", con = summary_connection)
writeLines(strrep("-", 73L), con = summary_connection)
write_formatted_table(placebo_correlation_summary, summary_connection)
writeLines("", con = summary_connection)

writeLines("--- correlation_real_vs_placebo ---", con = summary_connection)
write_formatted_table(correlation_comparison_table, summary_connection)
writeLines("", con = summary_connection)

writeLines("", con = summary_connection)
writeLines(strrep("=", 73L), con = summary_connection)
writeLines("Event-study models -- baseline reference week = 0", con = summary_connection)
writeLines(strrep("=", 73L), con = summary_connection)
write_paper_style_summary(
  model_names = model_results$model_name,
  coefficients_table = all_model_coefficients,
  fit_table = all_model_fit,
  summary_connection = summary_connection,
  include_baseline_marker = 0L
)

writeLines("", con = summary_connection)
writeLines(strrep("=", 73L), con = summary_connection)
writeLines("Event-study models -- baseline reference week = -1 (robustness)", con = summary_connection)
writeLines(strrep("=", 73L), con = summary_connection)
write_paper_style_summary(
  model_names = model_results_ref_minus1$model_name,
  coefficients_table = all_model_coefficients_ref_minus1,
  fit_table = all_model_fit_ref_minus1,
  summary_connection = summary_connection,
  include_baseline_marker = -1L
)

writeLines("", con = summary_connection)
writeLines(strrep("=", 73L), con = summary_connection)
writeLines("Placebo event-study models -- baseline reference week = 0", con = summary_connection)
writeLines(strrep("=", 73L), con = summary_connection)
write_paper_style_summary(
  model_names = placebo_model_results$model_name,
  coefficients_table = placebo_model_coefficients,
  fit_table = placebo_model_fit,
  summary_connection = summary_connection,
  include_baseline_marker = 0L
)

writeLines("", con = summary_connection)
writeLines(strrep("=", 73L), con = summary_connection)
writeLines("Placebo event-study models -- baseline reference week = -1 (robustness)", con = summary_connection)
writeLines(strrep("=", 73L), con = summary_connection)
write_paper_style_summary(
  model_names = placebo_model_results_ref_minus1$model_name,
  coefficients_table = placebo_model_coefficients_ref_minus1,
  fit_table = placebo_model_fit_ref_minus1,
  summary_connection = summary_connection,
  include_baseline_marker = -1L
)

if (nrow(oct7_coefficients_for_plot) > 0) {
  writeLines("", con = summary_connection)
  writeLines(strrep("=", 73L), con = summary_connection)
  writeLines("Dedicated October 7 model (single event) -- baseline 0", con = summary_connection)
  writeLines(strrep("=", 73L), con = summary_connection)
  write_paper_style_model_section(
    model_label = "oct7_event",
    coefficients_table = oct7_coefficients_for_plot %>% dplyr::filter(relative_week != 0L),
    fit_table = if (!is.null(oct7_model)) extract_model_fit(oct7_model, "oct7_event", nrow(oct7_event_window), oct7_event_window) else tibble::tibble(),
    summary_connection = summary_connection,
    include_baseline_marker = 0L
  )
}

if (nrow(oct7_coefficients_ref_minus1_for_plot) > 0) {
  writeLines("", con = summary_connection)
  writeLines(strrep("=", 73L), con = summary_connection)
  writeLines("Dedicated October 7 model (single event) -- baseline -1 (robustness)", con = summary_connection)
  writeLines(strrep("=", 73L), con = summary_connection)
  write_paper_style_model_section(
    model_label = "oct7_event",
    coefficients_table = oct7_coefficients_ref_minus1_for_plot %>% dplyr::filter(relative_week != -1L),
    fit_table = if (!is.null(oct7_model_ref_minus1)) extract_model_fit(oct7_model_ref_minus1, "oct7_event", nrow(oct7_event_window), oct7_event_window) else tibble::tibble(),
    summary_connection = summary_connection,
    include_baseline_marker = -1L
  )
}

if (nrow(oct7_group_results) > 0) {
  writeLines("", con = summary_connection)
  writeLines(strrep("=", 73L), con = summary_connection)
  writeLines("October 7 by entity group -- baseline 0", con = summary_connection)
  writeLines(strrep("=", 73L), con = summary_connection)
  write_paper_style_summary(
    model_names = oct7_group_results$model_name,
    coefficients_table = oct7_group_coefficients,
    fit_table = oct7_group_fit,
    summary_connection = summary_connection,
    include_baseline_marker = 0L
  )
}

if (nrow(oct7_group_results_ref_minus1) > 0) {
  writeLines("", con = summary_connection)
  writeLines(strrep("=", 73L), con = summary_connection)
  writeLines("October 7 by entity group -- baseline -1 (robustness)", con = summary_connection)
  writeLines(strrep("=", 73L), con = summary_connection)
  write_paper_style_summary(
    model_names = oct7_group_results_ref_minus1$model_name,
    coefficients_table = oct7_group_coefficients_ref_minus1,
    fit_table = oct7_group_fit_ref_minus1,
    summary_connection = summary_connection,
    include_baseline_marker = -1L
  )
}

# -------------------------
# Console output for RStudio users
# -------------------------
print_section("Descriptive Statistics (Overall)")
print(format_output_table(overall_spend_stats))

print_section("Descriptive Statistics (By Group)")
print(format_output_table(spend_stats_by_group))

print_section("Descriptive Statistics (By Year)")
print(format_output_table(yearly_spend_stats))

print_section("Presentation Descriptive Tables (Hebrew)")
print(descriptive_entity_type_summary_he)
print(descriptive_yearly_summary_he)
print(descriptive_yearly_group_gap_he)

print_section("Pre/Post October 7 Reference Week")
print(format_output_table(pre_post_oct7_stats))

print_section("Correlation Summary")
print(format_output_table(correlation_summary))

print_section("Regression Model Fit (All Splits)")
print(format_output_table(all_model_fit))

print_section("Regression Model Fit (Baseline -1)")
print(format_output_table(all_model_fit_ref_minus1))

if (nrow(oct7_group_fit) > 0) {
  print_section("October 7 Model Fit (By Group)")
  print(format_output_table(oct7_group_fit))
}

print_section("Output Files")
cat("Saved descriptive stats and regressions to: ", normalizePath(output_directory), "\n", sep = "")
cat("- summaries/regression_summary.txt\n")
cat("- tables/*\n")
cat("- tables/sample_statistics_he.{csv,md,html,tex,png}\n")
cat("- tables/descriptive_*_he.{csv,md,html}\n")
cat("- descriptive/*.csv\n")
cat("- descriptive/descriptive_yearly_group_spend_line.{png,pdf}\n")
cat("- correlations/real_events/*\n")
cat("- correlations/placebo_events/*\n")
cat("- event_study/baseline_0/*\n")
cat("- event_study/baseline_0/figures_by_model/*.png\n")
cat("- event_study/baseline_minus1/*\n")
cat("- event_study/baseline_minus1/figures_by_model/*.png\n")
cat("- placebo_event_study/baseline_0/*\n")
cat("- placebo_event_study/baseline_0/figures_by_model/*.png\n")
cat("- placebo_event_study/baseline_minus1/*\n")
cat("- placebo_event_study/baseline_minus1/figures_by_model/*.png\n")
if (nrow(oct7_coefficients_for_plot) > 0) {
  cat("- oct7_event_study/baseline_0/event_study_coefs_oct7.csv\n")
  cat("- oct7_event_study/baseline_0/event_study_figure_oct7.png\n")
}
if (nrow(oct7_coefficients_ref_minus1_for_plot) > 0) {
  cat("- oct7_event_study/baseline_minus1/event_study_coefs_oct7.csv\n")
  cat("- oct7_event_study/baseline_minus1/event_study_figure_oct7.png\n")
}
if (nrow(oct7_group_coefficients) > 0) {
  cat("- oct7_event_study/baseline_0/event_study_coefs_oct7_by_group.csv\n")
  cat("- oct7_event_study/baseline_0/event_study_model_fit_oct7_by_group.csv\n")
  cat("- oct7_event_study/baseline_0/event_study_figure_oct7_by_group.png\n")
}
if (nrow(oct7_group_coefficients_ref_minus1) > 0) {
  cat("- oct7_event_study/baseline_minus1/event_study_coefs_oct7_by_group.csv\n")
  cat("- oct7_event_study/baseline_minus1/event_study_model_fit_oct7_by_group.csv\n")
  cat("- oct7_event_study/baseline_minus1/event_study_figure_oct7_by_group.png\n")
}
