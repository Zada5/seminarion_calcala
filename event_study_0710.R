# Clear the environment, free memory, and clear the console
rm(list = ls())
gc()
cat("\014")

# =========================
# Event Study around the 07/10/2023 event (weekly level)
#
# What this script does:
#   1. Reads weekly ad-spend data from Google and Meta (Facebook) for Israeli parties
#   2. Focuses on a 5-week window: 2 weeks before the event through 2 weeks after
#   3. Runs an "event study" regression to measure how spending changed
#      relative to Week 0 (the week beginning 08/10/2023, right after the event)
#   4. Prints a summary, a coefficient table, and a plot in RStudio
#
# Week numbering:
#   rel_week = -2  -> two weeks before the event week
#   rel_week = -1  -> one week before the event week
#   rel_week =  0  -> the event week itself (08/10/2023) — this is the BASELINE
#   rel_week = +1  -> one week after
#   rel_week = +2  -> two weeks after
# =========================


# ---- Step 1: Install and load required packages ----
# These packages handle file reading, data manipulation, dates, and plotting.
required_packages <- c("readr", "dplyr", "lubridate", "ggplot2", "fixest", "broom")
packages_to_install <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(packages_to_install) > 0) install.packages(packages_to_install, repos = "https://cloud.r-project.org")

library(readr)      # reading CSV files
library(dplyr)      # data manipulation (filter, mutate, etc.)
library(lubridate)  # working with dates
library(ggplot2)    # plotting
library(fixest)     # fast fixed-effects regression (feols)
library(broom)      # tidy regression output


# ---- Step 2: Set file paths ----
# You can pass custom file paths as command-line arguments,
# or just let the script use the default paths below.
command_line_args <- commandArgs(trailingOnly = TRUE)

google_data_path <- if (length(command_line_args) >= 1) command_line_args[1] else "./cleaned_data/weekly_party_spend_google.csv"
meta_data_path   <- if (length(command_line_args) >= 2) command_line_args[2] else "./cleaned_data/weekly_party_spend_meta.csv"

message("Reading files:")
message(" - Google: ", google_data_path)
message(" - Meta  : ", meta_data_path)


# ---- Step 3: Read the data ----
# This helper function reads a CSV and strips any accidental leading/trailing
# spaces from the column names.
read_weekly_spend <- function(file_path) {
  weekly_data <- readr::read_csv(file_path, show_col_types = FALSE)
  names(weekly_data) <- trimws(names(weekly_data))
  weekly_data
}

google_weekly_spend <- read_weekly_spend(google_data_path)
meta_weekly_spend   <- read_weekly_spend(meta_data_path)

# Combine both platforms into one data frame
combined_ad_spend <- bind_rows(google_weekly_spend, meta_weekly_spend)


# ---- Step 4: Validate that required columns are present ----
required_column_names <- c("source", "party_name", "week_start_sunday", "total_spend_week")
missing_columns <- setdiff(required_column_names, names(combined_ad_spend))
if (length(missing_columns) > 0) stop("Missing required columns: ", paste(missing_columns, collapse = ", "))

# Make sure each column has the correct data type, and remove rows with missing values
combined_ad_spend <- combined_ad_spend %>%
  mutate(
    week_start_sunday = as.Date(week_start_sunday),
    total_spend_week  = as.numeric(total_spend_week),
    source            = as.character(source),
    party_name        = as.character(party_name)
  ) %>%
  filter(!is.na(week_start_sunday), !is.na(total_spend_week))


# ---- Step 5: Define the event and the analysis window ----
# The event we are studying: October 7, 2023
event_date <- as.Date("2023-10-07")

# Week 0 is defined as the week that STARTS on the Sunday immediately after
# the event (08/10/2023). All other weeks are measured relative to this week.
week_zero_start_date <- as.Date("2023-10-08")

# Keep only the 5-week window around the event (rel_week -2 to +2)
event_window_data <- combined_ad_spend %>%
  mutate(
    # How many weeks away from Week 0 is each row?
    rel_week = as.integer((week_start_sunday - week_zero_start_date) / 7),
    # Log-transform spending so the regression measures % changes
    log_spend = log1p(total_spend_week)
  ) %>%
  filter(rel_week >= -2, rel_week <= 2)

if (nrow(event_window_data) == 0) stop("No rows found in the event window. Check week_start_sunday and dates.")


# ---- Step 6: Print a quick overview of the data ----
cat("\n=== Window data overview (counts by source x rel_week) ===\n")
print(event_window_data %>% count(source, rel_week) %>% arrange(source, rel_week))

cat("\n=== Date range in window ===\n")
print(event_window_data %>% summarise(min_week = min(week_start_sunday), max_week = max(week_start_sunday)))

cat("\n=== Parties covered (top 30 by number of observations) ===\n")
print(event_window_data %>% count(party_name, sort = TRUE) %>% head(30))


# ---- Step 7: Run the Event Study regression ----
# We use a fixed-effects regression (feols) with:
#   - Dependent variable: log_spend (log of weekly ad spend)
#   - Independent variables: dummy indicators for each week relative to the event
#     (the i() syntax creates one dummy per week; ref=0 means Week 0 is the baseline)
#   - Fixed effects: party_name and source
#     (these control for each party's typical spending level and platform differences)
#   - Standard errors clustered by party_name
#
# Each estimated coefficient tells us:
#   "How much did log spending differ in that week compared to Week 0?"
event_study_model <- feols(
  log_spend ~ i(rel_week, ref = 0) | party_name + source,
  data  = event_window_data,
  vcov  = ~ party_name
)

cat("\n\n============================\n")
cat("Event Study Regression Summary\n")
cat("Event date: ", as.character(event_date), "\n")
cat("Baseline week (Week 0) starts on: ", as.character(week_zero_start_date), "\n")
cat("Window: rel_week in [-2, 2], baseline = 0\n")
cat("============================\n\n")
print(summary(event_study_model))


# ---- Step 8: Extract coefficients for plotting ----
# broom::tidy() turns the regression output into a clean data frame
event_study_coefficients <- broom::tidy(event_study_model, conf.int = TRUE) %>%
  filter(grepl("^rel_week::", term)) %>%
  mutate(rel_week = as.integer(sub("rel_week::", "", term))) %>%
  arrange(rel_week)

# Week 0 (the baseline) has an effect of exactly 0 by construction — add it manually
baseline_reference_point <- tibble(
  term      = "rel_week::(0) (baseline)",
  estimate  = 0,
  std.error = NA_real_,
  statistic = NA_real_,
  p.value   = NA_real_,
  conf.low  = 0,
  conf.high = 0,
  rel_week  = 0L
)

coefficients_for_plot <- bind_rows(event_study_coefficients, baseline_reference_point) %>%
  arrange(rel_week)

cat("\n=== Event-study coefficients (vs. baseline rel_week=0) ===\n")
print(coefficients_for_plot %>% select(rel_week, estimate, conf.low, conf.high, p.value) %>% arrange(rel_week))


# ---- Step 9: Plot the event study results ----
# The plot shows one point per week. The y-axis is the estimated change in
# log spending compared to Week 0. The error bars are 95% confidence intervals.
# A point at 0 on the y-axis means no change from the baseline week.
event_study_plot <- ggplot(coefficients_for_plot, aes(x = rel_week, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +  # reference line: no change
  geom_vline(xintercept = 0, linetype = "dotted") +  # reference line: event week
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
  scale_x_continuous(breaks = -2:2) +
  labs(
    title    = "Event Study: Effect on Weekly Ad Spend around 07/10/2023",
    subtitle = "Outcome: log(1 + weekly spend). Fixed effects: party + source. Baseline: Week 0 (starts 08/10/2023)",
    x        = "Week relative to event (0 = week starting 08/10/2023)",
    y        = "Estimated change vs. Week 0 (with 95% CI)"
  ) +
  theme_minimal(base_size = 12)

print(event_study_plot)


# ---- Step 10: Sanity check — pooled Pre vs Post regressions ----
# As an additional check (not a replacement for the event study above),
# we run two simple regressions:
#   1. Does spending differ between the two PRE-event weeks and Week 0?
#   2. Does spending differ between the two POST-event weeks and Week 0?
# pre = 1 for weeks -2 and -1 (before the event week)
# post = 1 for weeks +1 and +2 (after the event week)
event_window_data <- event_window_data %>%
  mutate(
    is_pre_event  = as.integer(rel_week < 0),
    is_post_event = as.integer(rel_week > 0)
  )

pre_event_model <- feols(
  log_spend ~ is_pre_event | party_name + source,
  data = event_window_data,
  vcov = ~ party_name
)

post_event_model <- feols(
  log_spend ~ is_post_event | party_name + source,
  data = event_window_data,
  vcov = ~ party_name
)

cat("\n\n============================\n")
cat("Sanity checks vs Week 0 (two separate regressions)\n")
cat("1) is_pre_event  = 1 for rel_week in {-2, -1} (compared to Week 0)\n")
cat("2) is_post_event = 1 for rel_week in {+1, +2} (compared to Week 0)\n")
cat("============================\n\n")

cat("\n--- Pre-event weeks vs Week 0 ---\n")
print(summary(pre_event_model))

cat("\n--- Post-event weeks vs Week 0 ---\n")
print(summary(post_event_model))
