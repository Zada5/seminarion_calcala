rm(list=ls())
gc()
cat("\014")

# =========================
# Event Study סביב 07/10/2023 (רמת שבוע)
# חלון: שבועיים לפני ושבועיים אחרי
# Week 0 = השבוע שמתחיל ב-08/10/2023
# baseline להשוואה = Week 0
# פלט: מוצג ב-RStudio (summary + גרף + טבלאות)
# =========================

# ---- packages (install if missing) ----
pkgs <- c("readr", "dplyr", "lubridate", "ggplot2", "fixest", "broom")
missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) install.packages(missing, repos = "https://cloud.r-project.org")

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(fixest)
library(broom)

# ---- file paths ----
args <- commandArgs(trailingOnly = TRUE)

google_path <- if (length(args) >= 1) args[1] else "./cleaned_data/weekly_party_spend_google.csv"
meta_path   <- if (length(args) >= 2) args[2] else "./cleaned_data/weekly_party_spend_meta.csv"

message("Reading files:")
message(" - Google: ", google_path)
message(" - Meta  : ", meta_path)

# ---- read data ----
read_weekly <- function(path) {
  df <- readr::read_csv(path, show_col_types = FALSE)
  names(df) <- trimws(names(df))
  df
}

g <- read_weekly(google_path)
m <- read_weekly(meta_path)

df <- bind_rows(g, m)

# ---- sanity checks ----
required_cols <- c("source","party_name","week_start_sunday","total_spend_week")
miss_cols <- setdiff(required_cols, names(df))
if (length(miss_cols) > 0) stop("Missing required columns: ", paste(miss_cols, collapse = ", "))

df <- df %>%
  mutate(
    week_start_sunday = as.Date(week_start_sunday),
    total_spend_week  = as.numeric(total_spend_week),
    source            = as.character(source),
    party_name        = as.character(party_name)
  ) %>%
  filter(!is.na(week_start_sunday), !is.na(total_spend_week))

# ---- define event & window ----
event_date <- as.Date("2023-10-07")

# Week 0 מוגדר כ"שבוע שמתחיל ביום ראשון אחרי האירוע"
event_week_start_sunday <- as.Date("2023-10-08")

df_window <- df %>%
  mutate(
    rel_week = as.integer((week_start_sunday - event_week_start_sunday) / 7),
    y = log1p(total_spend_week)  # dependent variable
  ) %>%
  filter(rel_week >= -2, rel_week <= 2)

if (nrow(df_window) == 0) stop("No rows found in the event window. Check week_start_sunday and dates.")

# ---- quick overview in RStudio ----
cat("\n=== Window data overview (counts by source x rel_week) ===\n")
print(df_window %>% count(source, rel_week) %>% arrange(source, rel_week))

cat("\n=== Date range in window ===\n")
print(df_window %>% summarise(min_week = min(week_start_sunday), max_week = max(week_start_sunday)))

cat("\n=== Parties covered (top 30 by obs) ===\n")
print(df_window %>% count(party_name, sort = TRUE) %>% head(30))

# ---- regression (Event Study style) ----
# baseline = Week 0 (rel_week = 0)
# כלומר: כל מקדם אומר "השינוי ביחס לשבוע שמתחיל ב-08/10/2023"
est <- feols(
  y ~ i(rel_week, ref = 0) | party_name + source,
  data = df_window,
  vcov = ~ party_name
)

cat("\n\n============================\n")
cat("Event Study Regression Summary\n")
cat("Event date: ", as.character(event_date), "\n")
cat("Week 0 (baseline) defined as week_start_sunday = ", as.character(event_week_start_sunday), "\n")
cat("Window: rel_week in [-2, 2], baseline = 0\n")
cat("============================\n\n")
print(summary(est))

# ---- coefficients for plot ----
coefs <- broom::tidy(est, conf.int = TRUE) %>%
  filter(grepl("^rel_week::", term)) %>%
  mutate(rel_week = as.integer(sub("rel_week::", "", term))) %>%
  arrange(rel_week)

# Add baseline point (week 0) for plotting (0 effect by definition)
baseline_row <- tibble(
  term = "rel_week::(0) (baseline)",
  estimate = 0,
  std.error = NA_real_,
  statistic = NA_real_,
  p.value = NA_real_,
  conf.low = 0,
  conf.high = 0,
  rel_week = 0L
)

coefs_plot <- bind_rows(coefs, baseline_row) %>%
  arrange(rel_week)

cat("\n=== Event-study coefficients (vs. baseline rel_week=0) ===\n")
print(coefs_plot %>% select(rel_week, estimate, conf.low, conf.high, p.value) %>% arrange(rel_week))

# ---- plot in RStudio ----
p <- ggplot(coefs_plot, aes(x = rel_week, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
  scale_x_continuous(breaks = -2:2) +
  labs(
    title = "Event Study: effect on weekly spend סביב 07/10/2023",
    subtitle = "DV: log(1 + total_spend_week). FE: party + source. Baseline: rel_week=0 (week starts 08/10/2023)",
    x = "שבוע יחסית לאירוע (0 = השבוע שמתחיל 08/10/2023)",
    y = "אומדן שינוי לעומת Week 0 עם 95% CI"
  ) +
  theme_minimal(base_size = 12)

print(p)

# ---- optional sanity check: pooled Post vs baseline week 0 (simple, not event study) ----
# כאן post=1 לשבועות אחרי הבייסליין (1,2), pre=1 לשבועות לפני (-2,-1)
# זה רק בדיקה אינטואיטיבית נוספת, לא תחליף ל-event study
df_window <- df_window %>%
  mutate(
    pre  = as.integer(rel_week < 0),
    post = as.integer(rel_week > 0)
  )

est_pre <- feols(
  y ~ pre | party_name + source,
  data = df_window,
  vcov = ~ party_name
)

est_post <- feols(
  y ~ post | party_name + source,
  data = df_window,
  vcov = ~ party_name
)

cat("\n\n============================\n")
cat("Sanity checks vs Week 0 (two separate regressions)\n")
cat("1) pre = 1 for rel_week in {-2,-1} (compared to week 0)\n")
cat("2) post = 1 for rel_week in {+1,+2} (compared to week 0)\n")
cat("============================\n\n")

cat("\n--- Pre vs Week 0 ---\n")
print(summary(est_pre))

cat("\n--- Post vs Week 0 ---\n")
print(summary(est_post))
