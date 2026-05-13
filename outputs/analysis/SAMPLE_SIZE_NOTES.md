# Sample-size conventions across output files

All files below are built from the **same cleaned weekly panel** with the **same filters**. Different `N` values reflect different units of aggregation, not different data sources.

| N | File / column | Unit |
|---|---|---|
| **9,549** | `descriptive/descriptive_overall.csv` → `total_rows`<br>`event_study/*/event_study_design_overview.csv` → `full_descriptive_weekly_rows` | Unique entity-week rows in the panel |
| **3,888** | `event_study/*/event_study_design_overview.csv` → `stacked_event_window_rows` | Rows after stacked event-study construction around the 25 v4 events. Entity-weeks inside multiple ±k event windows are duplicated (one copy per event). `extra_rows_from_stacking = 963`. |
| **3,887** | `event_study/*/event_study_model_fit.csv` → `used_rows` | 3,888 minus 1 row dropped by `fixest` as singleton FE / collinear |
| **11,355** | `placebo_event_study/*/placebo_event_study_model_fit.csv` → `used_rows` | Placebo event-study all-entities model on 75 placebo events |
| **313** | `correlations/*/correlation_summary.csv` → `weeks_in_sample` | Weekly time-series length (panel collapsed to one row per ISO week) |

**Cross-check:** `descriptive_overall.csv:total_rows (9,549) == event_study_design_overview.csv:full_descriptive_weekly_rows (9,549)`. Identical → no hidden filter difference.

See `README.md` (sections "Sample Size Conventions" + Hebrew explanation) for full discussion.
