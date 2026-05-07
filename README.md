

# Meta Political Ads – Weekly Spend Aggregation

## Overview

This project provides a **deterministic, reproducible pipeline** for transforming raw Meta Ad Library CSV exports into a **weekly time-series dataset of political advertising spend by party**.

It also includes a companion script for **Google Ads political spend** exports (weekly data already aggregated) that emits the **same output schema**, so both sources can be merged cleanly.

The script is designed to support:

* **Longitudinal analysis** of political ad spending
* **Detection of temporal patterns and structural changes**
* **Downstream statistical, visual, or ML-based analysis**
* **AI agents that need a clear data-generation contract**

The output is a single, clean CSV file where **each row represents one party in one week**, with standardized time indexing.

---

## Setup

Create a virtual environment and install dependencies:

```bash
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

---

## Analysis Scripts

After the weekly Meta and Google files are prepared, the repository includes two R analysis entry points:

* `event_study_0710.R`
  Main descriptive and event-study regression script. Default output directory: `./analysis_outputs`

* `did_0710.R`
  Separate DiD-style post-event fixed-effects regression on the same cleaned weekly inputs. Default output directory: `./analysis_outputs_did`

Both scripts use these default inputs unless command-line arguments override them:

```bash
./second_cleaning/weekly_party_spend_google.csv
./second_cleaning/weekly_party_spend_meta.csv
./Consolidated List of Terror and Political incidents 2020-2025 v3.csv
```

The default analysis sample is restricted to the seminar window: Sunday-start
weeks from `2020-01-05` through `2025-12-28`. Events are also restricted to
the same 2020-2025 window after conversion to the Sunday event week.

Run them from terminal with:

```bash
Rscript event_study_0710.R
Rscript did_0710.R
```

`did_0710.R` uses the same Sunday-start event windows as the event-study script and defines `PostEvent = 1` when `relative_week >= 0`. It also writes an additional robustness version where `PostEvent = 1` starts at `relative_week >= -1`.
It now also runs the same placebo-date design used by `event_study_0710.R`, using `placebo_events_2020_2025.csv` when available and otherwise falling back to the seeded deterministic placebo generator.

### Regression specifications

The seminar plan originally agreed on the textbook canonical TWFE forms

```
event study:  log(Spending_{i,t}) = alpha_i + gamma_t + sum_k beta_k * D_{i,k} + u_{i,t}
DiD:          log(Spending_{i,t}) = alpha_i + gamma_t + beta * PostEvent_{i,t} + u_{i,t}
```

We empirically discovered (see `analysis_outputs/event_study/gamma_t_demonstration/` and `analysis_outputs_did/gamma_t_demonstration/`) that adding the calendar-week fixed effect `gamma_t` (`week_start_sunday` FE) to our **stacked** panels destroys identification: every entity in our data is observed inside the `+/-W` window of every event, so on any given calendar week `PostEvent` and the relative-week dummies have no within-FE variation. Standard errors blow up by ~300x, the relative-week coefficients collapse to a single linear-in-`k` direction, and fixest reports the VCOV is not positive semi-definite. Following standard practice in the stacked event-study and stacked DiD literature, the scripts therefore **omit `gamma_t` for stacked specs**.

The effective specifications used to produce all `analysis_outputs/` and `analysis_outputs_did/` results except the demonstration folders are:

```
event study (stacked):  log(Spending_{i,t}) = alpha_i + sum_k beta_k * D_{i,k} + u_{i,t}
event study (Oct 7):    log(Spending_{i,t}) = alpha_i + sum_k beta_k * D_{i,k} + u_{i,t}
DiD (stacked):          log(Spending_{i,t}) = alpha_i + beta * PostEvent_{i,t} + u_{i,t}
DiD (Oct 7):            log(Spending_{i,t}) = alpha_i + beta * PostEvent_{i,t} + u_{i,t}
```

Implementation details:

- `alpha_i` is the entity fixed effect (`entity_name`; `data_source` FE is also added so platform-level shifts cannot leak into beta).
- `D_{i,k}` is the relative-week dummy (`relative_week == k`), `i()` reference week is `0` for the main run and `-1` for the robustness folder.
- `PostEvent_{i,t}` is `1` for `relative_week >= 0` (and a separate robustness model for `relative_week >= -1`).
- For multi-event (stacked) panels, `event_id` FE is added on top of `alpha_i`.
- Single-event runs (the October 7 dedicated models) use `entity_name + data_source` FE only — there is only one event so adding `event_id` FE would be a singleton and adding `gamma_t` would be collinear with relative week / PostEvent.
- Standard errors are clustered by `entity_name`.

### `gamma_t` demonstration folders (kept for the seminar paper)

Each script also runs **one** dedicated regression with `gamma_t` included on the `all_entities_all_events` panel, written to:

- `analysis_outputs/event_study/gamma_t_demonstration/` — event-study version (with side-by-side comparison CSV)
- `analysis_outputs_did/gamma_t_demonstration/` — DiD version (with side-by-side comparison CSV)

Each folder contains a `README.txt` explaining the result and a `*_comparison.csv` showing the no-`gamma_t` baseline next to the with-`gamma_t` regression so the paper can directly point at the numerical breakdown (huge SEs, linear-in-`k` collapse for event study; `beta` driven to numerical zero with VCOV warning for DiD). These folders are intentionally preserved as a record of the design decision and should not be deleted by future cleanup runs.

### Important: dependent variable is `log(Spending)`, **not** `log(1 + Spending)`

The earlier version of these scripts used `log1p(weekly_spend_ils)` for the dependent variable. That changed the estimand (especially for low-spend / zero-spend weeks) and was inconsistent with the agreed specs. Both scripts now use `log(weekly_spend_ils)` directly. Because `log(0)` is undefined, **rows with `weekly_spend_ils <= 0` are filtered out before estimation**, and each script prints the row count it dropped on stdout. Do not silently revert this to `log1p`.

### Main analysis outputs

`event_study_0710.R` now writes:

* `analysis_outputs/summaries/regression_summary.txt`: readable regression summary covering real event-study, placebo event-study, October 7, and the `baseline_minus1` robustness outputs
* `analysis_outputs/tables/`: paper-style correlation comparison tables (`.csv` and `.md`) including real vs placebo side-by-side outputs, plus `event_study_key_results_baseline_minus1.tex` and `.html` for direct paper use
* `analysis_outputs/descriptive/`: descriptive CSV tables with total, mean, median, standard deviation, min, quartiles, max, row counts, week counts, entity counts, and first/last week
* `analysis_outputs/correlations/real_events/`: real-event correlation CSV + graphs
* `analysis_outputs/correlations/placebo_events/`: placebo-event correlation CSV + graphs
* `analysis_outputs/event_study/baseline_0/`: main event-study CSVs + figures
* `analysis_outputs/event_study/baseline_minus1/`: event-study robustness outputs where `relative_week = -1` is the reference week
* `analysis_outputs/placebo_event_study/baseline_0/` and `analysis_outputs/placebo_event_study/baseline_minus1/`: placebo event-study outputs
* `analysis_outputs/oct7_event_study/baseline_0/` and `analysis_outputs/oct7_event_study/baseline_minus1/`: dedicated 2023-10-07 outputs split as all / political parties / other organizations
* `analysis_outputs/event_study/gamma_t_demonstration/`: paper artefact — single regression with `gamma_t` (calendar-week FE) included on the `all_entities_all_events` panel, plus a `README.txt` and a side-by-side comparison CSV showing why we omit `gamma_t` in the stacked specs (see "Regression specifications" below)
* Root-level legacy October 7 files for older drafts: `data_window.csv`, `event_study_coefs.csv`, `event_study_figure.png`, and `regression_summary.txt`. These are generated by the R script from the same inputs; the canonical, richer versions remain under `analysis_outputs/`.
* Uses the repository placebo list file `placebo_events_2020_2025.csv` (if present) so placebo dates are explicit and shared across collaborators

`did_0710.R` writes DiD outputs into:

* `analysis_outputs_did/summaries/regression_summary.txt`: readable summary covering the main DiD, placebo DiD, `post_from_minus1`, and October 7 split models
* `analysis_outputs_did/tables/`: paper-style DiD tables (`.csv` and `.md`) for real DiD, placebo DiD, and real-vs-placebo comparisons, plus `did_key_results_post_from_0` and `did_key_results_post_from_minus1` exports as `.tex`, `.html`, `.png`, and `.pdf` for direct paper use
* `analysis_outputs_did/post_from_0/`: main DiD design, sample, coefficient, fit, and graph outputs
* `analysis_outputs_did/post_from_minus1/`: robustness version where `PostEvent = 1` starts at `relative_week >= -1`
* `analysis_outputs_did/placebo/post_from_0/` and `analysis_outputs_did/placebo/post_from_minus1/`: placebo DiD outputs on the canonical placebo weeks
* `analysis_outputs_did/oct7/post_from_0/` and `analysis_outputs_did/oct7/post_from_minus1/`: dedicated 2023-10-07 DiD outputs
* `analysis_outputs_did/gamma_t_demonstration/`: paper artefact — single DiD regression with `gamma_t` (calendar-week FE) included, plus a `README.txt` and side-by-side comparison CSV showing β driven to numerical zero (see "Regression specifications" above)

Numeric outputs in generated CSV and summary text files are formatted to 3 decimal places where relevant, without scientific notation. Very small p-values are displayed as `<0.001` rather than `0.000`. Correlation summaries now also include Pearson-test p-values. The root-level legacy October 7 files are kept only for backward compatibility and must stay script-generated.

---

## Problem Definition

Meta Ad Library data is:

* Delivered as **many separate CSV files**
* Each file corresponds to a **manual download at a specific date**
* Spend is reported as **ranges**, not exact values
* Ads run over **arbitrary multi-day intervals**
* No built-in weekly aggregation exists

The goal is to:

> Convert these raw files into a **weekly spend panel**, aligned across parties and time.

---

## Core Assumptions (Explicit & Intentional)

These assumptions are **deliberate design choices** and should be preserved unless explicitly revised:

1. **Spend is evenly distributed per day**
   If an ad ran from `start_date` to `end_date`, its spend is assumed to be:

   ```
   daily_spend = midpoint(spend_range) / number_of_days
   ```

2. **Ongoing ads end at download date**
   If `ad_delivery_stop_time` is missing, the ad is assumed to have run **until the date the file was downloaded**.

3. **Weekly unit = Sunday–Saturday**

   * Each week starts on **Sunday**
   * This is consistent across the entire dataset

4. **Midpoint of spend range is used**
   For Meta’s reported spend:

   ```
   "lower_bound: X, upper_bound: Y" → (X + Y) / 2
   ```

5. **No deduplication across files (by default)**
   The script assumes:

   * Each CSV is authoritative
   * Duplicate ads across multiple downloads are possible
     (This can be added if needed, but is not enabled by default.)

---

## Input Data Contract

### File-level assumptions

* All files are Meta Ad Library CSV exports
* All files are placed in **one directory**
* Filenames include:

  ```
  <party_name>-YYYY-MM-DD.csv
  ```

Examples:

```
Likud-2026-01-04.csv
LaborParty-2026-01-04_2.csv
```

The **party name is inferred from the filename**, not from the internal Meta page name.

---

### Required CSV columns

The script expects (Meta standard):

| Column name              | Purpose                        |
| ------------------------ | ------------------------------ |
| `ad_delivery_start_time` | Ad start date                  |
| `ad_delivery_stop_time`  | Ad end date (may be empty)     |
| `spend`                  | Spend range string             |
| `currency`               | Optional; tracked for warnings |

If these columns change, the script must be updated.

---

## Output Data Contract

The script produces a **single CSV file** with the following schema:

| Column                   | Description                               |
| ------------------------ | ----------------------------------------- |
| `source`                 | `"meta"`                                  |
| `party_name`             | Party identifier (from filename)          |
| `week_start_sunday`      | ISO date of the Sunday starting that week |
| `week_index_since_2020`  | Sequential week number since 2020-01-05   |
| `total_spend_week`       | Total estimated spend in that week        |
| `avg_spend_per_day_week` | `total_spend_week / 7`                    |
| `currency`               | Spend currency (Meta outputs in ILS)      |

Each row represents:

> **One political party × one calendar week**

---

## Processing Logic (Step-by-Step)

For each CSV file:

1. **Extract metadata**

   * Party name from filename
   * Download date from filename

2. **Iterate over ads**

   * Parse start / stop dates
   * Parse spend range midpoint

3. **Compute daily spend**

   ```
   daily_spend = midpoint_spend / total_ad_days
   ```

4. **Allocate spend into weeks**

   * Determine all weeks overlapped by the ad
   * For each week:

     * Calculate number of overlapping days
     * Add `daily_spend × overlap_days` to that week

5. **Aggregate**

   * Sum spend by `(party_name, week_start_sunday)`

6. **Normalize**

   * Compute `week_index_since_2020`
   * Compute average daily spend per week

---

## Why This Structure Works for Analysis

* **Time-aligned across parties**
* **Stable weekly index** (not ISO-week dependent)
* **Additive & composable**
* **Explicit assumptions → interpretable results**
* **Easy to join with polling, events, media data**

This makes it suitable for:

* Change-point detection
* Pre/post event analysis
* Trend comparison across parties
* Feeding into causal or predictive models

---

## Google Ads Companion Script

Use `google_csvs_to_final_file.py` to convert the cleaned Google Ads XLSX export into the **same weekly schema** as the Meta output. It expects a sheet containing:

| Column name       | Purpose                                 |
| ----------------- | --------------------------------------- |
| `Advertiser_Name` | Party identifier                        |
| `Week_Start_Date` | Week start date (Excel serial or date)  |
| `Spend_ILS`       | Total spend per week (ILS)              |

The output file is `cleaned_data/weekly_party_spend_google.csv`, with the same `week_index_since_2020` logic as the Meta pipeline.

## For AI Agents (Important)

If you are an AI system using or extending this script:

* **Do not silently change assumptions**

* Any change to:

  * Spend allocation
  * Week definition
  * Deduplication logic
    must be treated as a **new data-generating process**

* This script defines a **canonical transformation** from Meta raw data → weekly panel

* All downstream reasoning should assume this transformation unless explicitly overridden

---

## Extensibility

Common extensions that can be added safely:

* Deduplication by `ad_archive_id`
* Support for Google Ads data
* Party name normalization table
* Currency conversion
* Daily (instead of weekly) output

---

## Usage

1. Place all Meta CSV files in one folder
2. Update:

   ```python
   input_folder = "./meta_csvs"
   ```
3. Run:

   ```bash
   python build_weekly_spend.py
   ```
4. Use `cleaned_data/weekly_party_spend_meta.csv` for analysis

---

## Final Note

This script prioritizes:

> **Transparency over cleverness**

Every number in the output can be traced back to:

* A specific ad
* A specific date range
* A specific modeling assumption

That is intentional.

If you want, I can also:

* Add a **methodology appendix**
* Write a **data limitations section**
* Produce a **schema.json** for automated validation
* Adapt this README for an academic paper or report
