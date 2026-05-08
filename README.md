

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

For the full repo-level lineage of raw files, cleaning stages, scripts, and outputs, start with:

* [`DATA_PIPELINE.md`](DATA_PIPELINE.md) - complete map from raw Meta/Google/event inputs through cleaning, analysis scripts, and output folders

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

After the weekly Meta and Google files are prepared, the repository includes three R analysis entry points:

* `summarize_platform_publisher_spend.R`
  Small descriptive-only script that summarizes total spend by platform and by publisher. It validates that the seminar-window publisher universe contains exactly 68 publishers. Default output directory: `./analysis_outputs/descriptive`

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

The weekly files in `second_cleaning/` are the preferred seminar inputs after manual cleaning and hygiene checks. Earlier script-produced weekly files live in `first_cleaning/`, and older cleaned files remain in `cleaned_data/` for compatibility. See [`DATA_PIPELINE.md`](DATA_PIPELINE.md) for the complete file lineage.

The default analysis sample is restricted to the seminar window: Sunday-start
weeks from `2020-01-05` through `2025-12-28`. Events are also restricted to
the same 2020-2025 window after conversion to the Sunday event week.

Run them from terminal with:

```bash
Rscript summarize_platform_publisher_spend.R
Rscript event_study_0710.R
Rscript did_0710.R
```

`summarize_platform_publisher_spend.R` is intentionally separate from the regression scripts. It reads only the two second-cleaning weekly spend files, uses the same 2020-2025 seminar window, and writes clear descriptive tables answering how much was spent on Google, how much was spent on Meta, and how those totals split across the 68 publishers.

Optional full argument form:

```bash
Rscript summarize_platform_publisher_spend.R <google_csv> <meta_csv> <output_dir>
```

`did_0710.R` uses the same Sunday-start event windows as the event-study script and defines `PostEvent = 1` when `relative_week >= 0`.
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
- `D_{i,k}` is the event-study relative-week dummy (`relative_week == k`), `i()` reference week is `0` for the main event-study run and `-1` for the event-study robustness folder.
- `PostEvent_{i,t}` is `1` for `relative_week >= 0`.
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

`summarize_platform_publisher_spend.R` writes:

* `analysis_outputs/descriptive/platform_spend_summary.csv`: Google vs Meta total spend, publisher counts, rows, active weeks, and weekly-row spend statistics
* `analysis_outputs/descriptive/publisher_platform_spend_summary.csv`: one row per publisher, with Google spend, Meta spend, combined spend, platform shares, row counts, active weeks, and weekly-row spend statistics
* `analysis_outputs/descriptive/publisher_count_validation.csv`: explicit check that the analysis sample contains exactly 68 publishers
* `analysis_outputs/descriptive/publisher_group_platform_spend_summary.csv`: platform totals split by `political_party` vs `other_org_or_person`

`event_study_0710.R` now writes:

* `analysis_outputs/summaries/regression_summary.txt`: readable regression summary covering real event-study, placebo event-study, October 7, and the `baseline_minus1` robustness outputs
* `analysis_outputs/tables/`: paper-style correlation comparison tables (`.csv` and `.md`) including real vs placebo side-by-side outputs, plus event-study regression tables for direct paper use
  * `correlation_paper_table.csv` / `.md`: final paper-style weekly correlation table
  * `correlation_real_vs_placebo.csv` / `.md`: side-by-side real-vs-placebo weekly correlation comparison
  * `event_study_key_results_baseline_minus1.csv` / `.tex` / `.html`: script-generated event-study key results, with `relative_week = -1` as the reference week and the reported coefficient taken from `relative_week = +1`
  * `event_study_panel_summary_baseline_minus1.csv` / `.md` / `.html` / `.tex` / `.xlsx`: presentation copy of the same real event-study key results in the compact three-column panel format used for the seminar table
* `analysis_outputs/descriptive/`: dedicated descriptive-output folder with CSV tables for total spend, group/year summaries, pre/post October 7 summaries, mean, median, standard deviation, min, quartiles, max, row counts, week counts, entity counts, and first/last week. These files are produced by the descriptive R step and can also be refreshed by the descriptive block in `event_study_0710.R`.
* `analysis_outputs/correlations/real_events/`: real-event correlation CSV + graphs
* `analysis_outputs/correlations/placebo_events/`: placebo-event correlation CSV + graphs
* `analysis_outputs/event_study/baseline_0/`: main event-study CSVs + figures
* `analysis_outputs/event_study/baseline_minus1/`: event-study robustness outputs where `relative_week = -1` is the reference week
* `analysis_outputs/placebo_event_study/baseline_0/` and `analysis_outputs/placebo_event_study/baseline_minus1/`: placebo event-study outputs
* `analysis_outputs/oct7_event_study/baseline_0/` and `analysis_outputs/oct7_event_study/baseline_minus1/`: dedicated 2023-10-07 outputs split as all / political parties / other organizations
* `analysis_outputs/event_study/gamma_t_demonstration/`: paper artefact — single regression with `gamma_t` (calendar-week FE) included on the `all_entities_all_events` panel, plus a `README.txt` and a side-by-side comparison CSV showing why we omit `gamma_t` in the stacked specs (see "Regression specifications" below)
* Root-level legacy October 7 files for older drafts: `data_window.csv`, `event_study_coefs.csv`, `event_study_figure.png`, and `regression_summary.txt`. These are generated by the R script from the same inputs; the canonical, richer versions remain under `analysis_outputs/`.
* Uses the repository placebo list file `placebo_events_2020_2025.csv` (if present) so placebo dates are explicit and shared across collaborators. Placebo weeks are kept inside the analysis sample far enough to have the full `+/-2` regression window.

`did_0710.R` writes DiD outputs into:

* `analysis_outputs_did/summaries/regression_summary.txt`: readable summary covering the main DiD, placebo DiD, and October 7 split models
* `analysis_outputs_did/tables/`: paper-style DiD tables (`.csv` and `.md`) for real DiD, placebo DiD, and real-vs-placebo comparisons, plus compact DiD tables for direct paper use
  * `did_paper_table_post_from_0.csv` / `.md`: paper-style main DiD table by model split
  * `placebo_did_paper_table_post_from_0.csv` / `.md`: paper-style placebo DiD table
  * `did_real_vs_placebo_post_from_0.csv` / `.md`: side-by-side real-vs-placebo DiD comparison
  * `did_key_results_post_from_0.csv` / `.tex` / `.html` / `.png` / `.pdf`: script-generated DiD key results, where `PostEvent = 1` for `relative_week >= 0`
  * `did_panel_summary_post_from_0.csv` / `.md` / `.html` / `.tex` / `.xlsx`: presentation copy of the same real DiD key results in the compact three-column panel format used for the seminar table
* `analysis_outputs_did/post_from_0/`: main DiD design, sample, coefficient, fit, and graph outputs
* `analysis_outputs_did/placebo/post_from_0/`: placebo DiD outputs on the canonical placebo weeks with complete `+/-2` regression windows
* `analysis_outputs_did/oct7/post_from_0/`: dedicated 2023-10-07 DiD outputs
* `analysis_outputs_did/gamma_t_demonstration/`: paper artefact — single DiD regression with `gamma_t` (calendar-week FE) included, plus a `README.txt` and side-by-side comparison CSV showing β driven to numerical zero (see "Regression specifications" above)

Numeric outputs in generated CSV and summary text files are formatted to 3 decimal places where relevant, without scientific notation. Very small p-values are displayed as `<0.001` rather than `0.000`. Correlation summaries now also include Pearson-test p-values. The root-level legacy October 7 files are kept only for backward compatibility and must stay script-generated.

### Which final table should I use?

For the seminar paper / presentation, use these compact final tables first:

| Purpose | Final table to open | Canonical source data |
| --- | --- | --- |
| Event-study regression table | `analysis_outputs/tables/event_study_panel_summary_baseline_minus1.xlsx` | `analysis_outputs/tables/event_study_key_results_baseline_minus1.csv` |
| Difference-in-differences regression table | `analysis_outputs_did/tables/did_panel_summary_post_from_0.xlsx` | `analysis_outputs_did/tables/did_key_results_post_from_0.csv` |
| Weekly correlation summary | `analysis_outputs/tables/correlation_paper_table.csv` or `.md` | `analysis_outputs/correlations/real_events/correlation_summary.csv` and `analysis_outputs/correlations/placebo_events/placebo_correlation_summary.csv` |

The `.xlsx` files are the human-friendly versions that match the requested seminar table shape. The `.csv` files listed under "Canonical source data" are the audit trail: use them when checking exactly which regression/correlation values fed the final table. For LaTeX or HTML drafts, use the matching `.tex` or `.html` file with the same base name.

### Output lineage: what creates what?

| Step | Input | Script / process | Main output |
| --- | --- | --- | --- |
| Meta weekly aggregation | `meta_csvs/*.csv` | `meta_csvs_to_final_file.py` | `cleaned_data/weekly_party_spend_meta.csv` |
| Google weekly aggregation | `google_csv/*.xlsx` / raw Google export | `google_csvs_to_final_file.py` | `cleaned_data/weekly_party_spend_google.csv` |
| Manual cleaning | `cleaned_data/` and manual review | first/second cleaning process | `first_cleaning/*.csv`, then preferred inputs in `second_cleaning/*.csv` |
| Main event-study analysis | `second_cleaning/*.csv` + event timeline + placebo file | `Rscript event_study_0710.R` | `analysis_outputs/` |
| Real/placebo weekly correlations | Same inputs as event study | `event_study_0710.R` correlation block | `analysis_outputs/correlations/*` and `analysis_outputs/tables/correlation_*` |
| Event-study regression figures and tables | Same inputs as event study | `event_study_0710.R` event-study block | `analysis_outputs/event_study/*` and `analysis_outputs/tables/event_study_key_results_baseline_minus1.*` |
| Compact event-study presentation table | `event_study_key_results_baseline_minus1.csv` | table-formatting step | `analysis_outputs/tables/event_study_panel_summary_baseline_minus1.*` |
| DiD analysis | `second_cleaning/*.csv` + event timeline + placebo file | `Rscript did_0710.R` | `analysis_outputs_did/` |
| DiD regression figures and tables | Same inputs as DiD | `did_0710.R` DiD block | `analysis_outputs_did/post_from_0/*` and `analysis_outputs_did/tables/did_key_results_post_from_0.*` |
| Compact DiD presentation table | `did_key_results_post_from_0.csv` | table-formatting step | `analysis_outputs_did/tables/did_panel_summary_post_from_0.*` |

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

The default output file is `first_cleaning/weekly_party_spend_google.csv`, with the same `week_index_since_2020` logic as the Meta pipeline. The preferred analysis input after later manual cleaning is `second_cleaning/weekly_party_spend_google.csv`.

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
   python3 meta_csvs_to_final_file.py
   ```
4. Review the generated `first_cleaning/weekly_party_spend_meta.csv`
5. Use `second_cleaning/weekly_party_spend_meta.csv` for the default seminar analysis after manual cleaning

---

## Final Note

This script prioritizes:

> **Transparency over cleverness**

Every number in the output can be traced back to:

* A specific ad
* A specific date range
* A specific modeling assumption

That is intentional.

For a higher-level file-by-file explanation of the full project workflow, see [`DATA_PIPELINE.md`](DATA_PIPELINE.md).
