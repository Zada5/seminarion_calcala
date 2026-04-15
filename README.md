

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

Run them from terminal with:

```bash
Rscript event_study_0710.R
Rscript did_0710.R
```

`did_0710.R` uses the same Sunday-start event windows as the event-study script and defines `PostEvent = 1` when `relative_week >= 0`.

### Main analysis outputs

`event_study_0710.R` now writes:

* Real-event correlation tables and graphs (`correlation_summary.csv`, `correlation_coefficients_heatmap.png`, `correlation_scatter_panels.png`)
* Placebo event dates + placebo correlations (`placebo_events_dates.csv`, `placebo_correlation_summary.csv`, placebo correlation graphs)
* Real and placebo event-study model outputs (CSV + per-model/combined figures)
* Dedicated 2023-10-07 outputs split as all / political parties / other organizations (`event_study_coefs_0710_all_party_org.csv`)

`did_0710.R` writes DiD outputs into `analysis_outputs_did/`, including dedicated 2023-10-07 all / political parties / other organizations coefficients in:

* `did_coefs_0710_all_party_org.csv`

Numeric outputs in generated CSV files are rounded to 3 decimal places for readability.

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
