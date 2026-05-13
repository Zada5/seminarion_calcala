# AGENTS Guide for `seminarion_calcala`

## Project purpose
This repository supports a bachelor-level economics seminar that studies the relationship between political/social-media ad spending in Israel and major events over time.

Main research question:
- How strongly is weekly ad spending associated with political and terror events?

## Data scope
- Sources: Meta Ads Library and Google Ads Library
- Data available in cleaned weekly spend files: approximately 2018 through 2026 (depending on platform/export)
- Default seminar analysis window: 2020 through end of 2025 (unless a script explicitly documents a different filter)
- Geography/context: Israel
- Unit used for analysis: weekly spend (Sunday-start week)

## Repository map
- `data/raw/meta_csvs/`: raw Meta exports
- `data/raw/google_csv/`: raw Google export inputs
- `data/raw/events/Consolidated List of Terror and Political incidents 2020-2025 v4.csv`: real event timeline
- `data/processed/first_cleaning/`: first manual-cleaning weekly files
- `data/processed/second_cleaning/`: second manual-cleaning weekly files (preferred analysis input)
- `data/processed/cleaned_data/`: earlier cleaned weekly files (still consumed as fallback inputs by the R scripts)
- `data/generated/`: script-generated tables (canonical placebo event list)
- `data/reference/Total_Spend_Per_Party_or_Entity.xlsx`: standalone combined-total reference workbook
- `outputs/analysis/`: event-study and descriptive outputs (was `analysis_outputs/`)
- `outputs/did/`: DiD outputs (was `analysis_outputs_did/`)
- `outputs/oct7_legacy/`: legacy October 7 compatibility files (`data_window.csv`, `event_study_coefs.csv`, `regression_summary.txt`)
- `scripts/01_meta_csvs_to_final_file.py`: convert Meta raw files to weekly format
- `scripts/02_google_csvs_to_final_file.py`: convert Google raw files to weekly format
- `data/raw/events/Consolidated List of Terror and Political incidents 2020-2025 v4.csv`: event timeline used for event-study windows
- `data/generated/placebo_events_2020_2025.csv`: canonical placebo event weeks used by the placebo checks
- `scripts/03_generate_placebo_events.py`: regenerate the canonical random placebo event-week file from clean weeks away from the real event timeline
- `scripts/04_summarize_platform_publisher_spend.R`: descriptive-only platform/publisher spend summary script; validates exactly 68 publishers in the seminar window
- `scripts/05_event_study_0710.R`: main analysis script (descriptive stats + regressions + figure)
- `scripts/06_did_0710.R`: separate DiD-style post-event regression script on the same cleaned weekly inputs

## Canonical analysis pipeline
1. Build weekly spend files from raw source exports.
2. Apply manual cleaning (first and second cleaning stages).
3. Regenerate `data/generated/placebo_events_2020_2025.csv` with `python3 scripts/03_generate_placebo_events.py` if the event timeline or placebo strategy changes.
4. Run `scripts/04_summarize_platform_publisher_spend.R` when you need the simple Google-vs-Meta and publisher-level descriptive spend totals.
5. Run `scripts/05_event_study_0710.R` against second-cleaning files.
6. Optionally run `scripts/06_did_0710.R` on the same cleaned files for separate post-event FE estimates. This now includes placebo DiD on the canonical placebo weeks.
7. Use outputs in `outputs/analysis/` and `outputs/did/` for interpretation and visualization.

For the full file-by-file lineage of this process, see `DATA_PIPELINE.md`. It documents the raw Meta/Google/event inputs, first and second cleaning files, analysis scripts, combined-total reference workbook, and output folders.

## Default analysis inputs
`scripts/05_event_study_0710.R` uses these defaults if no command-line args are supplied:
- `./data/processed/second_cleaning/weekly_party_spend_google.csv`
- `./data/processed/second_cleaning/weekly_party_spend_meta.csv`
- `./data/raw/events/Consolidated List of Terror and Political incidents 2020-2025 v4.csv`
- output directory: `./outputs/analysis`
- event window: `+/- 2` weeks
- analysis sample: Sunday-start weeks `2020-01-05` through `2025-12-28`

`scripts/06_did_0710.R` uses the same default input files, with:
- output directory: `./outputs/did`
- event window: `+/- 2` weeks
- analysis sample: Sunday-start weeks `2020-01-05` through `2025-12-28`

`scripts/04_summarize_platform_publisher_spend.R` uses the same default cleaned weekly spend inputs, with:
- output directory: `./outputs/analysis/descriptive`
- analysis sample: Sunday-start weeks `2020-01-05` through `2025-12-28`
- expected publisher count: exactly `68`

## How to run
In RStudio:
- Open `scripts/05_event_study_0710.R`
- Run the full script

From terminal:
```bash
Rscript scripts/04_summarize_platform_publisher_spend.R
Rscript scripts/05_event_study_0710.R
```

Optional platform/publisher summary argument form:
```bash
Rscript scripts/04_summarize_platform_publisher_spend.R <google_csv> <meta_csv> <output_dir>
```

Optional full argument form:
```bash
Rscript scripts/05_event_study_0710.R <google_csv> <meta_csv> <events_csv> <output_dir> <window_weeks>
```

To run the separate DiD-style regression:
```bash
Rscript scripts/06_did_0710.R
```

Optional full argument form:
```bash
Rscript scripts/06_did_0710.R <google_csv> <meta_csv> <events_csv> <output_dir> <window_weeks>
```

## Output files generated
- `outputs/analysis/descriptive/platform_spend_summary.csv` generated by `scripts/04_summarize_platform_publisher_spend.R`
- `outputs/analysis/descriptive/publisher_platform_spend_summary.csv` generated by `scripts/04_summarize_platform_publisher_spend.R`
- `outputs/analysis/descriptive/publisher_count_validation.csv` generated by `scripts/04_summarize_platform_publisher_spend.R`
- `outputs/analysis/descriptive/publisher_group_platform_spend_summary.csv` generated by `scripts/04_summarize_platform_publisher_spend.R`
- `outputs/analysis/summaries/regression_summary.txt`
- `outputs/analysis/tables/*`
- `outputs/analysis/tables/event_study_key_results_baseline_minus1.tex`
- `outputs/analysis/tables/event_study_key_results_baseline_minus1.html`
- `outputs/analysis/tables/event_study_panel_summary_baseline_minus1.csv`
- `outputs/analysis/tables/event_study_panel_summary_baseline_minus1.md`
- `outputs/analysis/tables/event_study_panel_summary_baseline_minus1.html`
- `outputs/analysis/tables/event_study_panel_summary_baseline_minus1.tex`
- `outputs/analysis/tables/event_study_panel_summary_baseline_minus1.xlsx`
- `outputs/analysis/tables/descriptive_entity_type_summary_he.csv`
- `outputs/analysis/tables/descriptive_entity_type_summary_he.md`
- `outputs/analysis/tables/descriptive_entity_type_summary_he.html`
- `outputs/analysis/tables/descriptive_yearly_summary_he.csv`
- `outputs/analysis/tables/descriptive_yearly_summary_he.md`
- `outputs/analysis/tables/descriptive_yearly_summary_he.html`
- `outputs/analysis/tables/descriptive_yearly_group_gap_he.csv`
- `outputs/analysis/tables/descriptive_yearly_group_gap_he.md`
- `outputs/analysis/tables/descriptive_yearly_group_gap_he.html`
- `outputs/analysis/descriptive/*.csv`
- `outputs/analysis/descriptive/descriptive_yearly_group_spend_line.png`
- `outputs/analysis/descriptive/descriptive_yearly_group_spend_line.pdf`
- `outputs/analysis/correlations/real_events/*`
- `outputs/analysis/correlations/placebo_events/*`
- `outputs/analysis/event_study/baseline_0/*`
- `outputs/analysis/event_study/baseline_0/figures_by_model/*.png`
- `outputs/analysis/event_study/baseline_minus1/*`
- `outputs/analysis/event_study/baseline_minus1/figures_by_model/*.png`
- `outputs/analysis/placebo_event_study/baseline_0/*`
- `outputs/analysis/placebo_event_study/baseline_0/figures_by_model/*.png`
- `outputs/analysis/placebo_event_study/baseline_minus1/*`
- `outputs/analysis/placebo_event_study/baseline_minus1/figures_by_model/*.png`
- `outputs/analysis/oct7_event_study/baseline_0/*`
- `outputs/analysis/oct7_event_study/baseline_minus1/*`
- `outputs/analysis/event_study/gamma_t_demonstration/*` (paper artefact -- see "gamma_t demonstration regression" below)
- `outputs/oct7_legacy/` legacy October 7 files generated by `scripts/05_event_study_0710.R`: `data_window.csv`, `event_study_coefs.csv`, `regression_summary.txt`. The earlier `event_study_figure.png` was removed because it was byte-identical to `outputs/analysis/oct7_event_study/baseline_minus1/event_study_figure_0710.png`; that block was removed from the script as well.

Descriptive outputs note:
- `outputs/analysis/descriptive/` is a dedicated descriptive-output folder for cleaned weekly Google + Meta spend summaries. It may be produced by a standalone descriptive R step and is also refreshable from the descriptive block in `scripts/05_event_study_0710.R`. Keep it documented as part of the pipeline, not as an incidental regression byproduct.

Additional DiD-style outputs generated by `scripts/06_did_0710.R`:
- `outputs/did/summaries/regression_summary.txt`
- `outputs/did/tables/*`
- `outputs/did/tables/did_key_results_post_from_0.tex`
- `outputs/did/tables/did_key_results_post_from_0.html`
- `outputs/did/tables/did_key_results_post_from_0.png`
- `outputs/did/tables/did_key_results_post_from_0.pdf`
- `outputs/did/tables/did_panel_summary_post_from_0.csv`
- `outputs/did/tables/did_panel_summary_post_from_0.md`
- `outputs/did/tables/did_panel_summary_post_from_0.html`
- `outputs/did/tables/did_panel_summary_post_from_0.tex`
- `outputs/did/tables/did_panel_summary_post_from_0.xlsx`
- `outputs/did/post_from_0/*`
- `outputs/did/placebo/post_from_0/*`
- `outputs/did/oct7/post_from_0/*`
- `outputs/did/gamma_t_demonstration/*` (paper artefact -- see "gamma_t demonstration regression" below)

## Recent analysis decisions for future agents
- Correlation summaries are weekly aggregate correlations, not per-event correlations. `scripts/05_event_study_0710.R` builds weekly event counts for `all_events`, `political`, and `terror`, then correlates those counts with total weekly spend for `all_entities`, `political_party`, and `other_org_or_person`.
- Correlation summaries now also report Pearson-test p-values, and `outputs/analysis/tables/` contains side-by-side real-vs-placebo comparison tables plus a paper-style correlation table.
- Row counts use two different units. Descriptive tables count the cleaned weekly spend panel (`9,548` entity-platform-week rows in the current 2020-2025 second-cleaning sample). Regression `N` counts stacked event-window observations after crossing weekly rows with events and keeping the `+/-2` week window; this can be larger because a weekly row can appear in multiple event windows. Model-fit and compact panel-summary files report both stacked regression observations and unique weekly rows in the windows.
- Final human-facing seminar tables:
  - Event study: open `outputs/analysis/tables/event_study_panel_summary_baseline_minus1.xlsx` first. It is the compact three-column panel table copied from the real event-study key results. Its audit/source table is `outputs/analysis/tables/event_study_key_results_baseline_minus1.csv`, which uses `relative_week = -1` as baseline and reports `relative_week = +1`.
  - DiD: open `outputs/did/tables/did_panel_summary_post_from_0.xlsx` first. It is the compact three-column panel table copied from the real DiD key results. Its audit/source table is `outputs/did/tables/did_key_results_post_from_0.csv`, where `PostEvent = 1` for `relative_week >= 0`.
  - Descriptive Hebrew tables: open `outputs/analysis/tables/descriptive_entity_type_summary_he.html`, `outputs/analysis/tables/descriptive_yearly_summary_he.html`, and `outputs/analysis/tables/descriptive_yearly_group_gap_he.html` for the seminar-style summary tables. Their audit/source files are the generated CSVs in `outputs/analysis/descriptive/`.
  - Descriptive yearly group chart: open `outputs/analysis/descriptive/descriptive_yearly_group_spend_line.png` or `.pdf` for the ggplot line chart comparing civic/private-body spend with formal-party spend by calendar year.
  - Correlations: use `outputs/analysis/tables/correlation_paper_table.csv` / `.md` for the paper-style weekly correlation summary. Its audit/source files are `outputs/analysis/correlations/real_events/correlation_summary.csv` and `outputs/analysis/correlations/placebo_events/placebo_correlation_summary.csv`.
- The compact `*_panel_summary_*` files are presentation copies only. Do not manually edit their numbers; regenerate them from the corresponding `*_key_results_*.csv` source if the regressions change.
- Correlation graphs are intentionally simple presentation outputs: heatmaps for coefficient values and scatter panels with OLS lines and 95% confidence bands.
- Placebo dates live in exactly one canonical table: `data/generated/placebo_events_2020_2025.csv`. It is generated by `python3 scripts/03_generate_placebo_events.py`, which reads the real event timeline, excludes candidate Sunday weeks within 3 weeks of any real event week, samples one distinct clean Sunday week per real event with seed `20260510` from the allowed pool `2020-01-26` through `2025-12-07`, then assigns labels matching the real event type counts. With the current v4 file this is 12 placebo events: 7 political and 5 terror/security.
- Both analysis scripts read this root file directly, validate required columns (`event_date`, `event_type_group`), validate that dates are unique Sundays inside the required analysis buffer and more than 3 weeks away from every real event week, and stop if the file is missing or malformed. They do not generate a hidden fallback placebo list and do not write expanded `placebo_events_dates.csv` copies.
- Placebo checks reuse the same model split table as the real event-study models. This is deliberate so real and placebo results are comparable.
- October 7, 2023 is kept both as an all-entity dedicated model and as split models for political parties and other organizations/people. Do not collapse this back into only one combined result. The canonical split coefficient files are `event_study_coefs_0710_by_group.csv` and `did_coefs_0710_by_group.csv` inside the relevant October 7 output subfolders; do not recreate the old duplicate `*_0710_all_party_org.csv` aliases.
- Graph confidence intervals should remain 95%. The scripts call `broom::tidy(..., conf.int = TRUE, conf.level = 0.95)` for model coefficient outputs, and the correlation scatter graphs use ggplot's `geom_smooth(..., se = TRUE)` default 95% interval.
- Generated CSV and summary text outputs are formatted through `write_clean_csv()` / `format_output_table()` for readability, no scientific notation, and p-values below display precision shown as `<0.001` rather than `0.000`.
- Do not add manually created report files at the repository root. The only legacy October 7 compatibility files intentionally kept live under `outputs/oct7_legacy/` and must be generated by `scripts/05_event_study_0710.R`.
- DiD-style analysis stays in `scripts/06_did_0710.R` and writes to `outputs/did/`. Do not merge DiD outputs into the main `outputs/analysis/` folder unless the project owner explicitly changes the reporting structure.
- `scripts/06_did_0710.R` mirrors the placebo-date logic from `scripts/05_event_study_0710.R`: it reads and validates `data/generated/placebo_events_2020_2025.csv` as the only placebo-date source and writes side-by-side real/placebo DiD comparison tables under `outputs/did/tables/`.

## Regression splits implemented
The script estimates event-study regressions with fixed effects and clustered SE by entity for:
- all entities, all events
- political parties only, all events
- other organizations/people only, all events
- all entities, political events only
- all entities, terror events only
- political parties + political events
- political parties + terror events
- other organizations/people + political events
- other organizations/people + terror events

## Important assumptions to preserve
- Weekly index is Sunday-start based.
- Spend values are in ILS and treated as numeric weekly totals.
- `class` is used to split `political_party` vs `other_org_or_person`.
- Event type split comes from `Type` in the consolidated incidents file. In v4, `Security` is mapped into the existing `terror` bucket for the political-vs-terror/security split.
- Event-study baseline is relative week `0`.
- Additional event-study robustness outputs use relative week `-1` as the reference week and live under `baseline_minus1` folders without replacing the baseline-0 outputs.
- In `scripts/06_did_0710.R`, `PostEvent = 1` when `relative_week >= 0` within the chosen event window.
- The canonical placebo list is a source-like analysis input, not just a generated artifact. Keep it at `data/generated/placebo_events_2020_2025.csv` so collaborators and future agents can inspect the dates directly.

## Regression specifications (do not silently change)

The seminar plan originally agreed on the textbook canonical TWFE forms with calendar-week fixed effects (`gamma_t`):

```
event study (textbook):  log(Spending_{i,t}) = alpha_i + gamma_t + sum_k beta_k * D_{i,k} + u_{i,t}
DiD          (textbook): log(Spending_{i,t}) = alpha_i + gamma_t + beta * PostEvent_{i,t} + u_{i,t}
```

After empirical investigation we deliberately moved to a no-`gamma_t` form for **stacked** specs (see "Identification problem with `gamma_t` in our stacked design" below). The scripts as they stand fit:

```
event study (stacked, in scripts):  log(Spending_{i,t}) = alpha_i + sum_k beta_k * D_{i,k} + u_{i,t}
event study (Oct 7, in scripts):    log(Spending_{i,t}) = alpha_i + sum_k beta_k * D_{i,k} + u_{i,t}
DiD          (stacked, in scripts): log(Spending_{i,t}) = alpha_i + beta * PostEvent_{i,t} + u_{i,t}
DiD          (Oct 7, in scripts):   log(Spending_{i,t}) = alpha_i + beta * PostEvent_{i,t} + u_{i,t}
```

Implementation rules — preserve these:

- **Dependent variable is `log(Spending)`, not `log(1 + Spending)`.** Both scripts call `log(weekly_spend_ils)`. Rows with `weekly_spend_ils <= 0` are filtered out before estimation (because `log(0)` is undefined); each script logs how many rows it dropped on stdout. Do not "fix" this back to `log1p` — that changes the estimand and breaks the seminar spec.
- `alpha_i` -> `entity_name` FE, plus `data_source` FE so platform-level differences cannot leak into beta.
- Multi-event (stacked) panels add `event_id` FE on top of `alpha_i`.
- Single-event runs (October 7 dedicated) use `entity_name + data_source` FE only — there is only one event so `event_id` is a singleton and `gamma_t` would be collinear with `relative_week` / `PostEvent` within the one window.
- Standard errors are clustered by `entity_name` (`vcov = ~ entity_name`).
- `run_event_study_model()` and `run_did_model()` accept an `include_gamma_t` flag. The default is `FALSE` (paper-baseline behavior). The flag is only set to `TRUE` for the dedicated demonstration regression described below.

## Identification problem with `gamma_t` in our stacked design

In our stacked panel every entity is observed inside the `+/-W` window of every event (the panel is built by `crossing(spend, events)`). Two consequences:

- **Stacked DiD with `gamma_t`:** inside each event window `PostEvent = 1[relative_week >= 0]` is a deterministic function of `(event_id, week_start_sunday)`. Once `entity + event_id + week_start_sunday` FE are included, `PostEvent` has no within-FE variation: `beta` collapses to ~1e-11 and fixest warns "The VCOV matrix is not positive semi-definite".
- **Stacked event study with `gamma_t`:** the relative-week dummies and `week_start_sunday` FE compete for the same variation. Standard errors blow up by ~300x and the four `beta_k` coefficients collapse to a single linear-in-`k` pattern (e.g. `-0.193, -0.097, +0.097, +0.193` for the all-entities all-events split), which is uninformative.

This is a structural property of the design (no never-treated control units at the same calendar week), not a bug. Following standard stacked-DiD / stacked-event-study practice we omit `gamma_t` for the stacked specs in the script. **Do not silently re-add `gamma_t` to the stacked formulas.**

## `gamma_t` demonstration regression (paper artefact -- preserve)

Each script runs one dedicated regression on the `all_entities_all_events` panel with `gamma_t` switched on (`include_gamma_t = TRUE`) and writes outputs to:

- `outputs/analysis/event_study/gamma_t_demonstration/`
- `outputs/did/gamma_t_demonstration/`

Each folder contains:

- `*_with_gamma_t.csv`            -- the broken regression's coefficients
- `*_with_gamma_t.csv` model fit   -- input/used rows, R2 (within R2 ~ 0)
- `*_comparison.csv`              -- side-by-side baseline (no `gamma_t`) vs `with_gamma_t`
- `README.txt`                    -- plain-English explanation of what the file shows and why

These folders are deliberately preserved so the seminar paper can show "we started with the textbook spec, observed the breakdown, and switched to the no-`gamma_t` stacked form". Do not delete them in cleanup runs and do not collapse them into the main `event_study/` or `post_from_0/` outputs.

## Safe extension rules for AI agents
- Do not silently change definitions of week boundaries or class grouping.
- If assumptions change, document them in both this file and `README.md`.
- Prefer adding new output files over overwriting historical inputs.
- Keep reproducibility: deterministic transforms, explicit file paths, no hidden state.
- Validate required columns before analysis; fail early with clear error messages.
- If changing the placebo strategy, follow the "Placebo refresh checklist" in `DATA_PIPELINE.md`: regenerate `data/generated/placebo_events_2020_2025.csv`, review the clean-week diagnostics, verify count/type/date/distance constraints, rerun `scripts/05_event_study_0710.R`, rerun `scripts/06_did_0710.R`, confirm no `placebo_events_dates.csv` copy was recreated, and update `DATA_PIPELINE.md`, this guide, and `README.md` if counts/range/exclusion/seed change.
