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
- `/meta_csvs/`: raw Meta exports
- `/google_csv/`: raw Google export inputs
- `/first_cleaning/`: first manual-cleaning weekly files
- `/second_cleaning/`: second manual-cleaning weekly files (preferred analysis input)
- `/cleaned_data/`: earlier cleaned weekly files
- `meta_csvs_to_final_file.py`: convert Meta raw files to weekly format
- `google_csvs_to_final_file.py`: convert Google raw files to weekly format
- `Consolidated List of Terror and Political incidents 2020-2025 v3.csv`: event timeline used for event-study windows
- `placebo_events_2020_2025.csv`: canonical placebo event weeks used by the placebo checks
- `event_study_0710.R`: main analysis script (descriptive stats + regressions + figure)
- `did_0710.R`: separate DiD-style post-event regression script on the same cleaned weekly inputs

## Canonical analysis pipeline
1. Build weekly spend files from raw source exports.
2. Apply manual cleaning (first and second cleaning stages).
3. Run `event_study_0710.R` against second-cleaning files.
4. Optionally run `did_0710.R` on the same cleaned files for separate post-event FE estimates.
5. Use outputs in `analysis_outputs/` and `analysis_outputs_did/` for interpretation and visualization.

## Default analysis inputs
`event_study_0710.R` uses these defaults if no command-line args are supplied:
- `./second_cleaning/weekly_party_spend_google.csv`
- `./second_cleaning/weekly_party_spend_meta.csv`
- `./Consolidated List of Terror and Political incidents 2020-2025 v3.csv`
- output directory: `./analysis_outputs`
- event window: `+/- 2` weeks

`did_0710.R` uses the same default input files, with:
- output directory: `./analysis_outputs_did`
- event window: `+/- 2` weeks

## How to run
In RStudio:
- Open `event_study_0710.R`
- Run the full script

From terminal:
```bash
Rscript event_study_0710.R
```

Optional full argument form:
```bash
Rscript event_study_0710.R <google_csv> <meta_csv> <events_csv> <output_dir> <window_weeks>
```

To run the separate DiD-style regression:
```bash
Rscript did_0710.R
```

Optional full argument form:
```bash
Rscript did_0710.R <google_csv> <meta_csv> <events_csv> <output_dir> <window_weeks>
```

## Output files generated
- `analysis_outputs/descriptive_overall.csv`
- `analysis_outputs/descriptive_by_group.csv`
- `analysis_outputs/descriptive_by_year.csv`
- `analysis_outputs/descriptive_by_year_and_group.csv`
- `analysis_outputs/descriptive_pre_post_oct7.csv`
- `analysis_outputs/correlation_summary.csv`
- `analysis_outputs/correlation_coefficients_heatmap.png`
- `analysis_outputs/correlation_scatter_panels.png`
- `analysis_outputs/event_study_coefficients_by_model.csv`
- `analysis_outputs/event_study_model_fit.csv`
- `analysis_outputs/event_study_figure_all_models.png`
- `analysis_outputs/event_study_figures/*.png`
- `analysis_outputs/regression_summary.txt`
- `analysis_outputs/event_study_coefs_0710.csv` (if the 2023-10-07 event is found)
- `analysis_outputs/event_study_figure_0710.png` (if the 2023-10-07 event model is estimated)
- `analysis_outputs/event_study_coefs_0710_by_group.csv` (if the 2023-10-07 split models are estimated)
- `analysis_outputs/event_study_model_fit_0710_by_group.csv` (if the 2023-10-07 split models are estimated)
- `analysis_outputs/event_study_figure_0710_by_group.png` (if the 2023-10-07 split models are estimated)
- `analysis_outputs/event_study_coefficients_by_model_ref_minus1.csv`
- `analysis_outputs/event_study_model_fit_ref_minus1.csv`
- `analysis_outputs/event_study_figure_all_models_ref_minus1.png`
- `analysis_outputs/event_study_figures_ref_minus1/*.png`
- `analysis_outputs/event_study_coefs_0710_ref_minus1.csv` (if the 2023-10-07 event model is estimated)
- `analysis_outputs/event_study_figure_0710_ref_minus1.png` (if the 2023-10-07 event model is estimated)
- `analysis_outputs/event_study_coefs_0710_by_group_ref_minus1.csv` (if the 2023-10-07 split models are estimated)
- `analysis_outputs/event_study_model_fit_0710_by_group_ref_minus1.csv` (if the 2023-10-07 split models are estimated)
- `analysis_outputs/event_study_figure_0710_by_group_ref_minus1.png` (if the 2023-10-07 split models are estimated)
- `analysis_outputs/placebo_events_dates.csv`
- `analysis_outputs/placebo_correlation_summary.csv`
- `analysis_outputs/placebo_correlation_coefficients_heatmap.png`
- `analysis_outputs/placebo_correlation_scatter_panels.png`
- `analysis_outputs/placebo_event_study_coefficients_by_model.csv`
- `analysis_outputs/placebo_event_study_model_fit.csv`
- `analysis_outputs/placebo_event_study_figure_all_models.png`
- `analysis_outputs/placebo_event_study_figures/*.png`
- `analysis_outputs/placebo_event_study_coefficients_by_model_ref_minus1.csv`
- `analysis_outputs/placebo_event_study_model_fit_ref_minus1.csv`
- `analysis_outputs/placebo_event_study_figure_all_models_ref_minus1.png`
- `analysis_outputs/placebo_event_study_figures_ref_minus1/*.png`

Additional DiD-style outputs generated by `did_0710.R`:
- `analysis_outputs_did/did_design_overview.csv`
- `analysis_outputs_did/did_sample_summary_by_model.csv`
- `analysis_outputs_did/did_coefficients_by_model.csv`
- `analysis_outputs_did/did_model_fit.csv`
- `analysis_outputs_did/did_regression_summary.txt`
- `analysis_outputs_did/did_coefficients_by_model.png`
- `analysis_outputs_did/did_coefs_0710.csv` (if the 2023-10-07 event is found)
- `analysis_outputs_did/did_coefs_0710_by_group.csv` (if the 2023-10-07 split models are estimated)
- `analysis_outputs_did/did_model_fit_0710_by_group.csv` (if the 2023-10-07 split models are estimated)
- `analysis_outputs_did/did_sample_summary_0710_by_group.csv` (if the 2023-10-07 split models are estimated)
- `analysis_outputs_did/did_coefficients_0710_by_group.png` (if the 2023-10-07 split models are estimated)
- `analysis_outputs_did/did_design_overview_post_from_minus1.csv`
- `analysis_outputs_did/did_sample_summary_by_model_post_from_minus1.csv`
- `analysis_outputs_did/did_coefficients_by_model_post_from_minus1.csv`
- `analysis_outputs_did/did_model_fit_post_from_minus1.csv`
- `analysis_outputs_did/did_coefficients_by_model_post_from_minus1.png`
- `analysis_outputs_did/did_coefs_0710_post_from_minus1.csv` (if the 2023-10-07 event is found)
- `analysis_outputs_did/did_coefs_0710_by_group_post_from_minus1.csv` (if the 2023-10-07 split models are estimated)
- `analysis_outputs_did/did_model_fit_0710_by_group_post_from_minus1.csv` (if the 2023-10-07 split models are estimated)
- `analysis_outputs_did/did_sample_summary_0710_by_group_post_from_minus1.csv` (if the 2023-10-07 split models are estimated)
- `analysis_outputs_did/did_coefficients_0710_by_group_post_from_minus1.png` (if the 2023-10-07 split models are estimated)

## Recent analysis decisions for future agents
- Correlation summaries are weekly aggregate correlations, not per-event correlations. `event_study_0710.R` builds weekly event counts for `all_events`, `political`, and `terror`, then correlates those counts with total weekly spend for `all_entities`, `political_party`, and `other_org_or_person`.
- Correlation graphs are intentionally simple presentation outputs: heatmaps for coefficient values and scatter panels with OLS lines and 95% confidence bands.
- Placebo dates are explicit in `placebo_events_2020_2025.csv`. The main script reads this file when present, validates required columns (`event_date`, `event_type_group`), validates `event_type_group` is `political` or `terror`, and checks that placebo weeks are more than `analysis_window_weeks + 1` weeks away from real event weeks.
- If `placebo_events_2020_2025.csv` is missing, `event_study_0710.R` falls back to deterministic random placebo generation with seed `7102023`, sampling Sunday weeks in the 2020-01-05 through 2025-12-28 seminar window.
- Placebo checks reuse the same model split table as the real event-study models. This is deliberate so real and placebo results are comparable.
- October 7, 2023 is kept both as an all-entity dedicated model and as split models for political parties and other organizations/people. Do not collapse this back into only one combined result. The canonical split coefficient file is `*_0710_by_group.csv`; do not recreate the old duplicate `*_0710_all_party_org.csv` aliases.
- Graph confidence intervals should remain 95%. The scripts call `broom::tidy(..., conf.int = TRUE, conf.level = 0.95)` for model coefficient outputs, and the correlation scatter graphs use ggplot's `geom_smooth(..., se = TRUE)` default 95% interval.
- Generated CSV and summary text outputs are formatted through `write_clean_csv()` / `format_output_table()` for readability, no scientific notation, and p-values below display precision shown as `<0.001` rather than `0.000`.
- DiD-style analysis stays in `did_0710.R` and writes to `analysis_outputs_did/`. Do not merge DiD outputs into the main `analysis_outputs/` folder unless the project owner explicitly changes the reporting structure.

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
- Event type split comes from `Type` in the consolidated incidents file.
- Event-study baseline is relative week `0`.
- Additional event-study robustness outputs use relative week `-1` as the reference week and add `_ref_minus1` to output names without replacing the baseline-0 outputs.
- In `did_0710.R`, `PostEvent = 1` when `relative_week >= 0` within the chosen event window.
- Additional DiD robustness outputs use `PostEvent = 1` when `relative_week >= -1` and add `_post_from_minus1` to output names without replacing the baseline DiD outputs.
- The canonical placebo list is a source-like analysis input, not just a generated artifact. Keep it in the repo root so collaborators and future agents can inspect the dates directly.

## Safe extension rules for AI agents
- Do not silently change definitions of week boundaries or class grouping.
- If assumptions change, document them in both this file and `README.md`.
- Prefer adding new output files over overwriting historical inputs.
- Keep reproducibility: deterministic transforms, explicit file paths, no hidden state.
- Validate required columns before analysis; fail early with clear error messages.
- If changing the placebo strategy, update `placebo_events_2020_2025.csv`, this guide, and `README.md` together.
