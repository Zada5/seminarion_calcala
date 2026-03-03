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
- `event_study_0710.R`: main analysis script (descriptive stats + regressions + figure)

## Canonical analysis pipeline
1. Build weekly spend files from raw source exports.
2. Apply manual cleaning (first and second cleaning stages).
3. Run `event_study_0710.R` against second-cleaning files.
4. Use outputs in `analysis_outputs/` for interpretation and visualization.

## Default analysis inputs
`event_study_0710.R` uses these defaults if no command-line args are supplied:
- `./second_cleaning/weekly_party_spend_google.csv`
- `./second_cleaning/weekly_party_spend_meta.csv`
- `./Consolidated List of Terror and Political incidents 2020-2025 v3.csv`
- output directory: `./analysis_outputs`
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

## Output files generated
- `analysis_outputs/descriptive_overall.csv`
- `analysis_outputs/descriptive_by_group.csv`
- `analysis_outputs/descriptive_by_year.csv`
- `analysis_outputs/descriptive_by_year_and_group.csv`
- `analysis_outputs/descriptive_pre_post_oct7.csv`
- `analysis_outputs/event_study_coefficients_by_model.csv`
- `analysis_outputs/event_study_model_fit.csv`
- `analysis_outputs/regression_summary.txt`
- `analysis_outputs/event_study_coefs_0710.csv` (if the 2023-10-07 event is found)
- `analysis_outputs/event_study_figure_0710.png` (if the 2023-10-07 event model is estimated)

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

## Safe extension rules for AI agents
- Do not silently change definitions of week boundaries or class grouping.
- If assumptions change, document them in both this file and `README.md`.
- Prefer adding new output files over overwriting historical inputs.
- Keep reproducibility: deterministic transforms, explicit file paths, no hidden state.
- Validate required columns before analysis; fail early with clear error messages.
