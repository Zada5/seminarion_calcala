gamma_t demonstration -- DiD
============================

Panel: all_entities_all_events, +/- 2 weeks around every political/terror event.
PostEvent = 1 when relative_week >= 0.

Two regressions are reported here side-by-side in
did_coefficient_comparison.csv:

  (A) without_gamma_t  (the spec used in the rest of outputs/did/)
      log_y ~ post_event | entity_name + data_source + event_id

  (B) with_gamma_t     (the textbook canonical TWFE-DiD spec we tried first)
      log_y ~ post_event |
          entity_name + data_source + event_id + week_start_sunday

What to point at in the paper:

  * In (B) beta is driven to ~1e-11 (numerical zero) and fixest emits
    'The VCOV matrix is not positive semi-definite and was fixed'.
  * In (A) beta is meaningfully sized and its standard error is
    well-behaved.

Why this happens:

  In our stacked DiD design every entity in every event window is
  'treated' by that event; there is no never-treated control group at
  the same calendar week. Inside each window PostEvent is a deterministic
  function of (event_id, week_start_sunday). Once entity FE + event_id FE
  + week FE are included, PostEvent has no within-FE variation and beta
  is not identified. Following standard practice in the stacked-DiD
  literature, the rest of this script omits gamma_t. The October 7
  single-event DiD models drop gamma_t for a related stricter reason:
  with one event, calendar week ↔ relative week one-to-one and
  PostEvent is a function of calendar week alone.

This folder is a record of the design decision and the empirical
evidence behind it. Do not delete.
