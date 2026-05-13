gamma_t demonstration -- event study
====================================

Panel: all_entities_all_events, +/- 2 weeks around every political/terror event.

Two regressions are reported here side-by-side in
event_study_coefficients_comparison.csv:

  (A) without_gamma_t  (the spec used in the rest of outputs/analysis/)
      log_y ~ i(relative_week, ref = 0) | entity_name + data_source + event_id

  (B) with_gamma_t     (the textbook canonical TWFE spec we tried first)
      log_y ~ i(relative_week, ref = 0) |
          entity_name + data_source + event_id + week_start_sunday

What to point at in the paper:

  * In (B) the four relative-week coefficients are forced into a single
    linear-in-k pattern: beta_{-2} = -beta_{+2} and beta_{-1} = -beta_{+1},
    with |beta_{+-2}| = 2 |beta_{+-1}|. This is the regression telling us
    only one direction of variation (the linear trend in k) survives the FE
    structure.
  * Standard errors in (B) are inflated by ~300x relative to (A), and
    fixest emits a 'VCOV matrix is not positive semi-definite' warning.
  * In (A) the coefficients are not constrained to be linear in k and the
    standard errors are well-behaved (~0.025-0.030).

Why this happens:

  In our stacked design every entity is observed inside the +/-W window of
  every event (the panel is built by crossing(spend, events)), so on any
  given calendar week the relative-week dummies and week_start_sunday FE
  are nearly collinear. Calendar-week FE absorbs the variation that would
  otherwise identify beta_k. Standard practice in the stacked-event-study
  literature is to omit gamma_t in this case, which is what the rest of
  the script does. The single-event October 7 models drop gamma_t for a
  related but stricter reason: with one event, calendar week is one-to-one
  with relative week.

This folder is a record of the design decision and the empirical
evidence behind it. Do not delete.
