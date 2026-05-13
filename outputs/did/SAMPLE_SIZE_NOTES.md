# DiD sample-size notes

All DiD outputs use the **stacked difference-in-differences** design on the same cleaned weekly panel used by the descriptive and event-study files. Different N values across files reflect the unit of aggregation, not different data.

| File | N | Meaning |
|---|---|---|
| `post_from_0/did_design_overview.csv` → `full_descriptive_weekly_rows` | **9,548** | Unique entity-week rows in source panel (identical to descriptive/event-study panel) |
| `post_from_0/did_design_overview.csv` → `total_rows` | **3,888** | After stacking around the 25 v4 events: 2,925 unique entity-weeks + 963 duplicates from overlapping ±k event windows |
| `post_from_0/did_model_fit.csv` → `used_rows` (all_entities_all_events) | **3,887** | 3,888 - 1 singleton FE row dropped by `fixest` |
| `placebo/post_from_0/did_design_overview.csv` → `full_descriptive_weekly_rows` | **9,548** | Same source panel as real-event DiD |
| `placebo/post_from_0/did_design_overview.csv` → `total_rows` | **11,355** | Placebo run uses 75 events → 6,013 unique entity-weeks + 5,342 stacked duplicates |
| `placebo/post_from_0/did_model_fit.csv` → `used_rows` | **11,355** | No singleton FE rows dropped in the all-entities placebo model |
| `oct7/post_from_0/did_model_fit_oct7_by_group.csv` → `used_rows` (oct7_all_entities) | **144** | Single event, single window, no stacking; 35 entities × ±2 weeks |
| `oct7/post_from_0/did_model_fit_oct7_by_group.csv` → `used_rows` (oct7_political_parties) | **13** | Political-party subset only |

**Cross-check across all analyses:** every `*_design_overview.csv` reports `full_descriptive_weekly_rows = 9,548`. Confirms identical source panel.

**Paste-ready table notes for the paper:**

- *Main DiD (all real events):* `Stacked DiD with ±2-week windows around the 25 v4 events. N = 3,887 observations (2,925 unique entity-weeks + 963 duplicated across overlapping event windows; 1 singleton dropped).`
- *Placebo DiD:* `Stacked DiD on 75 placebo events. N = 11,355 observations (6,013 unique + 5,342 stacked; no singletons dropped in the all-entities model).`
- *Oct-7 DiD:* `Single-event DiD around 2023-10-07 (±2 weeks). N = 144 observations (35 entities; no stacking).`

See `README.md` Hebrew section for full discussion.
