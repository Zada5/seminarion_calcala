# DiD sample-size notes

All DiD outputs use the **stacked difference-in-differences** design on the same cleaned weekly panel used by the descriptive and event-study files. Different N values across files reflect the unit of aggregation, not different data.

| File | N | Meaning |
|---|---|---|
| `post_from_0/did_design_overview.csv` → `full_descriptive_weekly_rows` | **9,458** | Unique entity-week rows in source panel (identical to descriptive/event-study panel) |
| `post_from_0/did_design_overview.csv` → `total_rows` | **10,040** | After stacking: 5,543 unique entity-weeks + 4,497 duplicates from overlapping ±k event windows |
| `post_from_0/did_model_fit.csv` → `used_rows` (all_entities_all_events) | **10,038** | 10,040 − 2 singleton FE rows dropped by `fixest` |
| `placebo/post_from_0/did_design_overview.csv` → `full_descriptive_weekly_rows` | **9,458** | Same source panel as real-event DiD |
| `placebo/post_from_0/did_design_overview.csv` → `total_rows` | **9,941** | Placebo events differ → different overlap pattern (4,332 unique + 5,609 stacked) |
| `placebo/post_from_0/did_model_fit.csv` → `used_rows` | **9,939** | 9,941 − 2 singletons |
| `oct7/post_from_0/did_model_fit_0710_by_group.csv` → `used_rows` (oct7_all_entities) | **144** | Single event, single window, no stacking; 35 entities × ±2 weeks |
| `oct7/post_from_0/did_model_fit_0710_by_group.csv` → `used_rows` (oct7_political_parties) | **13** | Political-party subset only |

**Cross-check across all analyses:** every `*_design_overview.csv` reports `full_descriptive_weekly_rows = 9,458`. Confirms identical source panel.

**Paste-ready table notes for the paper:**

- *Main DiD (all real events):* `Stacked DiD with ±2-week windows. N = 10,038 observations (5,543 unique entity-weeks + 4,497 duplicated across overlapping event windows; 2 singletons dropped).`
- *Placebo DiD:* `Stacked DiD on 66 placebo events. N = 9,939 observations (4,332 unique + 5,609 stacked; 2 singletons dropped).`
- *Oct-7 DiD:* `Single-event DiD around 2023-10-07 (±2 weeks). N = 144 observations (35 entities; no stacking).`

See `README.md` Hebrew section for full discussion.
