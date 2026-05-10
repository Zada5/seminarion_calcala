#!/usr/bin/env python3
"""Generate the one canonical placebo event-date table."""

import csv
import random
from datetime import date, timedelta
from pathlib import Path


OUTPUT_CSV = Path("placebo_events_2020_2025.csv")
SEED = 20260510

# Three weeks after the first Sunday week of 2020, and three weeks before
# the last Sunday week of 2025.
FIRST_PLACEBO_WEEK = date(2020, 1, 26)
LAST_PLACEBO_WEEK = date(2025, 12, 7)

EVENT_TYPE_COUNTS = {
    "political": 36,
    "terror": 30,
}


def sunday_weeks(start_week, end_week):
    weeks = []
    current_week = start_week
    while current_week <= end_week:
        weeks.append(current_week)
        current_week += timedelta(weeks=1)
    return weeks


def main():
    rng = random.Random(SEED)
    all_possible_weeks = sunday_weeks(FIRST_PLACEBO_WEEK, LAST_PLACEBO_WEEK)

    event_types = []
    for event_type, count in EVENT_TYPE_COUNTS.items():
        event_types.extend([event_type] * count)

    sampled_weeks = sorted(rng.sample(all_possible_weeks, len(event_types)))
    rng.shuffle(event_types)

    with OUTPUT_CSV.open("w", newline="", encoding="utf-8") as output_file:
        writer = csv.writer(output_file, lineterminator="\n")
        writer.writerow(["event_date", "event_type_group"])
        for event_week, event_type in zip(sampled_weeks, event_types):
            writer.writerow([event_week.isoformat(), event_type])

    print(f"Wrote {len(event_types)} placebo events to {OUTPUT_CSV}")
    print(f"Date range sampled from: {FIRST_PLACEBO_WEEK} to {LAST_PLACEBO_WEEK}")
    print(f"Counts: {EVENT_TYPE_COUNTS}")
    print(f"Seed: {SEED}")


if __name__ == "__main__":
    main()
