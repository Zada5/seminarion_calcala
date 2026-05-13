#!/usr/bin/env python3
"""Generate the one canonical clean placebo event-date table."""

import csv
import random
from collections import Counter
from datetime import date, datetime, timedelta
from pathlib import Path


EVENTS_CSV = Path("data/raw/events/Consolidated List of Terror and Political incidents 2020-2025 v4.csv")
OUTPUT_CSV = Path("data/generated/placebo_events_2020_2025.csv")
SEED = 20260510

# The R scripts use +/-2-week event windows and require an extra one-week
# buffer, so placebo weeks must be more than 3 weeks away from real events.
MIN_WEEKS_FROM_REAL_EVENT = 3

FIRST_PLACEBO_WEEK = date(2020, 1, 26)
LAST_PLACEBO_WEEK = date(2025, 12, 7)

# Security placebo events are stored as "terror" because the downstream
# analysis bucket is named terror/security.
PLACEBO_TYPE_COUNTS = {
    "political": 35,
    "terror": 25,
}


def parse_event_date(value):
    for date_format in ("%d/%m/%Y", "%Y-%m-%d"):
        try:
            return datetime.strptime(value.strip(), date_format).date()
        except ValueError:
            pass
    raise ValueError(f"Could not parse event date: {value}")


def event_type_group(value):
    value = value.lower()
    if "terror" in value or "security" in value:
        return "terror"
    if "political" in value:
        return "political"
    return None


def next_sunday(day):
    return day + timedelta(days=(6 - day.weekday()) % 7)


def sunday_weeks(start_week, end_week):
    weeks = []
    current_week = start_week
    while current_week <= end_week:
        weeks.append(current_week)
        current_week += timedelta(weeks=1)
    return weeks


def real_event_weeks():
    weeks = []
    with EVENTS_CSV.open(newline="", encoding="utf-8-sig") as input_file:
        for row in csv.DictReader(input_file):
            if event_type_group(row["Type"]) is None:
                continue
            weeks.append(next_sunday(parse_event_date(row["Date"])))
    if not weeks:
        raise RuntimeError(f"No political/terror events found in {EVENTS_CSV}")
    return weeks


def far_from_real_events(candidate_week, real_weeks):
    return all(
        abs((candidate_week - real_week).days) // 7 > MIN_WEEKS_FROM_REAL_EVENT
        for real_week in real_weeks
    )


def longest_consecutive_run(weeks):
    if not weeks:
        return 0
    longest = current = 1
    for previous_week, current_week in zip(weeks, weeks[1:]):
        if current_week - previous_week == timedelta(weeks=1):
            current += 1
            longest = max(longest, current)
        else:
            current = 1
    return longest


def write_placebo_events(rows):
    with OUTPUT_CSV.open("w", newline="", encoding="utf-8") as output_file:
        writer = csv.writer(output_file, lineterminator="\n")
        writer.writerow(["event_date", "event_type_group"])
        writer.writerows(rows)


def main():
    rng = random.Random(SEED)
    real_weeks = set(real_event_weeks())
    all_possible_weeks = sunday_weeks(FIRST_PLACEBO_WEEK, LAST_PLACEBO_WEEK)
    target_count = sum(PLACEBO_TYPE_COUNTS.values())

    selected_weeks = set()
    attempts = 0
    max_attempts = target_count * 100

    while len(selected_weeks) < target_count and attempts < max_attempts:
        attempts += 1
        candidate_week = rng.choice(all_possible_weeks)
        if candidate_week in selected_weeks:
            continue
        if not far_from_real_events(candidate_week, real_weeks):
            continue
        selected_weeks.add(candidate_week)

    if len(selected_weeks) < target_count:
        raise RuntimeError(
            f"Could only generate {len(selected_weeks)} clean placebo weeks; "
            f"needed {target_count}."
        )

    sampled_weeks = sorted(selected_weeks)
    event_types = [
        event_type
        for event_type, count in PLACEBO_TYPE_COUNTS.items()
        for _ in range(count)
    ]
    rng.shuffle(event_types)

    minimum_gap = min(
        abs((placebo_week - real_week).days) // 7
        for placebo_week in sampled_weeks
        for real_week in real_weeks
    )
    rows = [
        [event_week.isoformat(), event_type]
        for event_week, event_type in zip(sampled_weeks, event_types)
    ]
    write_placebo_events(rows)

    print(f"Wrote {len(rows)} placebo events to {OUTPUT_CSV}")
    print(f"Seed: {SEED}")
    print(f"Allowed date pool: {FIRST_PLACEBO_WEEK} to {LAST_PLACEBO_WEEK}")
    print(f"Candidate Sunday weeks: {len(all_possible_weeks)}")
    print(f"Real event weeks excluded around: {len(real_weeks)} unique weeks")
    print(f"Minimum distance from real event week: {minimum_gap} weeks")
    print(f"Placebo type counts: {dict(sorted(Counter(event_types).items()))}")
    print(f"Selected placebo years: {dict(sorted(Counter(week.year for week in sampled_weeks).items()))}")
    print(f"Longest consecutive selected-week run: {longest_consecutive_run(sampled_weeks)}")


if __name__ == "__main__":
    main()
