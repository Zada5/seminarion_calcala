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

# Three weeks after the first Sunday week of 2020, and three weeks before
# the last Sunday week of 2025.
FIRST_PLACEBO_WEEK = date(2020, 1, 26)
LAST_PLACEBO_WEEK = date(2025, 12, 7)

# Exclude placebo weeks inside the real event-study neighborhood.
MIN_WEEKS_FROM_REAL_EVENT = 3

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


def real_events():
    events = []
    with EVENTS_CSV.open(newline="", encoding="utf-8-sig") as input_file:
        for row in csv.DictReader(input_file):
            event_group = event_type_group(row["Type"])
            if event_group is None:
                continue
            events.append((next_sunday(parse_event_date(row["Date"])), event_group))
    return events


def far_from_real_events(candidate_week, blocked_weeks):
    return all(
        abs((candidate_week - real_week).days) // 7 > MIN_WEEKS_FROM_REAL_EVENT
        for real_week in blocked_weeks
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
    all_possible_weeks = sunday_weeks(FIRST_PLACEBO_WEEK, LAST_PLACEBO_WEEK)
    real_event_rows = real_events()
    if not real_event_rows:
        raise RuntimeError(f"No political/terror events found in {EVENTS_CSV}")
    blocked_weeks = {event_week for event_week, _ in real_event_rows}
    clean_weeks = [
        week for week in all_possible_weeks
        if far_from_real_events(week, blocked_weeks)
    ]

    event_types = []
    event_type_counts = Counter(event_group for _, event_group in real_event_rows)
    for event_type, count in event_type_counts.items():
        event_types.extend([event_type] * count)

    if len(clean_weeks) < len(event_types):
        raise RuntimeError(
            f"Need {len(event_types)} clean placebo weeks, but only {len(clean_weeks)} are available."
        )

    sampled_weeks = sorted(rng.sample(clean_weeks, len(event_types)))
    rng.shuffle(event_types)

    rows = [
        [event_week.isoformat(), event_type]
        for event_week, event_type in zip(sampled_weeks, event_types)
    ]
    write_placebo_events(rows)

    print(f"Wrote {len(rows)} placebo events to {OUTPUT_CSV}")
    print(f"Seed: {SEED}")
    print(f"Allowed date pool: {FIRST_PLACEBO_WEEK} to {LAST_PLACEBO_WEEK}")
    print(f"Candidate Sunday weeks: {len(all_possible_weeks)}")
    print(f"Clean weeks after real-event exclusion: {len(clean_weeks)}")
    print(f"Real event weeks excluded around: {len(blocked_weeks)} unique weeks")
    print(f"Minimum distance from real event week: > {MIN_WEEKS_FROM_REAL_EVENT} weeks")
    print(f"Real event type counts: {dict(sorted(event_type_counts.items()))}")
    print(f"Placebo type counts: {dict(sorted(Counter(event_types).items()))}")
    print(f"Selected placebo years: {dict(sorted(Counter(week.year for week in sampled_weeks).items()))}")
    print(f"Longest consecutive selected-week run: {longest_consecutive_run(sampled_weeks)}")


if __name__ == "__main__":
    main()
