#!/usr/bin/env python3
"""Generate the canonical placebo event-week list for the seminar analysis.

The R analysis scripts read `placebo_events_2020_2025.csv` when it exists.
This script regenerates that file from the real event timeline in a
reproducible random way while preserving the number of real events and the
political/terror type mix.
"""

from __future__ import annotations

import argparse
import csv
import random
from collections import Counter
from dataclasses import dataclass
from datetime import date, datetime, timedelta
from pathlib import Path


DEFAULT_EVENTS_CSV = Path("Consolidated List of Terror and Political incidents 2020-2025 v3.csv")
DEFAULT_OUTPUT_CSV = Path("placebo_events_2020_2025.csv")
DEFAULT_ANALYSIS_START_WEEK = "2020-01-05"
DEFAULT_ANALYSIS_END_WEEK = "2025-12-28"
DEFAULT_ANALYSIS_START_DATE = "2020-01-01"
DEFAULT_ANALYSIS_END_DATE = "2025-12-31"
DEFAULT_BOUNDARY_BUFFER_WEEKS = 3
DEFAULT_MIN_GAP_FROM_REAL_EVENTS_WEEKS = 3
DEFAULT_SEED = 20260510
VALID_EVENT_GROUPS = {"political", "terror"}


@dataclass(frozen=True)
class RealEvent:
    event_date: date
    event_week_start_sunday: date
    event_type_group: str


def parse_iso_date(value: str) -> date:
    return datetime.strptime(value, "%Y-%m-%d").date()


def parse_flexible_date(value: str) -> date | None:
    value = value.strip()
    for date_format in ("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y"):
        try:
            return datetime.strptime(value, date_format).date()
        except ValueError:
            continue
    return None


def next_sunday(input_date: date) -> date:
    # Python weekday: Monday=0 ... Sunday=6.
    days_until_sunday = (6 - input_date.weekday()) % 7
    return input_date + timedelta(days=days_until_sunday)


def normalize_event_type(raw_event_type: str) -> str | None:
    event_type = raw_event_type.strip().lower()
    if "terror" in event_type:
        return "terror"
    if "political" in event_type:
        return "political"
    return None


def iter_sundays(start_week: date, end_week: date) -> list[date]:
    if start_week.weekday() != 6 or end_week.weekday() != 6:
        raise ValueError("The placebo range boundaries must both be Sunday dates.")
    if end_week < start_week:
        raise ValueError("The placebo range end is earlier than the start.")

    weeks: list[date] = []
    current_week = start_week
    while current_week <= end_week:
        weeks.append(current_week)
        current_week += timedelta(weeks=1)
    return weeks


def load_real_events(
    events_csv_path: Path,
    analysis_start_date: date,
    analysis_end_date: date,
    analysis_start_week: date,
    analysis_end_week: date,
) -> list[RealEvent]:
    with events_csv_path.open(newline="", encoding="utf-8-sig") as input_file:
        reader = csv.DictReader(input_file)
        required_columns = {"Date", "Type", "Article"}
        missing_columns = required_columns.difference(reader.fieldnames or [])
        if missing_columns:
            missing = ", ".join(sorted(missing_columns))
            raise ValueError(f"Events file is missing required columns: {missing}")

        real_events: list[RealEvent] = []
        for row in reader:
            event_date = parse_flexible_date(row.get("Date", ""))
            event_type_group = normalize_event_type(row.get("Type", ""))
            if event_date is None or event_type_group not in VALID_EVENT_GROUPS:
                continue

            event_week_start_sunday = next_sunday(event_date)
            if not (analysis_start_date <= event_date <= analysis_end_date):
                continue
            if not (analysis_start_week <= event_week_start_sunday <= analysis_end_week):
                continue

            real_events.append(
                RealEvent(
                    event_date=event_date,
                    event_week_start_sunday=event_week_start_sunday,
                    event_type_group=event_type_group,
                )
            )

    real_events.sort(key=lambda event: (event.event_date, event.event_type_group))
    if not real_events:
        raise ValueError("No valid political/terror events were found in the analysis range.")
    return real_events


def minimum_gap_from_real_event_weeks(candidate_week: date, real_event_weeks: set[date]) -> int:
    return min(abs((candidate_week - real_week).days) // 7 for real_week in real_event_weeks)


def build_valid_placebo_weeks(
    candidate_weeks: list[date],
    real_events: list[RealEvent],
    min_gap_from_real_events_weeks: int,
) -> list[date]:
    real_event_weeks = {event.event_week_start_sunday for event in real_events}
    return [
        candidate_week
        for candidate_week in candidate_weeks
        if minimum_gap_from_real_event_weeks(candidate_week, real_event_weeks)
        > min_gap_from_real_events_weeks
    ]


def generate_placebo_rows(
    real_events: list[RealEvent],
    valid_placebo_weeks: list[date],
    seed: int,
) -> list[dict[str, str]]:
    required_event_count = len(real_events)
    if len(valid_placebo_weeks) < required_event_count:
        raise ValueError(
            "Not enough valid placebo weeks after applying the date-window and real-event gap "
            f"rules. Needed {required_event_count}, available {len(valid_placebo_weeks)}."
        )

    rng = random.Random(seed)
    sampled_weeks = sorted(rng.sample(valid_placebo_weeks, required_event_count))

    event_types = [event.event_type_group for event in real_events]
    rng.shuffle(event_types)

    rows = [
        {
            "event_date": sampled_week.isoformat(),
            "event_type_group": event_type,
        }
        for sampled_week, event_type in zip(sampled_weeks, event_types)
    ]
    return rows


def validate_placebo_rows(
    placebo_rows: list[dict[str, str]],
    real_events: list[RealEvent],
    earliest_placebo_week: date,
    latest_placebo_week: date,
    min_gap_from_real_events_weeks: int,
) -> None:
    real_type_counts = Counter(event.event_type_group for event in real_events)
    placebo_type_counts = Counter(row["event_type_group"] for row in placebo_rows)
    if placebo_type_counts != real_type_counts:
        raise ValueError(
            f"Placebo type counts {dict(placebo_type_counts)} do not match real event counts "
            f"{dict(real_type_counts)}."
        )

    placebo_dates = [parse_iso_date(row["event_date"]) for row in placebo_rows]
    if len(placebo_dates) != len(set(placebo_dates)):
        raise ValueError("Generated placebo events contain duplicate weeks.")
    if min(placebo_dates) < earliest_placebo_week or max(placebo_dates) > latest_placebo_week:
        raise ValueError("Generated placebo dates fall outside the allowed placebo range.")
    if any(placebo_date.weekday() != 6 for placebo_date in placebo_dates):
        raise ValueError("Generated placebo dates must all be Sunday-start weeks.")

    real_event_weeks = {event.event_week_start_sunday for event in real_events}
    observed_min_gap = min(
        minimum_gap_from_real_event_weeks(placebo_date, real_event_weeks)
        for placebo_date in placebo_dates
    )
    if observed_min_gap <= min_gap_from_real_events_weeks:
        raise ValueError(
            "Generated placebo dates violate the real-event gap rule. "
            f"Observed minimum gap: {observed_min_gap} weeks; required: "
            f"> {min_gap_from_real_events_weeks} weeks."
        )


def write_placebo_csv(placebo_rows: list[dict[str, str]], output_csv_path: Path) -> None:
    output_csv_path.parent.mkdir(parents=True, exist_ok=True)
    with output_csv_path.open("w", newline="", encoding="utf-8") as output_file:
        writer = csv.DictWriter(
            output_file,
            fieldnames=["event_date", "event_type_group"],
            lineterminator="\n",
        )
        writer.writeheader()
        writer.writerows(placebo_rows)


def build_argument_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Generate a reproducible random placebo event-week CSV."
    )
    parser.add_argument("--events-csv", type=Path, default=DEFAULT_EVENTS_CSV)
    parser.add_argument("--output-csv", type=Path, default=DEFAULT_OUTPUT_CSV)
    parser.add_argument("--analysis-start-week", default=DEFAULT_ANALYSIS_START_WEEK)
    parser.add_argument("--analysis-end-week", default=DEFAULT_ANALYSIS_END_WEEK)
    parser.add_argument("--analysis-start-date", default=DEFAULT_ANALYSIS_START_DATE)
    parser.add_argument("--analysis-end-date", default=DEFAULT_ANALYSIS_END_DATE)
    parser.add_argument("--boundary-buffer-weeks", type=int, default=DEFAULT_BOUNDARY_BUFFER_WEEKS)
    parser.add_argument(
        "--min-gap-from-real-events-weeks",
        type=int,
        default=DEFAULT_MIN_GAP_FROM_REAL_EVENTS_WEEKS,
        help="Placebo weeks must be strictly more than this many weeks from every real event week.",
    )
    parser.add_argument("--seed", type=int, default=DEFAULT_SEED)
    return parser


def main() -> None:
    args = build_argument_parser().parse_args()

    analysis_start_week = parse_iso_date(args.analysis_start_week)
    analysis_end_week = parse_iso_date(args.analysis_end_week)
    analysis_start_date = parse_iso_date(args.analysis_start_date)
    analysis_end_date = parse_iso_date(args.analysis_end_date)

    earliest_placebo_week = analysis_start_week + timedelta(weeks=args.boundary_buffer_weeks)
    latest_placebo_week = analysis_end_week - timedelta(weeks=args.boundary_buffer_weeks)

    real_events = load_real_events(
        events_csv_path=args.events_csv,
        analysis_start_date=analysis_start_date,
        analysis_end_date=analysis_end_date,
        analysis_start_week=analysis_start_week,
        analysis_end_week=analysis_end_week,
    )
    candidate_weeks = iter_sundays(earliest_placebo_week, latest_placebo_week)
    valid_placebo_weeks = build_valid_placebo_weeks(
        candidate_weeks=candidate_weeks,
        real_events=real_events,
        min_gap_from_real_events_weeks=args.min_gap_from_real_events_weeks,
    )
    placebo_rows = generate_placebo_rows(
        real_events=real_events,
        valid_placebo_weeks=valid_placebo_weeks,
        seed=args.seed,
    )
    validate_placebo_rows(
        placebo_rows=placebo_rows,
        real_events=real_events,
        earliest_placebo_week=earliest_placebo_week,
        latest_placebo_week=latest_placebo_week,
        min_gap_from_real_events_weeks=args.min_gap_from_real_events_weeks,
    )
    write_placebo_csv(placebo_rows, args.output_csv)

    real_type_counts = Counter(event.event_type_group for event in real_events)
    placebo_type_counts = Counter(row["event_type_group"] for row in placebo_rows)
    placebo_dates = [row["event_date"] for row in placebo_rows]

    print(f"Real events counted: {len(real_events)}")
    print(f"Real event type counts: {dict(sorted(real_type_counts.items()))}")
    print(f"Candidate Sunday weeks: {len(candidate_weeks)}")
    print(f"Valid Sunday weeks after real-event gap rule: {len(valid_placebo_weeks)}")
    print(f"Placebo events written: {len(placebo_rows)}")
    print(f"Placebo event type counts: {dict(sorted(placebo_type_counts.items()))}")
    print(f"Placebo date range: {min(placebo_dates)} to {max(placebo_dates)}")
    print(f"Seed: {args.seed}")
    print(f"Output: {args.output_csv}")


if __name__ == "__main__":
    main()
