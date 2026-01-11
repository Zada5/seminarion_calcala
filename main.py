import os
import re
import glob
import math
from datetime import datetime, date, timedelta
from collections import defaultdict

import pandas as pd


# ---------------------------
# Helpers
# ---------------------------

DATE_RE = re.compile(r"(20\d{2}-\d{2}-\d{2})")

def parse_party_and_download_date_from_filename(path: str):
    """
    Examples:
      "מקור ראשון-2026-01-04.csv"         -> party="מקור ראשון", download_date=2026-01-04
      "מחדל23-2026-01-04_2.csv"          -> party="מחדל23", download_date=2026-01-04
      "foo-bar-2024-11-01 (1).csv"       -> party="foo-bar", download_date=2024-11-01
    """
    base = os.path.basename(path)
    name = os.path.splitext(base)[0]

    m = DATE_RE.search(name)
    if not m:
        raise ValueError(f"Could not find YYYY-MM-DD in filename: {base}")

    download_date = datetime.strptime(m.group(1), "%Y-%m-%d").date()
    party_part = name[:m.start()].rstrip("-_ ").strip()
    if not party_part:
        party_part = "UNKNOWN_PARTY"

    return party_part, download_date


def parse_meta_spend_midpoint(spend_value) -> float | None:
    """
    Meta Ad Library CSV 'spend' looks like:
      "lower_bound: 100, upper_bound: 199"
    Return midpoint (149.5). If missing/invalid -> None.
    """
    if spend_value is None or (isinstance(spend_value, float) and math.isnan(spend_value)):
        return None

    s = str(spend_value)
    m = re.search(r"lower_bound:\s*([0-9]+)", s)
    n = re.search(r"upper_bound:\s*([0-9]+)", s)
    if not m or not n:
        return None

    low = float(m.group(1))
    high = float(n.group(1))
    return (low + high) / 2.0


def to_date_safe(x) -> date | None:
    if x is None or (isinstance(x, float) and math.isnan(x)):
        return None
    s = str(x).strip()
    if not s or s.lower() == "nan":
        return None
    # expected "YYYY-MM-DD"
    return datetime.strptime(s[:10], "%Y-%m-%d").date()


def week_start_sunday(d: date) -> date:
    # Python weekday: Mon=0 ... Sun=6
    offset = (d.weekday() + 1) % 7  # Sunday -> 0, Monday -> 1, ...
    return d - timedelta(days=offset)


EPOCH_WEEK1 = date(2020, 1, 5)  # first Sunday of 2020

def week_index_since_2020(ws: date) -> int:
    return ((ws - EPOCH_WEEK1).days // 7) + 1


def iter_week_starts(start_day: date, end_day: date):
    ws = week_start_sunday(start_day)
    we = week_start_sunday(end_day)
    cur = ws
    while cur <= we:
        yield cur
        cur += timedelta(days=7)


# ---------------------------
# Core processing
# ---------------------------

def process_meta_folder(input_folder: str, output_csv_path: str):
    csv_paths = sorted(glob.glob(os.path.join(input_folder, "*.csv")))
    if not csv_paths:
        raise FileNotFoundError(f"No CSV files found in folder: {input_folder}")

    # aggregation: (party, week_start) -> total spend in that week
    agg_total_week = defaultdict(float)

    # optional: track currencies to warn if mixed
    currencies_seen = set()

    for path in csv_paths:
        party, download_dt = parse_party_and_download_date_from_filename(path)

        df = pd.read_csv(path)

        # currency column exists in your sample files
        if "currency" in df.columns:
            for c in df["currency"].dropna().unique().tolist():
                currencies_seen.add(str(c))

        # Required columns (based on your sample):
        # ad_delivery_start_time, ad_delivery_stop_time, spend
        for row in df.itertuples(index=False):
            # tuple access by attribute name (pandas creates valid identifiers)
            start = to_date_safe(getattr(row, "ad_delivery_start_time", None))
            stop = to_date_safe(getattr(row, "ad_delivery_stop_time", None))
            spend_mid = parse_meta_spend_midpoint(getattr(row, "spend", None))

            if start is None or spend_mid is None:
                continue

            # if still running -> assume ended at download date (as you requested)
            if stop is None:
                stop = download_dt

            # guard
            if stop < start:
                continue

            duration_days = (stop - start).days + 1
            daily_spend = spend_mid / duration_days

            # allocate into weeks by day-overlap
            for ws in iter_week_starts(start, stop):
                week_end = ws + timedelta(days=6)
                overlap_start = max(start, ws)
                overlap_end = min(stop, week_end)
                overlap_days = (overlap_end - overlap_start).days + 1
                if overlap_days > 0:
                    agg_total_week[(party, ws)] += overlap_days * daily_spend

    # build output dataframe
    out_rows = []
    for (party, ws), total_week in agg_total_week.items():
        out_rows.append({
            "source": "meta",
            "party_name": party,
            "week_start_sunday": ws.isoformat(),
            "week_index_since_2020": week_index_since_2020(ws),
            "total_spend_week": total_week,
            "avg_spend_per_day_week": total_week / 7.0,
        })

    out_df = pd.DataFrame(out_rows)
    if out_df.empty:
        raise RuntimeError("No spend data aggregated. Check input files / columns / spend format.")

    out_df = out_df.sort_values(["party_name", "week_start_sunday"]).reset_index(drop=True)

    # write
    out_df.to_csv(output_csv_path, index=False, encoding="utf-8-sig")

    # helpful print
    print(f"Processed {len(csv_paths)} files.")
    print(f"Output: {output_csv_path}")
    if len(currencies_seen) > 1:
        print(f"WARNING: multiple currencies detected: {sorted(currencies_seen)}")
    elif len(currencies_seen) == 1:
        print(f"Currency: {next(iter(currencies_seen))}")


if __name__ == "__main__":
    # Example usage:
    # Put this script in your project, and set input folder to the folder containing the 50 CSVs.
    input_folder = "./meta_csvs"  # <-- change this
    output_csv_path = "./weekly_party_spend_meta.csv"
    process_meta_folder(input_folder, output_csv_path)
