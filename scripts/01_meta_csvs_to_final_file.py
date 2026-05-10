import requests
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
    if m:
        download_date = datetime.strptime(m.group(1), "%Y-%m-%d").date()
        party_part = name[:m.start()].rstrip("-_ ").strip()
        if not party_part:
            party_part = "UNKNOWN_PARTY"
        return party_part, download_date
    else:
        # Fallback: use filename (minus extension) as party, file's mtime as download date
        party_part = name.strip() if name.strip() else "UNKNOWN_PARTY"
        try:
            mtime = os.path.getmtime(path)
            download_date = datetime.fromtimestamp(mtime).date()
        except Exception:
            # If file doesn't exist or error, fallback to today
            download_date = date.today()
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
    # Try multiple date formats
    for fmt in ("%Y-%m-%d", "%d/%m/%Y"):
        try:
            return datetime.strptime(s[:10], fmt).date()
        except ValueError:
            continue
    return None


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
    # Exchange rate cache: {date_str: rate}
    exchange_rate_cache = {}

    def get_usd_to_ils_rate(date_obj):
        # Use 3.5 as the default fallback rate
        DEFAULT_RATE = 3.5
        API_KEY = "REMOVED_APILAYER_API_KEY"
        min_supported = datetime(2020, 1, 1).date()
        if date_obj < min_supported:
            print(f"Date {date_obj} before 2020-01-01, using 2020-01-01 for rate lookup.")
            date_obj = min_supported
        date_str = date_obj.strftime("%Y-%m-%d")
        if date_str in exchange_rate_cache:
            return exchange_rate_cache[date_str]
        url = f"https://api.apilayer.com/currency_data/convert?base=USD&symbols=ILS&amount=1&date={date_str}"
        headers = {"apikey": API_KEY}
        try:
            print(f"Fetching USD→ILS rate for {date_str} using API key: {API_KEY[:4]}... (URL: {url})")
            resp = requests.get(url, headers=headers, timeout=10)
            resp.raise_for_status()
            data = resp.json()
            # The expected response has 'result' for the converted amount
            if resp.status_code == 200 and "result" in data:
                rate = float(data["result"])
                exchange_rate_cache[date_str] = rate
                return rate
            else:
                print(f"ERROR: Unexpected API response for {date_str}: {data}. Using default rate {DEFAULT_RATE}.")
                exchange_rate_cache[date_str] = DEFAULT_RATE
                return DEFAULT_RATE
        except Exception as e:
            print(f"ERROR: Could not fetch USD→ILS rate for {date_str}: {e}. URL: {url} Using default rate {DEFAULT_RATE}.")
            exchange_rate_cache[date_str] = DEFAULT_RATE
            return DEFAULT_RATE

    csv_paths = sorted(glob.glob(os.path.join(input_folder, "*.csv")))
    if not csv_paths:
        raise FileNotFoundError(f"No CSV files found in folder: {input_folder}")

    # aggregation: (party, week_start, currency) -> total spend in that week
    agg_total_week = defaultdict(float)
    # track which (party, week_start) exist for output
    output_keys = set()

    for path in csv_paths:
        party, download_dt = parse_party_and_download_date_from_filename(path)
        df = pd.read_csv(path)

        # Required columns (based on your sample):
        # ad_delivery_start_time, ad_delivery_stop_time, spend
        for row in df.itertuples(index=False):
            start = to_date_safe(getattr(row, "ad_delivery_start_time", None))
            stop = to_date_safe(getattr(row, "ad_delivery_stop_time", None))
            spend_mid = parse_meta_spend_midpoint(getattr(row, "spend", None))
            currency = getattr(row, "currency", None)
            if currency is None and "currency" in df.columns:
                # fallback if column exists but value is missing
                currency = df["currency"].iloc[0]
            currency = str(currency) if currency is not None else "UNKNOWN"
            if start is None or spend_mid is None:
                continue
            if stop is None:
                stop = download_dt
            if stop < start:
                continue
            # Convert USD to ILS if needed
            if currency == "USD":
                rate = get_usd_to_ils_rate(start)
                spend_mid = spend_mid * rate
                currency = "ILS"  # treat as ILS from now on
            duration_days = (stop - start).days + 1
            daily_spend = spend_mid / duration_days
            for ws in iter_week_starts(start, stop):
                week_end = ws + timedelta(days=6)
                overlap_start = max(start, ws)
                overlap_end = min(stop, week_end)
                overlap_days = (overlap_end - overlap_start).days + 1
                if overlap_days > 0:
                    agg_total_week[(party, ws)] += overlap_days * daily_spend
                    output_keys.add((party, ws))

    # build output dataframe
    out_rows = []
    for (party, ws) in output_keys:
        total_week = agg_total_week[(party, ws)]
        out_rows.append({
            "source": "meta",
            "party_name": party,
            "week_start_sunday": ws.isoformat(),
            "week_index_since_2020": week_index_since_2020(ws),
            "total_spend_week": total_week,
            "avg_spend_per_day_week": round(total_week / 7.0, 2),
            "currency": "ILS",
        })

    out_df = pd.DataFrame(out_rows)
    if out_df.empty:
        raise RuntimeError("No spend data aggregated. Check input files / columns / spend format.")

    out_df = out_df.sort_values(["party_name", "week_start_sunday"]).reset_index(drop=True)

    # write
    os.makedirs(os.path.dirname(output_csv_path), exist_ok=True)
    out_df.to_csv(output_csv_path, index=False, encoding="utf-8-sig")

    # helpful print
    print(f"Processed {len(csv_paths)} files.")
    print(f"Output: {output_csv_path}")
    print("All spend is now in ILS. USD values were converted using exchangerate.host.")


if __name__ == "__main__":
    # Example usage:
    # Put this script in your project, and set input folder to the folder containing the 50 CSVs.
    input_folder = "./meta_csvs"  # <-- change this
    output_csv_path = "./first_cleaning/weekly_party_spend_meta.csv"
    process_meta_folder(input_folder, output_csv_path)
