import math
import os
from datetime import date, datetime, timedelta

import pandas as pd


def week_start_sunday(d: date) -> date:
    offset = (d.weekday() + 1) % 7
    return d - timedelta(days=offset)


EPOCH_WEEK1 = date(2020, 1, 5)


def week_index_since_2020(ws: date) -> int:
    return ((ws - EPOCH_WEEK1).days // 7) + 1


def to_date_safe(value) -> date | None:
    if value is None:
        return None
    if isinstance(value, date) and not isinstance(value, datetime):
        return value
    if isinstance(value, datetime):
        return value.date()
    if isinstance(value, (int, float)) and not math.isnan(value):
        # Excel serial date (days since 1899-12-30)
        return (datetime(1899, 12, 30) + timedelta(days=int(value))).date()
    value_str = str(value).strip()
    if not value_str or value_str.lower() == "nan":
        return None
    for fmt in ("%Y-%m-%d", "%d/%m/%Y"):
        try:
            return datetime.strptime(value_str[:10], fmt).date()
        except ValueError:
            continue
    return None


def normalize_columns(df: pd.DataFrame) -> dict[str, str]:
    normalized = {}
    for col in df.columns:
        key = col.strip().lower()
        normalized[key] = col
    return normalized


def process_google_file(input_path: str, output_csv_path: str, sheet_name: str | int = 0):
    try:
        df = pd.read_excel(input_path, sheet_name=sheet_name, engine="openpyxl")
    except ImportError as exc:
        raise ImportError(
            "Missing optional dependency 'openpyxl'. Install it with "
            "`pip install -r requirements.txt` and retry."
        ) from exc
    normalized = normalize_columns(df)

    advertiser_col = normalized.get("advertiser_name")
    week_start_col = normalized.get("week_start_date")
    spend_col = normalized.get("spend_ils")

    missing = [name for name, col in [
        ("Advertiser_Name", advertiser_col),
        ("Week_Start_Date", week_start_col),
        ("Spend_ILS", spend_col),
    ] if col is None]
    if missing:
        raise ValueError(f"Missing required columns: {', '.join(missing)}")

    agg_total_week = {}

    for row in df.itertuples(index=False):
        party_name = getattr(row, advertiser_col)
        week_start_raw = getattr(row, week_start_col)
        spend_value = getattr(row, spend_col)
        if party_name is None or str(party_name).strip() == "":
            continue
        week_start = to_date_safe(week_start_raw)
        if week_start is None:
            continue
        if spend_value is None or (isinstance(spend_value, float) and math.isnan(spend_value)):
            continue
        try:
            total_spend = float(spend_value)
        except (TypeError, ValueError):
            continue

        ws = week_start_sunday(week_start)
        agg_total_week[(party_name, ws)] = agg_total_week.get((party_name, ws), 0.0) + total_spend

    if not agg_total_week:
        raise RuntimeError("No spend data aggregated. Check input file / columns.")

    out_rows = []
    for (party, ws), total_week in agg_total_week.items():
        out_rows.append({
            "source": "google",
            "party_name": party,
            "week_start_sunday": ws.isoformat(),
            "week_index_since_2020": week_index_since_2020(ws),
            "total_spend_week": total_week,
            "avg_spend_per_day_week": round(total_week / 7.0, 2),
            "currency": "ILS",
        })

    out_df = pd.DataFrame(out_rows)
    out_df = out_df.sort_values(["party_name", "week_start_sunday"]).reset_index(drop=True)
    os.makedirs(os.path.dirname(output_csv_path), exist_ok=True)
    out_df.to_csv(output_csv_path, index=False, encoding="utf-8-sig")

    print(f"Output: {output_csv_path}")


if __name__ == "__main__":
    input_path = "./data/raw/google_csv/Google Ads Political Spendings Cleaned.xlsx"
    output_csv_path = "./data/processed/first_cleaning/weekly_party_spend_google.csv"
    process_google_file(input_path, output_csv_path)
