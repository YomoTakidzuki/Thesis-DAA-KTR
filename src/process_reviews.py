#!/usr/bin/env python3
"""
process_reviews.py
Preprocessing pipeline for DAA/KTR review data:
- schema validation,
- reports on missing values / frequencies / possible duplicates,
- date normalization into post_date_norm (anchor = 2025-09-16),
- saving the cleaned dataset to Parquet/Feather,
- initial CSV exports + a list of unique theme_codes,
- two simple plots (matplotlib, no seaborn).
"""

import argparse  # CLI argument parsing
import os # filesystem paths, directory creation
import re  # regex for validating/cleaning fields
from collections import Counter  # quick frequency summaries in validation
from datetime import datetime, timedelta  # anchor date + relative date math

import pandas as pd # core data handling
from dateutil.parser import parse as du_parse  # robust generic date parser (fallback)
import dateparser   # natural-language dates in RU/EN (“3 months ago”, etc.)
import matplotlib.pyplot as plt  # basic plotting (histograms/bars)

ANCHOR_DATE = datetime(2025, 9, 16) # Anchor date used to convert relative phrases like "3 months ago" into an absolute date.

# Columns that must be present in the input file. If any are missing, the script reports it (and for some steps creates empty placeholders).
REQUIRED_COLS = [
    "case", "source_platform", "rating_1_5", "text", "stance",
    "theme_codes", "pulled_by", "post_date", "has_text"
]

# Allowed categorical vocabularies for quick validation/sanity checks.
ALLOWED_CASE = {"DAA", "KTR"}  # Case identifiers (two study sites).
ALLOWED_SOURCE = {"GoogleMaps", "YandexMaps"} # Platforms where reviews were collected.
ALLOWED_STANCE = {"positive", "negative", "neutral", "mixed"}  # Sentiment/stance labels.
ALLOWED_HAS_TEXT = {"yes", "no"}   # Whether a review contains text (beyond rating).

REL_PATTERNS = [
    # The order is important: first years → months → weeks → days.
    # Russian variants of relative date expressions:
    (r"(\d+)\s*(год|года|лет)\s*назад", "years"),
    (r"(\d+)\s*(месяц|месяца|месяцев)\s*назад", "months"),
    (r"(\d+)\s*(неделю|недели|недель)\s*назад", "weeks"),
    (r"(\d+)\s*(день|дня|дней)\s*назад", "days"),

    # English variants (in case such forms appear in the dataset)
    (r"(\d+)\s*(year|years)\s*ago", "years"),
    (r"(\d+)\s*(month|months)\s*ago", "months"),
    (r"(\d+)\s*(week|weeks)\s*ago", "weeks"),
    (r"(\d+)\s*(day|days)\s*ago", "days"),
]


def parse_args():
    ap = argparse.ArgumentParser(description="Preprocess DAA/KTR reviews")

    # Path to the input review file (.xlsx or .csv)
    ap.add_argument(
        "--input",
        required=True,
        help="Path to the review file (.xlsx or .csv)"
    )

    # Optional Excel sheet name (used only if .xlsx file has multiple sheets)
    ap.add_argument(
        "--sheet",
        default=None,
        help="Excel sheet name (if .xlsx and a specific sheet is required)"
    )

    # Output directory for saving cleaned data and reports
    ap.add_argument(
        "--outdir",
        default=os.path.expanduser("~/Thesis-DAA-KTR/out"),
        help="Directory where results will be saved"
    )

    return ap.parse_args()


def load_data(path: str, sheet: str | None) -> pd.DataFrame:
    # Determine file extension and load data accordingly
    ext = os.path.splitext(path)[1].lower()

    if ext in [".xlsx", ".xls"]:
        # Load Excel file; specify sheet if provided
        return pd.read_excel(path, sheet_name=sheet, engine="openpyxl")

    elif ext == ".csv":
        # Try UTF-8 encoding first, then auto-detect if it fails
        try:
            return pd.read_csv(path)
        except UnicodeDecodeError:
            import chardet
            with open(path, "rb") as f:
                enc = chardet.detect(f.read())["encoding"] or "utf-8"
            return pd.read_csv(path, encoding=enc)

    else:
        # Stop execution if the file type is unsupported
        raise ValueError(f"Unsupported file extension: {ext}")

def validate_schema(df: pd.DataFrame) -> dict:
    # Validate that the dataset includes all required columns.
    # Create a report structure to track missing and invalid entries.
    report = {"missing_required": [], "value_issues": {}}

    # Check for missing columns defined in REQUIRED_COLS
    for col in REQUIRED_COLS:
        if col not in df.columns:
            report["missing_required"].append(col)

    # If critical columns are missing, skip further validation.
    # There is no point in checking values if the schema itself is incomplete.
    if report["missing_required"]:
        return report

    # Standardize column types and capitalization before validation
    df["case"] = df["case"].astype(str)
    df["source_platform"] = df["source_platform"].astype(str)
    df["stance"] = df["stance"].astype(str).str.lower()
    df["has_text"] = df["has_text"].astype(str).str.lower()

    # Prepare a container for columns containing invalid categorical values
    issues = {}

    # Identify invalid values in categorical columns and record their frequencies

    # Check 'case' column — should be only "DAA" or "KTR"
    bad_case = df.loc[~df["case"].isin(ALLOWED_CASE), "case"]
    if not bad_case.empty:
        issues["case"] = Counter(bad_case)

    # Check 'source_platform' — allowed values: "GoogleMaps", "YandexMaps"
    bad_src = df.loc[~df["source_platform"].isin(ALLOWED_SOURCE), "source_platform"]
    if not bad_src.empty:
        issues["source_platform"] = Counter(bad_src)

    # Check 'stance' — allowed values: "positive", "negative", "neutral", "mixed"
    bad_stance = df.loc[~df["stance"].isin(ALLOWED_STANCE), "stance"]
    if not bad_stance.empty:
        issues["stance"] = Counter(bad_stance)

    # Check 'has_text' — must be either "yes" or "no"
    bad_has_text = df.loc[~df["has_text"].isin(ALLOWED_HAS_TEXT), "has_text"]
    if not bad_has_text.empty:
        issues["has_text"] = Counter(bad_has_text)

    # Validate 'rating_1_5' column, if present, ensuring all values are integers 1–5
    if "rating_1_5" in df.columns:
        # Convert to numeric type (coerce invalid entries to NaN) and cast to nullable integer
        df["rating_1_5"] = pd.to_numeric(df["rating_1_5"], errors="coerce").astype("Int64")

        # Identify values outside the allowed 1–5 range
        bad_rating = df.loc[~df["rating_1_5"].isin([1, 2, 3, 4, 5]), "rating_1_5"]
        if not bad_rating.empty:
            issues["rating_1_5"] = Counter(bad_rating.dropna())

    # Store all discovered issues in the validation report
    report["value_issues"] = {k: dict(v) for k, v in issues.items()}
    return report


def normalize_theme_codes_format(df: pd.DataFrame) -> pd.Series:
    """
    Check that the 'theme_codes' column follows the expected format:
    lowercase Latin letters, digits, underscores, and semicolons — no spaces allowed.
    Returns a Boolean Series: True if valid or empty, False otherwise.
    """
    # If the column does not exist, consider all rows valid
    if "theme_codes" not in df.columns:
        return pd.Series([True] * len(df), index=df.index)

    # Regular expression pattern:
    # ^[a-z0-9_]+  -> at least one valid character (letter, digit, underscore)
    # (?:;[a-z0-9_]+)* ->optionally followed by semicolon-separated additional codes
    # $     -> must end after the last code (no trailing spaces)
    pat = re.compile(r"^[a-z0-9_]+(?:;[a-z0-9_]+)*$")  # digits are allowed too

    # Fill NaNs, strip spaces, and test against the pattern
    ok = df["theme_codes"].fillna("").astype(str).str.strip().apply(
        lambda s: (s == "") or bool(pat.fullmatch(s))
    )
    return ok

def rel_to_date(anchor: datetime, n: int, unit: str) -> datetime:
    """
    Convert a relative expression like "2 years ago" or "3 months ago" into
    an absolute date based on a fixed anchor date.

    Args:
        anchor: The reference datetime (e.g., the date of data collection).
        n: The number of time units to subtract.
        unit: The time unit ("years", "months", "weeks", or "days").

    Returns:
        A datetime object representing the computed past date, or None if invalid.
    """
    if unit == "years":
        # Subtract n years from the anchor date
        return anchor.replace(year=anchor.year - n)

    if unit == "months":
        # Subtract n months, adjusting year/month correctly
        y = anchor.year
        m = anchor.month - n
        while m <= 0:
            y -= 1
            m += 12
        # Keep the day valid within the new month (avoid invalid dates like 31 Feb)
        day = min(
            anchor.day,
            [31, 29 if y % 4 == 0 and (y % 100 != 0 or y % 400 == 0) else 28,
             31, 30, 31, 30, 31, 31, 30, 31, 30, 31][m - 1]
        )
        return anchor.replace(year=y, month=m, day=day)

    if unit == "weeks":
        # Subtract n weeks
        return anchor - timedelta(weeks=n)

    if unit == "days":
        # Subtract n days
        return anchor - timedelta(days=n)

    # Return None for unsupported units
    return None

from datetime import date

def parse_post_date(raw) -> pd.Timestamp:
    """
    Parse and normalize the 'post_date' field from various possible formats:
    - Recognizes Excel serial numbers, Python datetime objects, and relative or text-based dates.
    - Converts everything into a unified pandas.Timestamp.
    - Returns pd.NaT if parsing fails.
    """

    # 0) Handle missing values (NaN or None)
    if pd.isna(raw):
        return pd.NaT

    # 1) If it's already a datetime-like object (Timestamp/datetime/date)
    # -> convert to pandas Timestamp and normalize (drop time part)
    if isinstance(raw, (pd.Timestamp, datetime, date)):
        return pd.to_datetime(raw).normalize()

    # 2) Excel serial date numbers (days since 1899-12-30)
    # Common when exporting from spreadsheets
    if isinstance(raw, (int, float)):
        try:
            return pd.to_datetime(raw, unit="D", origin="1899-12-30").normalize()
        except Exception:
            # If conversion fails, we'll try to parse it as a text string below
            pass

    # 3) Text input: try to interpret as a date-like string
    s = str(raw).strip()
    if s == "":
        return pd.NaT

    # 3a) Year-only strings (e.g., "2019")
    # Convention: map to mid-year to avoid bias toward Jan 1 — use July 1
    if re.fullmatch(r"\d{4}", s):
        return pd.Timestamp(f"{s}-07-01")

    # 3b) Relative date phrases like "N years/months/weeks/days ago"
    # We lower-case the string and test against precompiled patterns (REL_PATTERNS).
    s_lower = s.lower()
    for pat, unit in REL_PATTERNS:
        m = re.search(pat, s_lower)
        if m:
            try:
                n = int(m.group(1))   # extract the integer offset
                dt = rel_to_date(ANCHOR_DATE, n, unit)  # convert relative offset to absolute date
                return pd.Timestamp(dt.date()) if dt else pd.NaT
            except Exception:
                return pd.NaT

    # 3c) Absolute date formats (Russian/English and other locales)
    # Try parsing with `dateparser` first — it supports multilingual date strings.
    dt = dateparser.parse(s, settings={"DATE_ORDER": "DMY"})
    if dt:
        return pd.Timestamp(dt.date())

    # If that fails, fall back to Python's dateutil.parser for flexible parsing
    try:
        dt2 = du_parse(s, dayfirst=True, fuzzy=True)
        return pd.Timestamp(dt2.date())
    except Exception:
        # If nothing worked, return a missing timestamp
        return pd.NaT

def main():
    args = parse_args()
    os.makedirs(args.outdir, exist_ok=True)

    # --- Load input file ---
    df = load_data(args.input, args.sheet)

    # --- Normalize platform names for consistency ---
    # Convert full names (with spaces) into compact standardized identifiers.
    platform_map = {
        "Google Maps": "GoogleMaps",
        "Yandex Maps": "YandexMaps",
    }
    df["source_platform"] = (
        df["source_platform"]
        .astype(str)
        .str.strip()
        .replace(platform_map)
    )

    # --- Remove accidental header rows that may have been included in the data ---
    # Sometimes the first row of a spreadsheet is repeated within the data body.
    # This filter ensures no rows where the cell equals its own column name remain.
    for col in ["case", "source_platform", "stance", "has_text"]:
        df = df[df[col].astype(str).str.lower() != col]

    # --- Normalize the 'theme_codes' column ---
    # Standardize separators and formatting for easier parsing later:
    # - Replace commas, spaces, and slashes with semicolons.
    # - Remove duplicate semicolons.
    # - Strip trailing semicolons and convert everything to lowercase.
    if "theme_codes" in df.columns:
        df["theme_codes"] = (
            df["theme_codes"]
            .astype(str)
            .fillna("")
            .str.strip()
            .str.replace(r"[,\s/]+", ";", regex=True)  # comma/space/slash → ";"
            .str.replace(r";{2,}", ";", regex=True) # remove duplicate semicolons
            .str.strip(";")
            .str.lower()
        )

    # --- Schema validation ---
    # Check that all required columns exist and contain valid values.
    schema_report = validate_schema(df.copy())

    # Stop execution if key structural columns are missing.
    # These are essential for grouping and analysis — script cannot proceed without them.
    critical_missing = [
        c for c in ["case", "source_platform", "stance"]
        if c in schema_report.get("missing_required", [])
    ]
    if critical_missing:
        print("ERROR: Missing critical columns:", ", ".join(critical_missing))
        print("Please fix the file and re-run the script.")
        return

    # Extract lists of missing columns and invalid value reports
    missing_cols = schema_report.get("missing_required", [])
    value_issues = schema_report.get("value_issues", {})

    # Ensure that all required columns exist.
    # If some are missing, create them with empty (NA) values so later summaries won’t fail.
    for col in REQUIRED_COLS:
        if col not in df.columns:
            df[col] = pd.NA

    # ---  Type normalization in the main DataFrame --- 
    # Convert key columns to consistent types and formats for downstream processing.
    df["case"] = df["case"].astype(str)
    df["source_platform"] = df["source_platform"].astype(str)
    df["stance"] = df["stance"].astype(str).str.lower()
    df["has_text"] = df["has_text"].astype(str).str.lower()
    df["rating_1_5"] = pd.to_numeric(df["rating_1_5"], errors="coerce").astype("Int64")

    # Identify empty texts (including NaN or whitespace-only strings).
    # This is used later to calculate the proportion of missing textual content.
    text_empty = df["text"].isna() | (df["text"].astype(str).str.strip() == "")

    # --- Detect potential duplicates ---
    # Combine several key columns into a unique string key and look for repeated patterns.
    # This helps identify identical reviews that may have been exported twice.
    dup_cols = ["case", "source_platform", "text", "rating_1_5"]
    dup_key = df[dup_cols].astype(str).agg("||".join, axis=1)
    dup_counts = dup_key.value_counts()
    possible_dups = dup_counts[dup_counts > 1]
    top10_dups = possible_dups.head(10)

    # --- Date normalization ---
    # Apply the unified parsing logic to all dates and compute the share of unparsed values.
    # Non-parsable or missing dates are logged to a separate CSV for manual review.
    df["post_date_norm"] = df["post_date"].apply(parse_post_date)
    not_norm_share = float(df["post_date_norm"].isna().mean())
    bad_dates_csv = os.path.join(args.outdir, "bad_post_dates.csv")
    df.loc[df["post_date_norm"].isna(), ["case", "source_platform", "post_date"]].to_csv(
        bad_dates_csv, index=False
    )

    # --- Additionally: yearly and monthly distributions ---
    df["year"] = df["post_date_norm"].dt.year
    df["ym"]   = df["post_date_norm"].dt.to_period("M").astype(str)
    # Save simple frequency tables for QA and quick plotting elsewhere
    df["year"].value_counts().sort_index().to_csv(os.path.join(args.outdir, "counts_by_year.csv"))
    df["ym"].value_counts().sort_index().to_csv(os.path.join(args.outdir, "counts_by_year_month.csv"))

    # Make text-like columns explicit pandas StringDtype
    # (prevents pyarrow from guessing mixed/object types on export)
    for col in ["text", "theme_codes", "pulled_by", "post_date"]:
        df[col] = df[col].astype("string")  # pandas StringDtype

    # Be explicit for key categorical columns as well (store as strings here)
    # (Downstream code/R can decide whether to recode as factors/categories)
    for col in ["case", "source_platform", "stance", "has_text"]:
        df[col] = df[col].astype("string")

    # --- Persist the cleaned layer for cross-language use (R/Python) ---
    clean_path_parquet = os.path.join(args.outdir, "reviews_master_clean.parquet")
    clean_path_feather = os.path.join(args.outdir, "reviews_master_clean.feather")
    # Parquet is columnar and compressed; Feather is fast for Python/R round-trips
    df.to_parquet(clean_path_parquet, index=False)
    df.to_feather(clean_path_feather)

    # --- Summary reports and basic frequency tables ---
    total = len(df)

    # Cross-tabulation of number of reviews by case and platform
    crosstab_case_platform = pd.crosstab(df["case"], df["source_platform"], dropna=False)

    # Frequency of "has_text" (yes/no/NaN)
    has_text_counts = df["has_text"].value_counts(dropna=False)

    # Overall stance distribution (positive / neutral / negative / mixed)
    stance_overall = df["stance"].value_counts(dropna=False)

    # Breakdown of stance by platform (GoogleMaps, YandexMaps)
    stance_by_platform = pd.crosstab(df["source_platform"], df["stance"], dropna=False)

    # Breakdown of stance by case (DAA vs KTR)
    stance_by_case = pd.crosstab(df["case"], df["stance"], dropna=False)

    # --- Histogram of ratings (if column is available) ---
    # Visualize the distribution of review scores (1–5 scale)
    rating_hist_path = os.path.join(args.outdir, "hist_rating_1_5.png")
    if "rating_1_5" in df.columns:
        plt.figure()
        (
            df["rating_1_5"]
            .dropna()
            .astype(int)
            .plot(kind="hist", bins=[1, 2, 3, 4, 5, 6], rwidth=0.9)
        )
        plt.title("Histogram: rating_1_5")
        plt.xlabel("Rating (1–5)")
        plt.ylabel("Count")
        plt.savefig(rating_hist_path, dpi=200, bbox_inches="tight")
        plt.close()

    # --- Initial exports (CSV summaries) ---
    # Save the main frequency tables for transparency and reproducibility.

    # Case × Platform (number of reviews)
    counts_by_case_platform = crosstab_case_platform.copy()
    counts_by_case_platform.to_csv(os.path.join(args.outdir, "counts_by_case_platform.csv"))

    # Case × Platform × Stance (multi-index cross-tab)
    stance_by_case_platform = pd.crosstab(
        [df["case"], df["source_platform"]],
        df["stance"],
        dropna=False
    )
    stance_by_case_platform.to_csv(os.path.join(args.outdir, "stance_by_case_platform.csv"))

    # --- theme_codes — collect all unique tags into a single-line text file ---
    theme_codes_path = os.path.join(args.outdir, "theme_codes_raw.txt")

    if "theme_codes" in df.columns:
        # Gather all codes from the column, filtering out empty strings
        all_codes = []
        for s in df["theme_codes"].fillna(""):
            s = str(s).strip()
            if s:
                # Split by semicolon and clean whitespace
                all_codes.extend([t.strip() for t in s.split(";") if t.strip()])

        # Sort alphabetically and remove duplicates
        uniq = sorted(set(all_codes))

        # Save as a semicolon-separated string (for easy reuse in R or Python)
        with open(theme_codes_path, "w", encoding="utf-8") as f:
            f.write(";".join(uniq))
    else:
        # If column does not exist, write an empty file (keeps output structure consistent)
        with open(theme_codes_path, "w", encoding="utf-8") as f:
            f.write("")

    # --- Basic stance visualizations (bar charts) ---
    # Plot stance distributions by case and by platform.

    # Bar chart: stance by case (DAA vs KTR)
    bar1_path = os.path.join(args.outdir, "bar_stance_by_case.png")
    plt.figure()
    stance_by_case.plot(kind="bar")
    plt.title("Stance by CASE")
    plt.xlabel("Case")
    plt.ylabel("Count")
    plt.legend(title="Stance")
    plt.tight_layout()
    plt.savefig(bar1_path, dpi=200)
    plt.close()

    # Bar chart: stance by source platform (GoogleMaps vs YandexMaps)
    bar2_path = os.path.join(args.outdir, "bar_stance_by_platform.png")
    plt.figure()
    stance_by_platform.plot(kind="bar")
    plt.title("Stance by SOURCE PLATFORM")
    plt.xlabel("Source platform")
    plt.ylabel("Count")
    plt.legend(title="Stance")
    plt.tight_layout()
    plt.savefig(bar2_path, dpi=200)
    plt.close()

    # --- Console summary report ---
    print("\n=== SUMMARY ===")
    print(f"Total number of reviews: {total}")

    print("\nBreakdown by case × source_platform:")
    print(crosstab_case_platform)

    print("\nShare of has_text (yes/no):")
    print((has_text_counts / total).round(3))

    print("\nOverall stance distribution:")
    print(stance_overall)

    print("\nStance distribution by platform:")
    print(stance_by_platform)

    print("\nStance distribution by case:")
    print(stance_by_case)

    print(f"\nShare of records with UNNORMALIZED dates (post_date_norm is NaT): {not_norm_share:.3%}")

    if missing_cols:
        print("\nMissing non-critical columns (note, but not fatal):", ", ".join(missing_cols))

    if value_issues:
        print("\nSuspicious or invalid values detected in columns:")
        for k, v in value_issues.items():
            print(f"- {k}: {v}")

    print("\nEmpty 'text' entries (including NaN/whitespace-only):", int(text_empty.sum()), "of", total)

    if not possible_dups.empty:
        print("\nPotential duplicates detected (by key: case + source_platform + text + rating_1_5).")
        print("Total duplicate groups:", len(possible_dups))
        print("Top-10 most frequent duplicates:")
        print(top10_dups)
    else:
        print("\nNo potential duplicates found for the defined key.")

    # Validate the format of theme_codes
    ok_theme = normalize_theme_codes_format(df)
    bad_theme_n = int((~ok_theme).sum())
    if bad_theme_n > 0:
        print(
            f"\nWARNING: {bad_theme_n} rows have invalid 'theme_codes' format "
            "(allowed: a-z, 0-9, '_', ';' without spaces)."
        )

    # --- Output: list of generated files ---
    print("\n=== FILES SAVED TO ===")
    print(args.outdir)
    print(" - reviews_master_clean.parquet")
    print(" - reviews_master_clean.feather")
    print(" - counts_by_case_platform.csv")
    print(" - stance_by_case_platform.csv")
    print(" - theme_codes_raw.txt")
    print(" - hist_rating_1_5.png (if 'rating' column present)")
    print(" - bar_stance_by_case.png")
    print(" - bar_stance_by_platform.png")

if __name__ == "__main__":
    main()
