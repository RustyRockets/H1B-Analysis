"""
Loading and cleaning helpers for the DOL LCA (Labor Condition Application)
disclosure data, FY2023.

The notebook imports from here so that the analysis cells stay focused on
questions rather than plumbing. Everything in this module is deterministic:
the same raw files always produce the same cleaned table.

Key choices (each explained where it happens):
  * The four quarterly Excel files are cached as Parquet on first load,
    because parsing ~240 MB of .xlsx takes several minutes.
  * The quarterly files overlap: ~100k cases appear in more than one file.
    We keep one row per CASE_NUMBER (its most recent appearance).
  * Wages are converted to annual figures so hourly/weekly/monthly offers can
    be compared, and a small number of obvious unit-entry errors are repaired.
"""

from pathlib import Path

import numpy as np
import pandas as pd

# Repo-relative locations. DATA_DIR holds the raw files as downloaded from DOL;
# CACHE_DIR is a git-ignored folder for the faster Parquet copies.
ROOT = Path(__file__).resolve().parent
DATA_DIR = ROOT / "data"
CACHE_DIR = ROOT / "cache"

QUARTERS = [1, 2, 3, 4]

# Multipliers that convert a pay rate in a given unit to an annual figure.
# 2,080 hours = 40 h/week x 52 weeks, the standard DOL full-time convention.
ANNUAL_MULTIPLIER = {
    "Year": 1,
    "Month": 12,
    "Bi-Weekly": 26,
    "Week": 52,
    "Hour": 2080,
}

# Columns that hold dates / numbers in the raw files. Everything else stays text.
DATE_COLS = ["RECEIVED_DATE", "DECISION_DATE", "ORIGINAL_CERT_DATE", "BEGIN_DATE", "END_DATE"]
NUMERIC_COLS = [
    "WAGE_RATE_OF_PAY_FROM", "WAGE_RATE_OF_PAY_TO", "PREVAILING_WAGE",
    "TOTAL_WORKER_POSITIONS", "NEW_EMPLOYMENT", "CONTINUED_EMPLOYMENT",
    "CHANGE_PREVIOUS_EMPLOYMENT", "NEW_CONCURRENT_EMPLOYMENT",
    "CHANGE_EMPLOYER", "AMENDED_PETITION", "TOTAL_WORKSITE_LOCATIONS",
]


def load_quarter(q: int) -> pd.DataFrame:
    """Load one quarterly file, reading from the Parquet cache when it exists.

    Every column is read as text first. Excel files mix types within columns
    (e.g. ZIP codes with and without leading zeros), so typing is done
    explicitly later rather than trusting the spreadsheet.
    """
    cache = CACHE_DIR / f"LCA_FY2023_Q{q}.parquet"
    if cache.exists():
        return pd.read_parquet(cache)

    raw = DATA_DIR / f"LCA_Disclosure_Data_FY2023_Q{q}.xlsx"
    df = pd.read_excel(raw, engine="openpyxl", dtype=str)
    CACHE_DIR.mkdir(exist_ok=True)
    df.to_parquet(cache)
    return df


def load_raw() -> pd.DataFrame:
    """Stack all four quarters, tagging each row with the file it came from."""
    frames = [load_quarter(q).assign(SOURCE_QUARTER=q) for q in QUARTERS]
    return pd.concat(frames, ignore_index=True)


def deduplicate(df: pd.DataFrame) -> pd.DataFrame:
    """Keep one row per case.

    DOL's quarterly files are not disjoint: the Q2 file re-publishes most Q1
    decisions, and some cases re-appear later when their status changes
    (e.g. "Certified" -> "Certified - Withdrawn"). Keeping the *last*
    appearance gives each case its most up-to-date status.
    """
    return (
        df.sort_values("SOURCE_QUARTER", kind="stable")
          .drop_duplicates("CASE_NUMBER", keep="last")
          .reset_index(drop=True)
    )


def annualize_wages(df: pd.DataFrame) -> pd.DataFrame:
    """Add ANNUAL_WAGE, ANNUAL_WAGE_TO, ANNUAL_PW and WAGE_TO_PW columns.

    ANNUAL_WAGE is the bottom of the offered range (WAGE_RATE_OF_PAY_FROM),
    because the top of the range is blank for about two thirds of filings.

    Unit-error repair: some employers typed an annual salary but picked
    "Hour" (or another sub-annual unit), giving "wages" in the hundreds of
    millions. We treat a sub-annual wage as annual when BOTH
      * annualizing it gives more than 3x the prevailing wage, and
      * the raw number, read as annual, sits between 0.95x and 3x the
        prevailing wage (i.e. it looks exactly like a normal annual salary).
    This is deliberately conservative; it touches only ~200 rows and leaves
    genuinely high hourly rates (e.g. physicians) alone.
    """
    out = df.copy()
    wage_mult = out["WAGE_UNIT_OF_PAY"].map(ANNUAL_MULTIPLIER)
    pw_mult = out["PW_UNIT_OF_PAY"].map(ANNUAL_MULTIPLIER)

    out["ANNUAL_PW"] = out["PREVAILING_WAGE"] * pw_mult
    out["ANNUAL_WAGE"] = out["WAGE_RATE_OF_PAY_FROM"] * wage_mult
    out["ANNUAL_WAGE_TO"] = out["WAGE_RATE_OF_PAY_TO"] * wage_mult

    raw_vs_pw = out["WAGE_RATE_OF_PAY_FROM"] / out["ANNUAL_PW"]
    unit_error = (
        (out["WAGE_UNIT_OF_PAY"] != "Year")
        & (out["ANNUAL_WAGE"] > 3 * out["ANNUAL_PW"])
        & raw_vs_pw.between(0.95, 3)
    )
    out.loc[unit_error, "ANNUAL_WAGE"] = out.loc[unit_error, "WAGE_RATE_OF_PAY_FROM"]
    out.loc[unit_error, "ANNUAL_WAGE_TO"] = out.loc[unit_error, "WAGE_RATE_OF_PAY_TO"]
    out["WAGE_UNIT_FIXED"] = unit_error

    out["WAGE_TO_PW"] = out["ANNUAL_WAGE"] / out["ANNUAL_PW"]
    return out


def clean(df: pd.DataFrame) -> pd.DataFrame:
    """Type the columns and add the derived fields used throughout the notebook."""
    out = df.copy()
    for col in DATE_COLS:
        out[col] = pd.to_datetime(out[col], errors="coerce")
    for col in NUMERIC_COLS:
        out[col] = pd.to_numeric(out[col], errors="coerce")

    # Employer names are free text; upper-casing and trimming merges the most
    # common variants ("Apple Inc." vs "APPLE INC."). Distinct legal entities
    # (e.g. Amazon.com Services vs Amazon Web Services) are deliberately kept
    # apart, because that is how they file.
    out["EMPLOYER"] = out["EMPLOYER_NAME"].str.upper().str.strip()
    out["WORKSITE_CITY"] = out["WORKSITE_CITY"].str.strip().str.title()

    # Calendar days from receipt to decision.
    out["PROCESSING_DAYS"] = (out["DECISION_DATE"] - out["RECEIVED_DATE"]).dt.days

    # The first two digits of a SOC code are its major group; 15 = Computer
    # and Mathematical occupations. NAICS sector is likewise the first two digits.
    out["SOC_MAJOR"] = out["SOC_CODE"].str[:2]
    out["NAICS_SECTOR"] = out["NAICS_CODE"].str[:2]

    return annualize_wages(out)


def load_lca() -> pd.DataFrame:
    """One call to get the de-duplicated, cleaned FY2023 table (all visa classes, all statuses)."""
    return clean(deduplicate(load_raw()))
