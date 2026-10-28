# H-1B Analysis: LCA Disclosure Data, FY2023 vs FY2026

An exploratory analysis in Python of the U.S. Department of Labor's **Labor Condition Application (LCA)**
disclosure data. Employers must file an LCA before petitioning for an H-1B (or E-3 / H-1B1) worker. The data
include employer, job, location and wage details for each case.

- **Part 1** explores fiscal year 2023 (1 Oct 2022 – 30 Sep 2023, ~540,000 cases) in depth. It covers who files, for
  which jobs, where, at what pay, and how employer business models differ.
- **Part 2** compares FY2023 with FY2026. DOL has published FY2026 only through Q3, so the comparison uses the same
  October–June window for both years.

**→ Start with [`h1b_lca_analysis.ipynb`](h1b_lca_analysis.ipynb).** It is saved with
outputs, so it can be read on GitHub without running anything.

## Highlights

**FY2023**
- **The raw quarterly files overlap.** About 100k cases (≈18%) appear in two files and are double-counted if the files are simply stacked.
- **Certification is near-automatic.** Median decision time is 7 days, and under 0.5% of H-1B LCAs are denied.
- **The cap lottery sets the calendar.** 23% of certified LCAs request a start date of 1 October 2023.
- **Demand is concentrated.** The top 1% of ~51,000 employers file half of all LCAs. Two thirds are computer/math occupations.
- **Pay varies with place and employer type.** Software developer median offers range from \$99k (MI) to \$164k (CA).
  H-1B-dependent employers put about two thirds of jobs at wage Level II with a median premium over the prevailing wage of zero,
  while other employers' median premium is about 10%.
- **Most LCAs are not new hires.** Only about a third include new employment.

**FY2023 → FY2026 (Oct–Jun of each year)**
- **Volume is up modestly.** Certified LCAs rose 5%, worker positions 12%, and the number of distinct employers 21%.
- **Almost no LCAs were decided after 1 October 2025.** The gap lasted the rest of the month, coinciding with the federal government shutdown, and the backlog cleared in mid-November.
- **IT services shrank and big tech/AI grew.** LCAs from H-1B-dependent employers fell 15% (share 29% → 23%), while Amazon, Apple, Meta, Nvidia and Tesla grew.
- **The job mix broadened.** Data scientists more than doubled, while software developers fell 10%.
- **Wages rose with the floor.** Median offers were up about 15% (nominal), with an unchanged ratio to the prevailing wage.
  Prevailing wages based on private salary surveys tripled.

See the summaries in sections 11 and 12.6 of the notebook for the full findings and caveats. In particular, an LCA is not
an approved visa, and the wage is the offered wage, not the wage actually paid.

## Running it

Requires Python 3.10+.

```bash
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
jupyter notebook h1b_lca_analysis.ipynb
```

The FY2023 files are included in `data/`. **The FY2026 file is not included** because it exceeds GitHub's 100 MB file limit.
To run Part 2, download `LCA_Disclosure_Data_FY2026_Q3.xlsx` from the DOL page linked below and save it in `data/`.

The first run parses the Excel files (a few minutes per year) and caches them as Parquet in `cache/`.
Later runs load in seconds.

## Repository structure

```
.
├── h1b_lca_analysis.ipynb             # the analysis: narrative, code and charts (Part 1 + Part 2)
├── lca.py                             # loading, de-duplication and wage cleaning (imported by the notebook)
├── requirements.txt
└── data/
    ├── LCA_Disclosure_Data_FY2023_Q{1-4}.xlsx   # raw quarterly files from DOL
    └── LCA_Disclosure_Data_FY2026_Q3.xlsx       # download separately (not in git)
```

`lca.load_lca(fiscal_year)` returns a cleaned table for any year whose files are in `data/`. The main cleaning steps are:

1. Stack that year's files (FY2023 has one per quarter; FY2026 so far is one cumulative file).
2. Keep one row per `CASE_NUMBER`, using its latest appearance so the case status is current.
3. Normalise employer names (case, commas and word-final periods) so the same company matches across files and years.
4. Convert offered and prevailing wages to annual amounts, and conservatively repair filings that entered an annual
   salary with an hourly or other sub-annual unit.

## Data source

U.S. Department of Labor, Employment and Training Administration, Office of Foreign Labor Certification:
[Performance Data – LCA Programs (H-1B, H-1B1, E-3)](https://www.dol.gov/agencies/eta/foreign-labor/performance).
FY2023 Q1–Q4 and FY2026 Q3 disclosure files. The data are public records of the U.S. government.

## License

Code is released under the [MIT License](LICENSE). The data are U.S. government works in the public domain.
