# H-1B Analysis: LCA Disclosure Data, FY2023

An exploratory analysis in Python of the U.S. Department of Labor's **Labor Condition Application (LCA)**
disclosure data for fiscal year 2023 (1 Oct 2022 – 30 Sep 2023). Employers must file an LCA before
petitioning for an H-1B (or E-3 / H-1B1) worker. The data cover about 540,000 cases with employer, job,
location and wage details.

The analysis is open-ended: it looks at who files, for which jobs, where, at what pay, and how employer
business models differ.

**→ Start with [`h1b_lca_fy2023_exploration.ipynb`](h1b_lca_fy2023_exploration.ipynb).** It is saved with
outputs, so it can be read on GitHub without running anything.

## Highlights

- **The raw quarterly files overlap.** About 100k cases (≈18%) appear in two files and are double-counted if the files are simply stacked.
- **Certification is near-automatic.** Median decision time is 7 days, and under 0.5% of H-1B LCAs are denied.
- **The cap lottery sets the calendar.** 23% of certified LCAs request a start date of 1 October 2023.
- **Demand is concentrated.** The top 1% of ~54,000 employers file half of all LCAs. Two thirds are computer/math occupations.
- **Pay varies with place and employer type.** Software developer median offers range from \$99k (MI) to \$164k (CA).
  H-1B-dependent employers put about two thirds of jobs at wage Level II with a median premium over the prevailing wage of zero,
  while other employers' median premium is about 10%.
- **Most LCAs are not new hires.** Only about a third include new employment.

See section 12 of the notebook for the full list and caveats. In particular, an LCA is not an approved visa,
and the wage is the offered wage, not the wage actually paid.

## Running it

Requires Python 3.10+.

```bash
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
jupyter notebook h1b_lca_fy2023_exploration.ipynb
```

The first run parses the four Excel files (a few minutes) and caches them as Parquet in `cache/`.
Later runs load in seconds.

## Repository structure

```
.
├── h1b_lca_fy2023_exploration.ipynb   # the analysis: narrative, code and charts
├── lca.py                             # loading, de-duplication and wage cleaning (imported by the notebook)
├── requirements.txt
└── data/
    └── LCA_Disclosure_Data_FY2023_Q{1-4}.xlsx   # raw quarterly files from DOL
```

`lca.load_lca()` returns the cleaned table in one call. The main cleaning steps are:

1. Stack the four quarterly files.
2. Keep one row per `CASE_NUMBER`, using its latest appearance so the case status is current.
3. Convert offered and prevailing wages to annual amounts, and conservatively repair about 200 filings that entered an annual
   salary with an hourly unit.

## Data source

U.S. Department of Labor, Employment and Training Administration, Office of Foreign Labor Certification:
[Performance Data – LCA Programs (H-1B, H-1B1, E-3)](https://www.dol.gov/agencies/eta/foreign-labor/performance),
FY2023 Q1–Q4 disclosure files. The data are public records of the U.S. government.

## License

Code is released under the [MIT License](LICENSE). The data are U.S. government works in the public domain.
