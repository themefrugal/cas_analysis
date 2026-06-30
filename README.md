# cas_analysis

Analysis of Mutual Fund Consolidated Account Statements (CAS) — a Shiny app that parses CAMS-generated CAS PDFs and provides XIRR, gains, benchmark comparison, and portfolio analytics.

## Source Files

| File | Purpose |
|------|---------|
| `app/server.R` | Shiny server — reactives, XIRR calculations, all UI outputs |
| `app/cas_reader.R` | PDF parsing, NAV fetching, fund-name matching, portfolio valuation |
| `app/cas_regex.R` | Regex patterns for CAS PDF line detection |
| `app/refresh_mf_codes.R` | One-off script to refresh local scheme lists from mfapi.in |
| `app/ui.R` | Shiny UI definition |

## Setup

1. Install R dependencies (handled automatically on first run via `cas_reader.R`'s package bootstrap).
2. Open `app/server.R` in RStudio and click **Run App**, or run `shiny::runApp("app")` from the repo root.

## Refreshing Scheme Data

The app uses two local RData files for fund-name matching and the benchmark dropdown:

- `app/mf_codes.RData` — full scheme list (~37,000 schemes) from [mfapi.in](https://api.mfapi.in/mf)
- `app/mf_codes_equity.RData` — equity-only subset, filtered via AMFI category data

Run the refresh script whenever new schemes appear (post-merger, NFOs, renamed schemes):

```r
Rscript app/refresh_mf_codes.R
# or from RStudio with app/ as working directory:
source("refresh_mf_codes.R")
```

Restart the Shiny app after refreshing.

## Fund-Name Matching Pipeline

`match_fund_to_scheme()` in `cas_reader.R` resolves a CAS fund name to an mfapi.in scheme code using a 5-step fallback chain:

1. **ISIN lookup** via `isin.db` (casparser-isin database) — most precise. Validates that the resolved scheme name actually resembles the CAS fund name (≥2 non-generic brand words in common) to detect stale ISINs from merged/renamed schemes.
2. **Exact normalised name match** against local `mf_codes.RData`.
3. **Approximate string match** (`agrep`, edit-distance ≤ 30%) — candidates validated through the overlap scorer before accepting.
4. **Overlap coefficient** — checks both directions:
   - *Forward*: fraction of mfapi-name words found in CAS name (handles CAS names with extra descriptors).
   - *Reverse*: fraction of CAS-name words found in mfapi name (handles post-SEBI renames that added qualifier words, e.g. "Tax Saver" added to all ELSS schemes).
   Requires ≥4 matching words and at least one non-generic brand/AMC word in the intersection.
5. **mfapi.in live search API** — last resort for schemes absent from local data, validated with the same two-direction overlap scorer.

## Scheme Category Data

Scheme category and sub-category (e.g. `Equity Scheme / Large Cap Fund`) are loaded from [AMFI's NAVAll.txt](https://portal.amfiindia.com/spages/NAVAll.txt) via `get_navall_categorized()`, cached locally under `app/nav_cache/navall_categorized.rds` and refreshed every 20 hours. NAVs themselves still come from mfapi.in — this file is used for category metadata only.

The Fund-wise summary table exposes `Category` and `SubCategory` columns (with per-column filters) to allow slicing performance by scheme type.

## Key Calculations

- **Overall Portfolio XIRR** — computed directly from all raw transactions including each fund's `Cur Value` row on its own as-of date.
- **Analysis Period XIRR** — synthetic cash-flow XIRR for a selected sub-period: start value (portfolio valued at period start via NAV lookup), actual period transactions, and end value. When the period spans the full statement history, reuses the raw `Cur Value` rows (same cash flows as Overall XIRR) so both numbers agree.
- **Benchmark XIRR** — simulates investing all your historical cash flows into a chosen benchmark index fund; uses the same cash-flow timing as your actual investments for a like-for-like comparison.
