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

## Deploying the R Shiny App on Render

This repo includes Render-ready Docker files for the R Shiny app:

- `Dockerfile` builds an R 4.5.3 image, restores packages from `app/renv.lock`, copies `app/`, and starts Shiny with `app/start_render.R`.
- `.dockerignore` keeps local caches and development artifacts out of the Docker build while preserving the app data files under `app/`.
- `render.yaml` is optional Blueprint metadata for accounts that can use Blueprints; it is not required for the manual Web Service flow.

To deploy:

1. Push this repo to GitHub/GitLab/Bitbucket.
2. In Render, create a new Web Service from the repo.
3. Set the Language field to Docker.
4. Keep the Dockerfile path as `./Dockerfile`, the Docker context as `.`, and let the Dockerfile `CMD` start the app.
5. Set the service name to `cas-analysis-shiny`, or choose any Render service name you prefer.
6. Render sets `PORT=10000` by default; `app/start_render.R` also honors any custom `PORT` value configured for the service.
7. After deployment, open the `onrender.com` URL for the service.

The Shiny app requires `app/mf_codes.RData` and `app/mf_codes_equity.RData` at startup, so keep those files available in the deployed repo.

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
