#!/usr/bin/env Rscript
# Refreshes R Shiny scheme data from mfapi.in and AMFI NAVAll.txt.
# Run this script at least weekly, or manually whenever new schemes appear
# (post-merger, NFOs, etc.) or fund matching fails for recently launched/renamed
# schemes.
#
# Usage (from the app/ directory):
#   Rscript refresh_mf_codes.R
#
# Output:
#   navall_categorized.rds - full AMFI NAVAll scheme/category cache,
#                                      used by the R Shiny benchmark dropdown
#   mf_codes.RData         — full scheme list (all categories), used for NAV matching
#   mf_codes_equity.RData  — legacy equity-only subset retained for compatibility

setwd(dirname(normalizePath(
    if (interactive()) rstudioapi::getSourceEditorContext()$path
    else sub("--file=", "", grep("--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1])
)))

suppressPackageStartupMessages({
    library(rjson)
    library(data.table)
    library(stringr)
})

source("market_data.R")

MF_LIST_URL <- "https://api.mfapi.in/mf"

cat("Fetching full scheme list from mfapi.in ...\n")
mf_list <- tryCatch(
    fromJSON(paste(readLines(MF_LIST_URL, warn = FALSE), collapse = "")),
    error = function(e) stop("Failed to fetch scheme list: ", conditionMessage(e))
)
cat(sprintf("  Retrieved %d schemes.\n", length(mf_list)))

dt_mfs <- rbindlist(lapply(mf_list, as.data.table), fill = TRUE)
dt_mfs <- unique(dt_mfs)

save(dt_mfs, file = "mf_codes.RData")
cat("  Saved mf_codes.RData (", nrow(dt_mfs), "schemes )\n")

# ── Legacy equity-only subset ─────────────────────────────────────────────────
# Reuse the AMFI category map to keep the legacy equity subset available.
# Falls back to a keyword filter only if the equity filter itself fails.
cat("\nRefreshing NAVAll cache for R Shiny benchmark dropdown ...\n")
dt_navall <- get_navall_categorized(force_refresh = TRUE)
cat(sprintf("  Cached %d NAVAll schemes.\n", nrow(dt_navall)))

cat("\nBuilding legacy equity-only subset ...\n")
dt_equity <- tryCatch({
    equity_codes <- dt_navall[Category == "Equity Scheme"]$SchemeCode
    dt_mfs[schemeCode %in% as.integer(equity_codes)]
}, error = function(e) {
    warning("Could not use NAVAll category filter (", conditionMessage(e),
            "); falling back to name-based keyword filter.")
    equity_kw <- paste(
        "equity|elss|bluechip|large.cap|mid.cap|small.cap|multi.cap|flexi.cap",
        "contra|value|dividend yield|focused|sectoral|thematic|infrastructure",
        "banking|pharma|technology|consumption|international|global|overseas",
        sep = "|"
    )
    dt_mfs[grepl(equity_kw, schemeName, ignore.case = TRUE)]
})

dt_mfs <- dt_equity   # match the variable name server.R loads from this file
save(dt_mfs, file = "mf_codes_equity.RData")
cat(sprintf("  Saved mf_codes_equity.RData (%d schemes)\n", nrow(dt_mfs)))

cat("\nDone. Restart the Shiny app to pick up the updated scheme lists.\n")
