library(data.table)
library(stringr)
library(tidyr)
library(rjson)
library(DBI)
library(RSQLite)
library(httr)
library(pdftools)

script_file <- sub('^--file=', '', commandArgs(FALSE)[grep('^--file=', commandArgs(FALSE))[1]])
if (is.na(script_file) || identical(script_file, character(0))) {
    script_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NA_character_)
}
script_dir <- if (!is.na(script_file)) dirname(normalizePath(script_file)) else getwd()
repo_root <- normalizePath(file.path(script_dir, '..'), mustWork = TRUE)
app_dir <- file.path(repo_root, 'app')
out_root <- file.path(repo_root, 'tests', 'fixtures', 'r_golden')
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

options(cas_analysis.app_dir = app_dir)
source(file.path(app_dir, 'cas_regex.R'))
source(file.path(app_dir, 'cas_parser.R'))
source(file.path(app_dir, 'market_data.R'))
source(file.path(app_dir, 'analytics.R'))

write_csv <- function(dt, path) {
    out <- as.data.table(copy(dt))
    date_cols <- names(out)[vapply(out, inherits, logical(1), what = 'Date')]
    for (col in date_cols) out[, (col) := as.character(get(col))]
    fwrite(out, path, na = '')
}

normalise_transactions <- function(dt) {
    out <- copy(dt)
    setcolorder(out, c(
        'date', 'description', 'amt', 'units', 'nav', 'bal_units', 'days',
        'years', 'amc', 'fund', 'advisor', 'folio', 'pan'
    ))
    out[order(fund, folio, date, description, amt)]
}

fund_summaries <- function(dt) {
    funds <- unique(dt[description != 'Cur Value']$fund)
    rows <- lapply(funds, function(fund_name) {
        dt_fund <- dt[fund == fund_name]
        dt_fund <- recalc_xirr_basis(dt_fund)
        get_mf_summary(dt_fund, folio_ord_num = 1)
    })
    out <- rbindlist(rows, fill = TRUE)
    out[order(Fund)]
}

folio_summaries <- function(dt) {
    rows <- lapply(unique(dt$folio), function(folio_id) {
        get_mf_table_for_txns(dt, folio_id)
    })
    out <- rbindlist(rows, fill = TRUE)
    out[order(Folio)]
}

hierarchy_summary <- function(dt) {
    enriched <- copy(dt)
    enriched[, AMC := amc]
    enriched[, Scheme := fund]
    enriched[, Folio := folio]
    out <- build_hierarchy_xirr_table(enriched, c('AMC', 'Scheme', 'Folio'))
    out[order(Path, Level)]
}

sample_dir <- file.path(app_dir, 'www', 'samples')
sample_files <- list.files(sample_dir, pattern = '\\.pdf$', full.names = TRUE)
if (length(sample_files) == 0) stop('No sample PDFs found under ', sample_dir)

for (sample_file in sample_files) {
    sample_name <- tools::file_path_sans_ext(basename(sample_file))
    sample_out <- file.path(out_root, sample_name)
    dir.create(sample_out, recursive = TRUE, showWarnings = FALSE)

    state <- parse_cas_pdf(sample_file, password = '')
    dt <- get_portfolio_transactions(state)

    write_csv(normalise_transactions(dt), file.path(sample_out, 'transactions.csv'))
    write_csv(fund_summaries(dt), file.path(sample_out, 'fund_summary.csv'))
    write_csv(folio_summaries(dt), file.path(sample_out, 'folio_summary.csv'))
    write_csv(hierarchy_summary(dt), file.path(sample_out, 'hierarchy_amc_scheme_folio.csv'))
}

cat('Generated R golden fixtures in ', out_root, '\n', sep = '')
