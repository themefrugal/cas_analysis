library(data.table)
library(stringr)
library(tidyr)

script_file <- sub('^--file=', '', commandArgs(FALSE)[grep('^--file=', commandArgs(FALSE))[1]])
if (is.na(script_file) || identical(script_file, character(0))) {
    script_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NA_character_)
}
script_dir <- if (!is.na(script_file)) dirname(normalizePath(script_file)) else getwd()
repo_root <- normalizePath(file.path(script_dir, '..'), mustWork = TRUE)
app_dir <- file.path(repo_root, 'app')
out_root <- file.path(repo_root, 'tests', 'fixtures', 'r_parser')
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

options(cas_analysis.app_dir = app_dir)
source(file.path(app_dir, 'cas_regex.R'))
source(file.path(app_dir, 'cas_parser.R'))

write_csv <- function(dt, path) {
    out <- as.data.table(copy(dt))
    date_cols <- names(out)[vapply(out, inherits, logical(1), what = 'Date')]
    for (col in date_cols) out[, (col) := as.character(get(col))]
    fwrite(out, path, na = '')
}

write_case <- function(name, pages) {
    out_dir <- file.path(out_root, name)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    state <- cas_state_from_pages(pages)
    dt <- get_portfolio_transactions(state)
    setcolorder(dt, c(
        'date', 'description', 'amt', 'units', 'nav', 'bal_units', 'days',
        'years', 'amc', 'fund', 'advisor', 'folio', 'pan'
    ))
    writeLines(pages, file.path(out_dir, 'pages.txt'), useBytes = TRUE)
    write_csv(dt, file.path(out_dir, 'transactions.csv'))
}

write_error_case <- function(name, pages) {
    out_dir <- file.path(out_root, name)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    message <- tryCatch({
        state <- cas_state_from_pages(pages)
        get_portfolio_transactions(state)
        ''
    }, error = function(e) conditionMessage(e))
    writeLines(pages, file.path(out_dir, 'pages.txt'), useBytes = TRUE)
    writeLines(message, file.path(out_dir, 'error.txt'), useBytes = TRUE)
}

write_case('standard_two_folio', c(paste(
    c(
        'Alpha Mutual Fund',
        'Folio No: 900000010 / 10                                  PAN: AAAAA0000A',
        'ALPHA-Alpha Growth Fund (Advisor: DIRECT)                                      Registrar : CAMS',
        'Opening Unit Balance: 0.000',
        '01-Jan-2024 Purchase (Continuous Offer)                 10,000.000    100.000       100.000    100.000',
        'Closing Unit Balance: 100.000    NAV: INR 110.000    Market Value on 31-Jan-2024: INR 11,000.000',
        'CAMSCASWS-SAMPLE Version:V1.0 Demo',
        'Beta Mutual Fund',
        'Folio No: 900000011 / 11                                  PAN: BBBBB1111B',
        'BETA-Beta Income Fund - ISIN: INF000000001 (Advisor: ARN-12345)                                      Registrar : CAMS',
        'Opening Unit Balance: 0.000',
        '05-Jan-2024 Purchase (Continuous Offer)                  5,000.000     50.000       100.000     50.000',
        'Closing Unit Balance: 50.000    NAV: INR 120.000    Market Value on 31-Jan-2024: INR 6,000.000'
    ),
    collapse = '\n'
)))

write_case('idcw_reinvest', c(paste(
    c(
        'Gamma Mutual Fund',
        'Folio No: 900000012 / 12                                  PAN: CCCCC2222C',
        'GAMMA-Gamma Dividend Fund (Advisor: DIRECT)',
        'Opening Unit Balance: 0.000',
        '10-Jan-2024 ****IDCW Reinvest**** 1,000.000',
        'Closing Unit Balance: 0.000    NAV: INR 10.000    Market Value on 10-Jan-2024: INR 0.000'
    ),
    collapse = '\n'
)))

write_error_case('bad_closing', c(paste(
    c(
        'Delta Mutual Fund',
        'Folio No: 900000013 / 13                                  PAN: DDDDD3333D',
        'DELTA-Delta Growth Fund (Advisor: DIRECT)',
        'Opening Unit Balance: 0.000',
        '01-Jan-2024 Purchase (Continuous Offer)                 10,000.000    100.000       100.000    100.000',
        'Closing Unit Balance:'
    ),
    collapse = '\n'
)))

cat('Generated R parser fixtures in ', out_root, '\n', sep = '')
