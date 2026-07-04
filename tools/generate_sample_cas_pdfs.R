library(data.table)

script_file <- sub('^--file=', '', commandArgs(FALSE)[grep('^--file=', commandArgs(FALSE))[1]])
if (is.na(script_file) || identical(script_file, character(0))) {
    script_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NA_character_)
}
script_dir <- if (!is.na(script_file)) dirname(normalizePath(script_file)) else getwd()
repo_root <- normalizePath(file.path(script_dir, '..'), mustWork = TRUE)
app_dir <- file.path(repo_root, 'app')
out_dir <- file.path(app_dir, 'www', 'samples')
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

funds <- list(
    kotak_liquid = list(
        amc = 'Kotak Mutual Fund',
        scheme_code = '119766',
        cas_name = 'K470D-Kotak Liquid Fund Direct Plan Growth (Non-Demat) - ISIN: INF174K01NE8',
        advisor = 'DIRECT',
        registrar = 'CAMS'
    ),
    parag_flexi = list(
        amc = 'PPFAS Mutual Fund',
        scheme_code = '122639',
        cas_name = 'PP001ZG-Parag Parikh Flexi Cap Fund - Direct Plan - Growth - ISIN: INF879O01027',
        advisor = 'DIRECT',
        registrar = 'CAMS'
    ),
    axis_smallcap = list(
        amc = 'Axis Mutual Fund',
        scheme_code = '125354',
        cas_name = 'AXISSCG-Axis Small Cap Fund - Direct Plan - Growth - ISIN: INF846K01K35',
        advisor = 'DIRECT',
        registrar = 'CAMS'
    ),
    icici_nasdaq = list(
        amc = 'ICICI Prudential Mutual Fund',
        scheme_code = '149219',
        cas_name = 'PINC100-ICICI Prudential NASDAQ 100 Index Fund - Direct Plan - Growth - ISIN: INF109KC1U50',
        advisor = 'DIRECT',
        registrar = 'CAMS'
    ),
    sbi_gilt = list(
        amc = 'SBI Mutual Fund',
        scheme_code = '119707',
        cas_name = 'LD040A-SBI Gilt Fund Direct Plan Growth - ISIN: INF200K01SH3',
        advisor = 'DIRECT',
        registrar = 'CAMS'
    )
)

fmt_money <- function(x) {
    format(round(x, 3), big.mark = ',', trim = TRUE, nsmall = 3, scientific = FALSE)
}
fmt_amt <- function(x) {
    val <- fmt_money(abs(x))
    if (x < 0) paste0('(', val, ')') else val
}
fmt_units <- function(x) fmt_amt(x)
fmt_date <- function(x) format(as.Date(x), '%d-%b-%Y')

nav_at_or_before <- function(code, target_date) {
    nav_path <- file.path(app_dir, 'nav_cache', paste0(code, '.rds'))
    if (!file.exists(nav_path)) {
        stop('Missing NAV cache for scheme code ', code, ': ', nav_path)
    }
    dt <- as.data.table(readRDS(nav_path))[order(date)]
    row <- dt[date <= as.Date(target_date)][.N]
    if (nrow(row) == 0) {
        stop('No NAV available for scheme code ', code, ' on or before ', target_date)
    }
    row
}

build_folio <- function(fund, folio, pan, transactions, closing_date) {
    rows <- list()
    balance <- 0

    for (txn in transactions) {
        nav_row <- nav_at_or_before(fund$scheme_code, txn$date)
        nav <- nav_row$nav
        units <- round(abs(txn$amount) / nav, 3)
        signed_units <- if (txn$amount < 0) -units else units
        balance <- round(balance + signed_units, 3)
        rows[[length(rows) + 1]] <- data.table(
            date = nav_row$date,
            description = txn$description,
            amount = txn$amount,
            units = signed_units,
            nav = nav,
            balance = balance
        )
    }

    closing_nav <- nav_at_or_before(fund$scheme_code, closing_date)
    closing_value <- round(balance * closing_nav$nav, 3)

    list(
        fund = fund,
        folio = folio,
        pan = pan,
        rows = rbindlist(rows),
        closing_date = closing_nav$date,
        closing_nav = closing_nav$nav,
        closing_units = balance,
        closing_value = closing_value
    )
}

folio_lines <- function(folio) {
    fund <- folio$fund
    c(
        fund$amc,
        sprintf('Folio No: %s                                  PAN: %s                                  KYC: OK PAN: OK',
                folio$folio, folio$pan),
        sprintf('%s (Advisor: %s)                                      Registrar : %s',
                fund$cas_name, fund$advisor, fund$registrar),
        'Opening Unit Balance: 0.000',
        apply(folio$rows, 1, function(row) {
            sprintf('%s %-45s %14s %12s %12s %12s',
                    fmt_date(row[['date']]),
                    row[['description']],
                    fmt_amt(as.numeric(row[['amount']])),
                    fmt_units(as.numeric(row[['units']])),
                    fmt_money(as.numeric(row[['nav']])),
                    fmt_money(as.numeric(row[['balance']])))
        }),
        sprintf('Closing Unit Balance: %s    NAV: INR %s    Market Value on %s: INR %s',
                fmt_money(folio$closing_units),
                fmt_money(folio$closing_nav),
                fmt_date(folio$closing_date),
                fmt_money(folio$closing_value)),
        'CAMSCASWS-SAMPLE Version:V1.0 Demo'
    )
}

write_sample_pdf <- function(filename, title, folios) {
    path <- file.path(out_dir, filename)
    grDevices::pdf(path, width = 11.7, height = 8.3, family = 'Courier', paper = 'special')
    on.exit(grDevices::dev.off(), add = TRUE)

    all_lines <- c(
        'Consolidated Account Statement - SAMPLE ONLY',
        'All PANs, folio numbers, and transactions in this file are fictional.',
        'Fund names and transaction NAVs are based on cached public NAV histories.',
        title,
        '',
        unlist(lapply(folios, folio_lines), use.names = FALSE)
    )

    per_page <- 37
    pages <- split(all_lines, ceiling(seq_along(all_lines) / per_page))
    for (page in pages) {
        plot.new()
        y <- seq(0.96, 0.06, length.out = length(page))
        for (i in seq_along(page)) {
            text(0.02, y[i], page[i], adj = c(0, 1), family = 'Courier', cex = 0.58)
        }
    }

    invisible(path)
}

liquid_folio <- build_folio(
    funds$kotak_liquid,
    folio = '900000001 / 01',
    pan = 'AAAAA0000A',
    closing_date = '2026-07-03',
    transactions = list(
        list(date = '2023-04-03', description = 'Purchase (Continuous Offer)', amount = 200000),
        list(date = '2024-01-02', description = 'Purchase (Continuous Offer)', amount = 75000),
        list(date = '2025-08-01', description = 'Redemption', amount = -50000)
    )
)

flexi_folio <- build_folio(
    funds$parag_flexi,
    folio = '900000002 / 02',
    pan = 'BBBBB1111B',
    closing_date = '2026-07-03',
    transactions = list(
        list(date = '2021-07-01', description = 'Purchase (Continuous Offer)', amount = 125000),
        list(date = '2022-11-15', description = 'Systematic Investment', amount = 50000),
        list(date = '2025-02-10', description = 'Redemption', amount = -40000)
    )
)

axis_folio <- build_folio(
    funds$axis_smallcap,
    folio = '900000003 / 03',
    pan = 'CCCCC2222C',
    closing_date = '2026-04-10',
    transactions = list(
        list(date = '2020-08-03', description = 'Purchase (Continuous Offer)', amount = 80000),
        list(date = '2021-12-01', description = 'Systematic Investment', amount = 60000),
        list(date = '2024-09-02', description = 'Redemption', amount = -30000)
    )
)

nasdaq_folio <- build_folio(
    funds$icici_nasdaq,
    folio = '900000004 / 04',
    pan = 'DDDDD3333D',
    closing_date = '2026-03-16',
    transactions = list(
        list(date = '2022-01-03', description = 'Purchase (Continuous Offer)', amount = 90000),
        list(date = '2023-06-01', description = 'Systematic Investment', amount = 45000)
    )
)

gilt_folio <- build_folio(
    funds$sbi_gilt,
    folio = '900000005 / 05',
    pan = 'EEEEE4444E',
    closing_date = '2026-04-10',
    transactions = list(
        list(date = '2020-04-01', description = 'Purchase (Continuous Offer)', amount = 150000),
        list(date = '2022-07-01', description = 'Purchase (Continuous Offer)', amount = 75000),
        list(date = '2025-12-01', description = 'Redemption', amount = -50000)
    )
)

created <- c(
    write_sample_pdf('sample-liquid-cas.pdf', 'Sample 1 - Liquid fund portfolio', list(liquid_folio)),
    write_sample_pdf('sample-equity-cas.pdf', 'Sample 2 - Equity and index portfolio', list(flexi_folio, axis_folio, nasdaq_folio)),
    write_sample_pdf('sample-mixed-cas.pdf', 'Sample 3 - Mixed asset portfolio', list(liquid_folio, flexi_folio, gilt_folio))
)

cat(paste(normalizePath(created, winslash = '/'), collapse = '\n'), '\n')
