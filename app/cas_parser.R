# CAS PDF parsing layer.
# Produces transaction data from pdftools::pdf_text() output.  Functions accept a
# parser state explicitly; legacy global-state fallback is kept for old scripts.

cas_parser_dir <- getOption('cas_analysis.app_dir')
if (is.null(cas_parser_dir)) {
    cas_parser_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
    cas_parser_dir <- if (!is.na(cas_parser_file)) dirname(cas_parser_file) else getwd()
}
source(file.path(cas_parser_dir, 'cas_regex.R'))

legacy_cas_state <- function() {
    required <- c('all_lines', 'folio_lines', 'amc_lines',
                  'opening_lines', 'closing_lines')
    missing <- required[!vapply(required, exists, logical(1), envir = .GlobalEnv)]
    if (length(missing) > 0) {
        stop('CAS parser state is missing: ', paste(missing, collapse = ', '))
    }
    list(
        all_lines     = get('all_lines', envir = .GlobalEnv),
        folio_lines   = get('folio_lines', envir = .GlobalEnv),
        amc_lines     = get('amc_lines', envir = .GlobalEnv),
        opening_lines = get('opening_lines', envir = .GlobalEnv),
        closing_lines = get('closing_lines', envir = .GlobalEnv)
    )
}

validate_cas_state <- function(state) {
    required <- c('all_lines', 'folio_lines', 'amc_lines',
                  'opening_lines', 'closing_lines')
    missing <- required[!required %in% names(state)]
    if (length(missing) > 0) {
        stop('CAS parser state is incomplete: ', paste(missing, collapse = ', '))
    }

    n_folio <- length(state$folio_lines)
    n_open  <- length(state$opening_lines)
    n_close <- length(state$closing_lines)
    if (n_folio == 0) {
        stop('No folio sections were found in the CAS PDF.')
    }
    if (n_folio != n_open || n_folio != n_close) {
        stop('CAS structure mismatch: found ', n_folio, ' folio lines, ',
             n_open, ' opening balance lines, and ', n_close,
             ' closing balance lines.')
    }
    bad_order <- which(!(state$folio_lines < state$opening_lines &
                         state$opening_lines < state$closing_lines))
    if (length(bad_order) > 0) {
        stop('CAS structure mismatch around folio ordinal(s): ',
             paste(bad_order, collapse = ', '))
    }
    invisible(state)
}

cas_state_from_pages <- function(pages) {
    all_lines <- unlist(str_split(pages, pattern = '\n'), use.names = FALSE)
    all_lines <- gsub('[\u2212\u2010\u2011\u2012\u2013\u2014]', '-', all_lines)
    state <- list(
        all_lines     = all_lines,
        folio_lines   = which(grepl('Folio No\\s*:', all_lines, ignore.case = TRUE)),
        amc_lines     = which(grepl('Mutual Fund', all_lines, ignore.case = TRUE)),
        opening_lines = which(grepl('Opening Unit Balance:', all_lines, ignore.case = TRUE)),
        closing_lines = which(grepl('Closing Unit Balance:', all_lines, ignore.case = TRUE))
    )
    validate_cas_state(state)
    state
}

parse_cas_pdf <- function(file_path, password = '') {
    pages <- pdf_text(file_path, upw = password)
    cas_state_from_pages(pages)
}

fund_and_advisor <- function(folio_ord_num, state = legacy_cas_state()) {
    validate_cas_state(state)
    all_lines     <- state$all_lines
    folio_lines   <- state$folio_lines
    opening_lines <- state$opening_lines

    folio_to_txn_lines <- all_lines[folio_lines[folio_ord_num]:opening_lines[folio_ord_num]]
    fund_name_line <- folio_to_txn_lines[which(grepl(fund_name_pattern, folio_to_txn_lines))]
    if (length(fund_name_line) == 0) {
        stop('No fund-name line found for folio ordinal ', folio_ord_num)
    }
    mf_name <- trimws(strsplit(fund_name_line[1], "\\s{6}")[[1]][1])

    fund_advisor <- str_split(gsub(fund_advisor_pattern, '\\1:::\\2', mf_name), ':::')[[1]]
    fund_part <- trimws(fund_advisor[1])
    advisor_part <- if (length(fund_advisor) == 1) '' else trimws(fund_advisor[2])

    if (!grepl('INF[A-Z0-9]{9}', fund_part)) {
        isin_lines <- grep('ISIN:\\s*INF[A-Z0-9]{9}', folio_to_txn_lines, value = TRUE)
        if (length(isin_lines) > 0) {
            isin_m <- regmatches(isin_lines[1],
                                 regexpr('INF[A-Z0-9]{9}', isin_lines[1]))
            if (length(isin_m) == 1L)
                fund_part <- paste0(fund_part, ' - ISIN: ', isin_m)
        }
    }

    c(fund_part, advisor_part)
}

folio_and_pan <- function(folio_ord_num, state = legacy_cas_state()) {
    validate_cas_state(state)
    all_lines   <- state$all_lines
    folio_lines <- state$folio_lines

    folio_pan_split <- str_split(all_lines[folio_lines[folio_ord_num]], folio_pan_pattern)[[1]]
    if (length(folio_pan_split) < 2) {
        stop('No PAN marker found for folio ordinal ', folio_ord_num)
    }
    folio_num <- str_split(folio_pan_split[1], 'Folio No:\\s+')[[1]][2]
    pan_num <- substr(folio_pan_split[2], 1, 10)
    c(folio_num, pan_num)
}

get_transactions <- function(folio_ord_num, state = legacy_cas_state()) {
    validate_cas_state(state)
    all_lines     <- state$all_lines
    folio_lines   <- state$folio_lines
    closing_lines <- state$closing_lines
    amc_lines     <- state$amc_lines

    folio_range <- folio_lines[folio_ord_num]:closing_lines[folio_ord_num]
    working_set <- all_lines[folio_range]

    df_txns <- data.frame(date = character(), description = character(),
                          amount = character())
    for (i in seq_along(working_set)) {
        grouped_str <- gsub(transaction_pattern, '\\1xx\\2xx\\3', working_set[i])
        dt_desc_nums <- str_split(grouped_str, pattern = 'xx')
        separated_words <- unlist(lapply(dt_desc_nums[[1]], trimws))

        if (length(separated_words) == 3) {
            df_txns[nrow(df_txns) + 1, ] <- separated_words
        } else if (grepl('\\*+\\s*IDCW.*', separated_words)) {
            idcw_line <- gsub(idcw_pattern, '\\1:::\\2:::\\4', separated_words)
            dt_desc_value <- str_split(idcw_line, pattern = ':::')
            idcw_words <- unlist(lapply(dt_desc_value[[1]], trimws))
            idcw_words[3] <- paste(paste0('(', idcw_words[3], ')'), '0', '0', '0')
            df_txns[nrow(df_txns) + 1, ] <- idcw_words
        }
    }
    if (nrow(df_txns) == 0) {
        stop('No transaction rows parsed for folio ordinal ', folio_ord_num)
    }

    df_txns <- df_txns %>% separate(amount, c('amt', 'units', 'nav', 'bal_units'), '\\s+')
    dt_txns <- data.table(df_txns)
    dt_txns[, amt := gsub('\\(([,.0-9]+)\\)', '-\\1', amt)]
    dt_txns[, units := gsub('\\(([,.0-9]+)\\)', '-\\1', units)]

    dt_txns[, amt := as.numeric(gsub(',', '', amt))]
    dt_txns[, units := as.numeric(gsub(',', '', units))]
    dt_txns[, nav := as.numeric(gsub(',', '', nav))]
    dt_txns[, bal_units := as.numeric(gsub(',', '', bal_units))]
    dt_txns[, date := as.Date(date, format = '%d-%b-%Y')]

    dt_txn_dr <- dt_txns[description %like% 'IDCW Reinvest']
    if (nrow(dt_txn_dr) > 0) {
        dt_txn_dr$amt <- -dt_txn_dr$amt
        dt_txns <- rbind(dt_txns, dt_txn_dr)
    }
    dt_txns <- dt_txns[order(date)]

    closing_line <- gsub(closing_line_pattern, '\\1 \\2 \\3 \\4',
                         all_lines[closing_lines[folio_ord_num]])
    closing_strings <- str_split(closing_line, '\\s+')[[1]]
    if (length(closing_strings) < 4) {
        stop('Could not parse closing balance for folio ordinal ', folio_ord_num)
    }
    cur_value <- as.numeric(gsub(',', '', closing_strings[4]))
    if (!is.na(cur_value) && cur_value != 0) {
        dt_txns <- rbind(dt_txns, data.table(
            date = as.Date(closing_strings[3], format = '%d-%b-%Y'),
            description = 'Cur Value',
            amt = -cur_value,
            units = 0,
            nav = as.numeric(gsub(',', '', closing_strings[2])),
            bal_units = as.numeric(gsub(',', '', closing_strings[1]))
        ))
    }

    dt_txns[, days := as.numeric(max(dt_txns$date) - date)]
    dt_txns[, years := days / 365.25]

    folio_pan <- folio_and_pan(folio_ord_num, state)
    fund_adv  <- fund_and_advisor(folio_ord_num, state)
    prior_amc <- amc_lines[which(amc_lines < folio_lines[folio_ord_num])]
    amc_name <- if (length(prior_amc) == 0) NA_character_ else all_lines[tail(prior_amc, 1)]

    dt_txns[, amc := amc_name]
    dt_txns[, fund := fund_adv[1]]
    dt_txns[, advisor := fund_adv[2]]
    dt_txns[, folio := folio_pan[1]]
    dt_txns[, pan := folio_pan[2]]

    dt_txns
}

get_portfolio_transactions <- function(state_or_f_lines = legacy_cas_state(), state = NULL) {
    if (is.list(state_or_f_lines) && all(c('all_lines', 'folio_lines') %in% names(state_or_f_lines))) {
        state <- state_or_f_lines
        ordinals <- seq_along(state$folio_lines)
    } else {
        if (is.null(state)) state <- legacy_cas_state()
        ordinals <- seq_along(state_or_f_lines)
    }
    validate_cas_state(state)
    dt_txns <- rbindlist(lapply(ordinals, get_transactions, state = state), fill = TRUE)
    dt_txns <- dt_txns[order(date)]
    dt_txns[, days := as.numeric(max(dt_txns$date) - date)]
    dt_txns[, years := days / 365.25]
    dt_txns
}
