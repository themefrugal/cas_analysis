# Analytics layer: cash-flow classification, XIRR, summaries, valuations, curves.

external_cashflows <- function(dt_txns, exclude_switches = TRUE) {
    dt <- dt_txns[description != 'Cur Value']
    if (exclude_switches) {
        dt <- dt[!grepl('^Switch', description, ignore.case = TRUE)]
    }
    dt
}

position_cashflows <- function(dt_txns) {
    dt_txns[description != 'Cur Value']
}

recalc_xirr_basis <- function(dt_txns, as_of = NULL) {
    if (nrow(dt_txns) == 0) return(dt_txns)
    if (is.null(as_of)) as_of <- max(dt_txns$date)
    dt <- copy(dt_txns)
    dt[, days := as.numeric(as_of - date)]
    dt[, years := days / 365.25]
    dt
}

XIRR <- function(dt_txn) {
    if (nrow(dt_txn) == 0 || !any(dt_txn$amt > 0) || !any(dt_txn$amt < 0)) {
        return(NA_real_)
    }
    fvs <- function(r) sum(dt_txn$amt * (1 + r) ^ (dt_txn$years))
    intervals <- list(c(-0.9999, 1), c(-0.9999, 10), c(-0.9999, 100))

    for (interval in intervals) {
        vals <- tryCatch(c(fvs(interval[1]), fvs(interval[2])),
                         error = function(e) c(NA_real_, NA_real_))
        if (any(is.na(vals)) || vals[1] * vals[2] > 0) next
        out <- tryCatch(uniroot(fvs, interval)$root,
                        error = function(e) NA_real_,
                        warning = function(w) NA_real_)
        if (!is.na(out)) return(out)
    }
    NA_real_
}

get_mf_summary <- function(dt_txns, folio_ord_num = -1, folio_id = '',
                           include_switches = TRUE) {
    first_date <- min(dt_txns$date)
    last_date <- max(dt_txns$date)
    cur_value <- -sum(dt_txns[description == 'Cur Value']$amt)

    dt_xirr <- rbindlist(list(
        external_cashflows(dt_txns, exclude_switches = !include_switches)[, .(date, amt)],
        dt_txns[description == 'Cur Value', .(date, amt)]
    ), fill = TRUE)
    dt_xirr <- recalc_xirr_basis(dt_xirr)
    xirr_val <- XIRR(dt_xirr)

    cash_txns <- if (include_switches) {
        position_cashflows(dt_txns)
    } else {
        external_cashflows(dt_txns)
    }
    cash_in  <- sum(cash_txns[amt > 0]$amt)
    cash_out <- -sum(cash_txns[amt < 0]$amt)

    redemptions <- cash_out
    total_out <- redemptions + cur_value
    cost_of_redemptions <- if (total_out > 0) cash_in * redemptions / total_out else 0
    realized_gains <- redemptions - cost_of_redemptions
    unrealized_gains <- cur_value - (cash_in - cost_of_redemptions)
    xirr_pct <- if (is.na(xirr_val)) NA_real_ else xirr_val * 100

    if (folio_ord_num == -1) {
        data.frame(Folio = folio_id, Cur.Value = cur_value,
                   Invested = cash_in, Redeemed = redemptions,
                   RealizedGains = realized_gains,
                   UnrealizedGains = unrealized_gains,
                   XIRR = xirr_pct, StartDate = first_date,
                   RecentDate = last_date)
    } else {
        fund_part <- unique(dt_txns$fund)[1]
        data.frame(Fund = fund_part, Cur.Value = cur_value,
                   Invested = cash_in, Redeemed = redemptions,
                   RealizedGains = realized_gains,
                   UnrealizedGains = unrealized_gains,
                   XIRR = xirr_pct, StartDate = first_date,
                   RecentDate = last_date)
    }
}

get_mf_table <- function(folio_ord_num, state = legacy_cas_state()) {
    dt_txns <- get_transactions(folio_ord_num, state)
    get_mf_summary(dt_txns, folio_ord_num)
}

get_mf_table_for_txns <- function(dt_all_txns, folio_id) {
    dt_txns <- dt_all_txns[folio == folio_id]
    if (nrow(dt_txns) > 0) dt_txns <- recalc_xirr_basis(dt_txns)
    get_mf_summary(dt_txns, -1, folio_id)
}

get_select_transactions <- function(selectors, presence, dt_all_txns = NULL) {
    stopifnot(length(selectors) == length(presence))
    if (is.null(dt_all_txns)) dt_all_txns <- get('dt_all_txns', envir = .GlobalEnv)
    dt_cur_txns <- dt_all_txns
    for (i in seq_along(selectors)) {
        selector <- selectors[i]
        inclusion <- presence[i]
        if (inclusion == 'IN') {
            dt_cur_txns <- dt_cur_txns[get(names(selector)) %in% unlist(selector)]
        } else {
            dt_cur_txns <- dt_cur_txns[!get(names(selector)) %in% unlist(selector)]
        }
    }
    if (nrow(dt_cur_txns) > 0) dt_cur_txns <- recalc_xirr_basis(dt_cur_txns)
    dt_cur_txns
}

portfolio_value_at <- function(dt_base, target_date, fund_scheme_map) {
    funds <- unique(dt_base[description != 'Cur Value']$fund)
    total <- 0
    warns <- character(0)

    for (f in funds) {
        prior <- dt_base[fund == f & date < target_date & description != 'Cur Value']
        if (nrow(prior) == 0) next

        folios <- unique(prior$folio)
        units_held <- sum(vapply(folios, function(fol) {
            rows <- prior[folio == fol][order(date)]
            if (nrow(rows) == 0) return(0)
            rows[.N]$bal_units
        }, numeric(1)))
        if (units_held <= 0) next

        scheme_code <- fund_scheme_map[[f]]
        if (is.null(scheme_code) || is.na(scheme_code)) {
            warns <- c(warns, paste0('No scheme match for fund: ', f))
            next
        }

        dt_navs <- get_cached_navs(scheme_code, required_date = target_date)
        if (is.null(dt_navs) || nrow(dt_navs) == 0) {
            warns <- c(warns, paste0('NAV fetch failed for: ', f))
            next
        }
        nav_rows <- dt_navs[date <= target_date]
        if (nrow(nav_rows) == 0) {
            warns <- c(warns, paste0('No NAV available for ', f,
                                     ' on or before ', target_date))
            next
        }
        total <- total + units_held * nav_rows[.N]$nav
    }
    list(value = total, warnings = warns)
}

portfolio_values_by_fund_at <- function(dt_base, target_date, fund_scheme_map) {
    funds <- unique(dt_base[description != 'Cur Value']$fund)
    rows <- lapply(funds, function(f) {
        prior <- dt_base[fund == f & date < target_date & description != 'Cur Value']
        if (nrow(prior) == 0) {
            return(data.table(fund = f, date = target_date, amt = 0))
        }
        units_held <- sum(vapply(unique(prior$folio), function(fol) {
            rows <- prior[folio == fol][order(date)]
            if (nrow(rows) == 0) return(0)
            rows[.N]$bal_units
        }, numeric(1)))
        scheme_code <- fund_scheme_map[[f]]
        if (is.null(scheme_code) || is.na(scheme_code) || units_held <= 0) {
            return(data.table(fund = f, date = target_date, amt = 0))
        }
        dt_navs <- get_cached_navs(scheme_code, required_date = target_date)
        if (is.null(dt_navs) || nrow(dt_navs) == 0) {
            return(data.table(fund = f, date = target_date, amt = 0))
        }
        nav_rows <- dt_navs[date <= target_date]
        if (nrow(nav_rows) == 0) {
            return(data.table(fund = f, date = target_date, amt = 0))
        }
        data.table(fund = f, date = nav_rows[.N]$date,
                   amt = -(units_held * nav_rows[.N]$nav))
    })
    rbindlist(rows, fill = TRUE)
}

get_portfolio_curve <- function(dt_base, fund_scheme_map, sample_by = 'month') {
    funds <- unique(dt_base[description != 'Cur Value']$fund)
    end_date <- max(dt_base[description == 'Cur Value']$date)
    start_date <- min(dt_base[description != 'Cur Value']$date)

    sample_dates <- seq.Date(start_date, end_date, by = sample_by)
    if (!(end_date %in% sample_dates)) sample_dates <- c(sample_dates, end_date)
    n_dates <- length(sample_dates)

    dt_sample <- data.table(date = sample_dates)
    setkey(dt_sample, date)
    portfolio_values <- numeric(n_dates)

    for (f in funds) {
        scheme_code <- fund_scheme_map[[f]]
        if (is.null(scheme_code) || is.na(scheme_code)) next

        dt_navs <- get_cached_navs(scheme_code, required_date = end_date)
        if (is.null(dt_navs) || nrow(dt_navs) == 0) next
        setkey(dt_navs, date)

        fund_txns <- dt_base[fund == f & description != 'Cur Value'][order(date)]
        units_vec <- numeric(n_dates)
        for (fol in unique(fund_txns$folio)) {
            folio_bal <- fund_txns[folio == fol, .SD[.N], by = date][, .(date, bal_units)]
            if (nrow(folio_bal) == 0) next
            setkey(folio_bal, date)
            bal <- folio_bal[dt_sample, roll = TRUE]$bal_units
            bal[is.na(bal)] <- 0
            units_vec <- units_vec + bal
        }

        nav_vec <- dt_navs[dt_sample, roll = TRUE]$nav
        nav_vec[is.na(nav_vec)] <- 0
        portfolio_values <- portfolio_values + units_vec * nav_vec
    }

    daily_net <- external_cashflows(dt_base)[, .(daily_amt = sum(amt)), by = date][order(date)]
    daily_net[, cum_amt := cumsum(daily_amt)]
    setkey(daily_net, date)
    cum_invested <- daily_net[dt_sample, roll = TRUE]$cum_amt
    cum_invested[is.na(cum_invested)] <- 0
    cum_invested <- pmax(cum_invested, 0)

    data.table(
        date = sample_dates,
        portfolio_value = round(portfolio_values, 2),
        net_invested = round(cum_invested, 2),
        gains = round(portfolio_values - cum_invested, 2)
    )
}

get_allocation_summary <- function(dt_leaves, group_col, top_n = NULL) {
    if (nrow(dt_leaves) == 0) return(data.table())
    dt <- as.data.table(dt_leaves)
    out <- dt[, .(`Cur Value` = sum(`Cur Value`, na.rm = TRUE),
                  Gains = sum(Gains, na.rm = TRUE)),
              by = group_col]
    total_value <- sum(out$`Cur Value`, na.rm = TRUE)
    out[, Weight := if (total_value > 0) `Cur Value` / total_value * 100 else NA_real_]
    setorder(out, -`Cur Value`)
    if (!is.null(top_n) && nrow(out) > top_n) out <- out[seq_len(top_n)]
    setnames(out, group_col, 'Group')
    out
}

get_performance_contributors <- function(dt_leaves, top_n = 10) {
    if (nrow(dt_leaves) == 0) return(list(top = data.table(), bottom = data.table()))
    dt <- as.data.table(dt_leaves)
    by_scheme <- dt[, .(`Cur Value` = sum(`Cur Value`, na.rm = TRUE),
                        Invested = sum(Invested, na.rm = TRUE),
                        Redeemed = sum(Redeemed, na.rm = TRUE),
                        Gains = sum(Gains, na.rm = TRUE)),
                    by = Scheme]
    setorder(by_scheme, -Gains)
    list(
        top = head(by_scheme, top_n),
        bottom = head(by_scheme[order(Gains)], top_n)
    )
}

build_quality_diagnostics <- function(dt_base, fund_scheme_map, fund_category_map,
                                      nav_status_log, period_warnings = character(0)) {
    funds <- unique(dt_base[description != 'Cur Value']$fund)
    unmatched <- names(Filter(function(x) is.null(x) || is.na(x), fund_scheme_map))
    cat_unknown <- fund_category_map[is.na(Category) | Category == '(Unknown)' |
                                         is.na(SubCategory) | SubCategory == '(Unknown)']
    switch_rows <- dt_base[grepl('^Switch', description, ignore.case = TRUE)]
    stale_nav <- nav_status_log[grepl('stale|failed|No match', Source, ignore.case = TRUE)]

    data.table(
        Check = c('Funds parsed', 'Unmatched NAV schemes', 'Unknown categories',
                  'Switch transactions', 'NAV cache issues', 'Period warnings'),
        Count = c(length(funds), length(unmatched), nrow(unique(cat_unknown, by = 'Fund')),
                  nrow(switch_rows), nrow(stale_nav), length(period_warnings)),
        Severity = c('Info',
                     ifelse(length(unmatched) > 0, 'High', 'OK'),
                     ifelse(nrow(cat_unknown) > 0, 'Medium', 'OK'),
                     ifelse(nrow(switch_rows) > 0, 'Info', 'OK'),
                     ifelse(nrow(stale_nav) > 0, 'Medium', 'OK'),
                     ifelse(length(period_warnings) > 0, 'Medium', 'OK')),
        Detail = c(
            'Distinct fund strings found in the CAS.',
            'Funds without a resolved mfapi/AMFI scheme code.',
            'Funds missing AMFI category or sub-category metadata.',
            'Internal switch rows detected and excluded from portfolio-level cash flows.',
            'NAV rows that used stale cache, failed fetches, or had no scheme match.',
            'Warnings generated by selected-period valuation.'
        )
    )
}
