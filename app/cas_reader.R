# From: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
list.of.packages <- c("rjson", "data.table", "pdftools", "zeallot", "stringr", "dplyr", "tidyr", "tvm")

for (package in list.of.packages){
    if(!require(package, character.only=TRUE)){
        install.packages(package)
        library(package, character.only=TRUE)
    }
}

source('cas_regex.R')

XIRR <- function(dt_txn){
    out <- tryCatch(
    {
        fvs <- function(r){ sum(dt_txn$amt * (1 + r) ^ (dt_txn$years)) }
        unx <- uniroot(fvs, c(-1,1))
        return (unx$root)
    },
    error = function(cond){
        return (0)
    },
    warning = function(cond){
        return (0)
    },
    finally = {

    })
    return (out)
}

fund_and_advisor <- function(folio_ord_num){
    folio_to_txn_lines <- all_lines[folio_lines[folio_ord_num]:opening_lines[folio_ord_num]]
    fund_name_line <- folio_to_txn_lines[which(grepl(fund_name_pattern, folio_to_txn_lines))]
    # print(paste(folio_ord_num, fund_name_line))
    mf_name <- trimws(strsplit(fund_name_line, "\\s{6}")[[1]][1])

    fund_advisor <- str_split(gsub(fund_advisor_pattern, '\\1:::\\2', mf_name), ':::')[[1]]
    fund_part <- trimws(fund_advisor[1])
    if (length(fund_advisor) == 1){
        advisor_part <- ''
    } else {
        advisor_part <- trimws(fund_advisor[2])
    }
    return (c(fund_part, advisor_part))
}

folio_and_pan <- function(folio_ord_num){
    folio_pan_split <- str_split(all_lines[folio_lines[folio_ord_num]], folio_pan_pattern)[[1]]
    folio_num <- str_split(folio_pan_split, 'Folio No:\\s+')[[1]][2]
    pan_num <- substr(folio_pan_split[2], 1, 10)
    return(c(folio_num, pan_num))
}

get_transactions <- function(folio_ord_num){
    folio_range <- folio_lines[folio_ord_num]:closing_lines[folio_ord_num]
    working_set <- all_lines[folio_range]

    df_txns <- data.frame(date=character(), description=character(), amount=double())
    df_info <- data.frame(description=character())
    for (i in 1:length(working_set)){
        grouped_str <- gsub(transaction_pattern, '\\1xx\\2xx\\3', working_set[i])
        dt_desc_nums <- str_split(grouped_str, pattern = "xx")
        separated_words <- unlist(lapply(dt_desc_nums[[1]], trimws))

        if (length(separated_words) == 3){
            df_txns[nrow(df_txns) + 1, ]  <- separated_words
        } else {
            if(grepl('\\*+\\s*IDCW.*', separated_words)){
                idcw_line <- gsub(idcw_pattern, '\\1:::\\2:::\\4', separated_words)
                dt_desc_value <- str_split(idcw_line, pattern = ':::')
                idcw_words <- unlist(lapply(dt_desc_value[[1]], trimws))
                idcw_words[3] <- paste(paste0('(', idcw_words[3], ')'), '0', '0', '0')
                df_txns[nrow(df_txns) + 1, ]  <- idcw_words
            }
            df_info[nrow(df_info) + 1, ] <- separated_words
        }
    }

    df_txns <- df_txns %>% separate(amount, c("amt", "units", "nav", "bal_units"), "\\s+")
    dt_txns <- data.table(df_txns)
    dt_txns[, amt := gsub('\\(([,.0-9]+)\\)', '-\\1', amt)]
    dt_txns[, units := gsub('\\(([,.0-9]+)\\)', '-\\1', units)]

    dt_txns[, amt := as.numeric(gsub(',', '', amt))]
    dt_txns[, units := as.numeric(gsub(',', '', units))]
    dt_txns[, nav := as.numeric(gsub(',', '', nav))]
    dt_txns[, bal_units := as.numeric(gsub(',', '', bal_units))]
    dt_txns[, date := as.Date(date, format="%d-%b-%Y")]

    dt_txn_dr <- dt_txns[description %like% 'IDCW Reinvest']
    if(nrow(dt_txn_dr) > 0){
        dt_txn_dr$amt <- -dt_txn_dr$amt
        dt_txns <- rbind(dt_txns, dt_txn_dr)
    }
    dt_txns <- dt_txns[order(date)]  # Sorting with Earliest Date first

    closing_line <- gsub(closing_line_pattern, '\\1 \\2 \\3 \\4', all_lines[closing_lines[folio_ord_num]])
    closing_strings <- str_split(closing_line, '\\s+')[[1]]
    #print(closing_strings)
    cur_value <- as.numeric(gsub(',', '', closing_strings[4]))
    if (cur_value != 0){
        dt_txns <- rbind(dt_txns, data.table(
            date=as.Date(closing_strings[3], format="%d-%b-%Y"), description="Cur Value", amt=-cur_value,
            units=0, nav=as.numeric(gsub(',', '', closing_strings[2])),
            bal_units=as.numeric(gsub(',', '', closing_strings[1]))))
    }

    dt_txns[, days :=  as.numeric(max(dt_txns$date) - date)]
    dt_txns[, years := days/365.25]

    c(folio_num, pan_num) %<-% folio_and_pan(folio_ord_num)
    c(fund_part, advisor_part) %<-% fund_and_advisor(folio_ord_num)
    amc_name <- all_lines[tail(amc_lines[which(amc_lines < folio_lines[folio_ord_num])], 1)]

    dt_txns[, amc :=  amc_name]
    dt_txns[, fund :=  fund_part]
    dt_txns[, advisor :=  advisor_part]
    dt_txns[, folio :=  folio_num]
    dt_txns[, pan :=  pan_num]

    return (dt_txns)
}

get_mf_summary <- function(dt_txns, folio_ord_num, folio_id=''){
    first_date <- min(dt_txns$date)
    last_date <- max(dt_txns$date)

    closing_line <- gsub(closing_line_pattern, '\\1 \\2 \\3 \\4', all_lines[closing_lines[folio_ord_num]])
    closing_strings <- str_split(closing_line, '\\s+')[[1]]
    #print(closing_strings)
    cur_value <- as.numeric(gsub(',', '', closing_strings[4]))

    xirr_val <- XIRR(dt_txns)
    cash_in <- sum(dt_txns[amt > 0]$amt)
    cash_out <- -sum(dt_txns[amt < 0]$amt)
    # Realized and Unrealized Gains yet to be refined
    if (folio_ord_num == -1){
        df_mf <- rbind(data.frame(Folio = folio_id, Cur.Value = cur_value,
            Invested = cash_in, Redeemed = cash_out - cur_value,
            RealizedGains = ifelse(cur_value != 0, 0, cash_out - cash_in),
            UnrealizedGains = ifelse(cur_value != 0, cash_out - cash_in, 0),
            XIRR = xirr_val * 100, StartDate=first_date, RecentDate=last_date))
    } else {
        c(fund_part, advisor_part) %<-% fund_and_advisor(folio_ord_num)
        df_mf <- rbind(data.frame(Fund = fund_part, Cur.Value = cur_value,
            Invested = cash_in, Redeemed = cash_out - cur_value,
            RealizedGains = ifelse(cur_value != 0, 0, cash_out - cash_in),
            UnrealizedGains = ifelse(cur_value != 0, cash_out - cash_in, 0),
            XIRR = xirr_val * 100, StartDate=first_date, RecentDate=last_date))
    }
    return (df_mf)
}

get_mf_table <- function(folio_ord_num){
    dt_txns <- get_transactions(folio_ord_num)
    dt_mf_summary <- get_mf_summary(dt_txns, folio_ord_num)
    return (dt_mf_summary)
}

get_mf_table_for_txns <- function(dt_all_txns, folio_id){
    dt_txns <- dt_all_txns[folio == folio_id]
    dt_mf_summary <- get_mf_summary(dt_txns, -1, folio_id)
    return (dt_mf_summary)
}

get_portfolio_transactions <- function(f_lines){
    # Getting all transactions
    dt_txns <- rbindlist(lapply(c(1:length(f_lines)), get_transactions))
    dt_txns <- dt_txns[order(date)]  # Sorting with Earliest Date first
    dt_txns[, days :=  as.numeric(max(dt_txns$date) - date)]
    dt_txns[, years := days/365.25]

    return (dt_txns)
}

get_select_transactions <- function(selectors, presence){
    stopifnot(length(selectors) == length(presence))
    dt_cur_txns <- dt_all_txns
    for (i in c(1:length(selectors))){
        selector <- selectors[i]
        inclusion <- presence[i]
        if (inclusion == 'IN'){
            dt_cur_txns <- dt_cur_txns[get(names(selector)) %in% unlist(selector)]
        } else {
            dt_cur_txns <- dt_cur_txns[!get(names(selector)) %in% unlist(selector)]
        }
    }
    if(nrow(dt_cur_txns) > 0){
        dt_cur_txns[, days :=  as.numeric(max(dt_cur_txns$date) - date)]
        dt_cur_txns[, years := days/365.25]
    }
    return (dt_cur_txns)
}

get_navs <- function(scheme_code){
    mf_url <- paste0('https://api.mfapi.in/mf/', scheme_code)
    # Directly using readLines on the URL
    json_data <- fromJSON(paste(readLines(mf_url), collapse=""))

    # MF Info
    dt_mf_info <- data.table(t(data.frame(unlist(json_data[[1]]))))

    # MF NAVs
    dt_navs <- data.table(do.call(rbind.data.frame, json_data[[2]]))
    dt_navs[, date := as.Date(date, format="%d-%m-%Y")]
    dt_navs[, nav := as.numeric(nav)]
    dt_navs <- dt_navs[order(date)]

    # Fill in for all dates
    all_dates <- seq.Date(min(dt_navs$date), max(dt_navs$date), by=1)
    dt_all_dates <- data.table(all_dates)
    names(dt_all_dates) <- 'date'
    dt_navs <- merge(dt_all_dates, dt_navs, by='date', all.x=TRUE)
    # Get the next observed value carried backward for missing days ("nocb")
    dt_navs$nav <- nafill(dt_navs$nav, type='nocb')
    dt_navs <- dt_navs[nav != 0]
    return(dt_navs)
}

# ── NAV local cache ────────────────────────────────────────────────────────────

NAV_CACHE_DIR <- './nav_cache'

# Fetches NAV for a scheme, using a local RDS cache.
# Re-fetches from mfapi.in only when the cached data doesn't cover `required_date`
# (with a 7-day tolerance for weekends/holidays).
get_cached_navs <- function(scheme_code, required_date = Sys.Date()) {
    if (!dir.exists(NAV_CACHE_DIR)) dir.create(NAV_CACHE_DIR, recursive = TRUE)
    cache_file <- file.path(NAV_CACHE_DIR, paste0(scheme_code, '.rds'))

    if (file.exists(cache_file)) {
        dt_cached <- readRDS(cache_file)
        if (max(dt_cached$date) >= required_date - 7) {
            return(dt_cached)
        }
    }
    # Cache missing or stale — fetch from API and persist
    tryCatch({
        dt_navs <- get_navs(scheme_code)
        saveRDS(dt_navs, cache_file)
        dt_navs
    }, error = function(e) {
        if (file.exists(cache_file)) readRDS(cache_file) else NULL
    })
}

# ── Fund name matching ─────────────────────────────────────────────────────────

# Strips CAS-specific junk from the raw fund string stored in the `fund` column:
#   - Folio/scheme prefix    e.g. "123TSGPG-"  or "K168D-"
#   - ISIN suffix            e.g. "- ISIN: INF082J01069"
#   - (formerly ...)  /  (Erstwhile ...)  parentheticals
#   - (Non-Demat)
#   - (Advisor: ...)  — may be unclosed if line was truncated
#   - Registrar : CAMS  trailing text
# Plan/option words (Regular, Direct, Growth, IDCW, etc.) are intentionally
# kept because they identify distinct schemes with different NAVs.
extract_fund_name <- function(name) {
    name <- gsub("^[A-Z0-9]+\\s*-\\s*", "", name)               # folio prefix
    name <- gsub("\\s*-?\\s*ISIN:.*$", "", name, ignore.case = TRUE)
    name <- gsub("\\(formerly[^)]*\\)", "", name, ignore.case = TRUE)
    name <- gsub("\\(Erstwhile[^)]*\\)", "", name, ignore.case = TRUE)
    name <- gsub("\\(Non-Demat\\)", "", name, ignore.case = TRUE)
    name <- gsub("\\(Advisor:.*", "", name, ignore.case = TRUE)  # unclosed paren ok
    name <- gsub("\\s*Registrar\\s*:.*", "", name, ignore.case = TRUE)
    trimws(name)
}

# Normalises only formatting — lowercase, collapse hyphens (off-shore→offshore),
# collapse spaced single-letter abbreviations (U S→US), strip other punctuation.
normalize_fund_name <- function(name) {
    name <- tolower(name)
    name <- gsub("-", "", name)                              # off-shore → offshore
    name <- gsub("[^a-z0-9 ]", " ", name)                   # other punctuation → space
    name <- gsub("\\s+", " ", trimws(name))
    # Collapse adjacent single-letter words: "u s" → "us", "f i i" → "fii"
    # Apply twice to handle runs of three single letters
    name <- gsub("\\b([a-z]) ([a-z])\\b", "\\1\\2", name)
    name <- gsub("\\b([a-z]) ([a-z])\\b", "\\1\\2", name)
    name <- gsub("\\s+", " ", trimws(name))
    name
}

# Returns a scheme code string, or NA_character_ if no match found.
match_fund_to_scheme <- function(cas_fund_name, dt_mfs) {
    cleaned  <- extract_fund_name(cas_fund_name)
    norm_cas <- normalize_fund_name(cleaned)
    norm_mfs <- sapply(as.character(dt_mfs$schemeName), normalize_fund_name)
    codes    <- as.character(dt_mfs$schemeCode)

    # 1. Exact normalised match
    exact_idx <- which(norm_mfs == norm_cas)
    if (length(exact_idx) > 0) return(codes[exact_idx[1]])

    # 2. Approximate string match (handles "Direct Plan Growth" vs "Direct Growth" etc.)
    approx_idx <- agrep(norm_cas, norm_mfs, ignore.case = TRUE, max.distance = 0.3)
    if (length(approx_idx) > 0) return(codes[approx_idx[1]])

    NA_character_
}

# ── Portfolio valuation at a given date ───────────────────────────────────────

# Returns list(value = numeric, warnings = character vector).
# Uses bal_units from the last transaction before target_date for each folio,
# sums across folios per fund, then multiplies by NAV fetched from cache/API.
portfolio_value_at <- function(dt_base, target_date, fund_scheme_map) {
    funds   <- unique(dt_base[description != 'Cur Value']$fund)
    total   <- 0
    warns   <- character(0)

    for (f in funds) {
        prior <- dt_base[fund == f & date < target_date & description != 'Cur Value']
        if (nrow(prior) == 0) next

        # Sum last bal_units per folio (units are folio-specific)
        folios      <- unique(prior$folio)
        units_held  <- sum(sapply(folios, function(fol) {
            rows <- prior[folio == fol][order(date)]
            if (nrow(rows) == 0) return(0)
            rows[.N]$bal_units
        }))
        if (units_held <= 0) next

        scheme_code <- fund_scheme_map[[f]]
        if (is.na(scheme_code)) {
            warns <- c(warns, paste0("No scheme match for fund: ", f))
            next
        }

        dt_navs <- get_cached_navs(scheme_code, required_date = target_date)
        if (is.null(dt_navs) || nrow(dt_navs) == 0) {
            warns <- c(warns, paste0("NAV fetch failed for: ", f))
            next
        }
        nav_rows <- dt_navs[date <= target_date]
        if (nrow(nav_rows) == 0) {
            warns <- c(warns, paste0("No NAV available for ", f, " on or before ", target_date))
            next
        }
        total <- total + units_held * nav_rows[.N]$nav
    }
    list(value = total, warnings = warns)
}

# ── NAV cache pre-warmer ───────────────────────────────────────────────────────

# Iterates every fund in fund_scheme_map, pre-warms the RDS cache, and returns
# a data.table summarising what happened for each fund.
# Columns: Fund, SchemeCode, Source, NAVsUpTo
#   Source values: "Cache", "API - new", "API - refreshed",
#                  "No match", "Fetch failed", "Cache (stale, fetch failed)"
# progress_fn(fund_name_raw) is called before each fund so the caller can
# update a Shiny progress bar.
pre_warm_navs <- function(fund_scheme_map, required_date = Sys.Date(),
                          progress_fn = NULL) {
    if (!dir.exists(NAV_CACHE_DIR)) dir.create(NAV_CACHE_DIR, recursive = TRUE)

    rows <- lapply(names(fund_scheme_map), function(f) {
        if (!is.null(progress_fn)) progress_fn(f)

        scheme_code <- fund_scheme_map[[f]]
        clean_name  <- extract_fund_name(f)

        if (is.na(scheme_code)) {
            return(data.table(Fund       = clean_name,
                              SchemeCode = NA_character_,
                              Source     = 'No match',
                              NAVsUpTo   = NA_character_))
        }

        cache_file <- file.path(NAV_CACHE_DIR, paste0(scheme_code, '.rds'))

        if (file.exists(cache_file)) {
            dt_cached  <- readRDS(cache_file)
            max_cached <- max(dt_cached$date)
            if (max_cached >= required_date - 7) {
                return(data.table(Fund       = clean_name,
                                  SchemeCode = scheme_code,
                                  Source     = 'Cache',
                                  NAVsUpTo   = as.character(max_cached)))
            }
            source_label <- 'API - refreshed'
        } else {
            source_label <- 'API - new'
        }

        tryCatch({
            dt_navs <- get_navs(scheme_code)
            saveRDS(dt_navs, cache_file)
            data.table(Fund       = clean_name,
                       SchemeCode = scheme_code,
                       Source     = source_label,
                       NAVsUpTo   = as.character(max(dt_navs$date)))
        }, error = function(e) {
            if (file.exists(cache_file)) {
                dt_fb <- readRDS(cache_file)
                data.table(Fund       = clean_name,
                           SchemeCode = scheme_code,
                           Source     = 'Cache (stale, fetch failed)',
                           NAVsUpTo   = as.character(max(dt_fb$date)))
            } else {
                data.table(Fund       = clean_name,
                           SchemeCode = scheme_code,
                           Source     = 'Fetch failed',
                           NAVsUpTo   = NA_character_)
            }
        })
    })
    rbindlist(rows, fill = TRUE)
}

# ── Portfolio value curve ──────────────────────────────────────────────────────

# Samples portfolio value and cumulative net investment at monthly intervals.
# Returns a data.table with columns:
#   date            – sample date
#   portfolio_value – total value of all holdings (units × NAV)
#   net_invested    – cumulative cash invested minus redemptions up to that date
#   gains           – portfolio_value − net_invested  (can be negative)
#
# Relies on the RDS NAV cache; call after pre_warm_navs() for speed.
get_portfolio_curve <- function(dt_base, fund_scheme_map, sample_by = 'month') {
    funds      <- unique(dt_base[description != 'Cur Value']$fund)
    end_date   <- max(dt_base[description == 'Cur Value']$date)
    start_date <- min(dt_base[description != 'Cur Value']$date)

    sample_dates <- seq.Date(start_date, end_date, by = sample_by)
    if (!(end_date %in% sample_dates)) sample_dates <- c(sample_dates, end_date)
    n_dates <- length(sample_dates)

    dt_sample <- data.table(date = sample_dates)
    setkey(dt_sample, date)

    portfolio_values <- numeric(n_dates)

    for (f in funds) {
        scheme_code <- fund_scheme_map[[f]]
        if (is.na(scheme_code)) next

        dt_navs <- get_cached_navs(scheme_code, required_date = end_date)
        if (is.null(dt_navs) || nrow(dt_navs) == 0) next
        setkey(dt_navs, date)

        fund_txns <- dt_base[fund == f & description != 'Cur Value'][order(date)]
        folios    <- unique(fund_txns$folio)

        # Units held at each sample date: last bal_units per folio, rolled forward
        units_vec <- numeric(n_dates)
        for (fol in folios) {
            # One row per date (take last transaction if multiple on same day)
            folio_bal <- fund_txns[folio == fol, .SD[.N], by = date][, .(date, bal_units)]
            if (nrow(folio_bal) == 0) next
            setkey(folio_bal, date)
            joined    <- folio_bal[dt_sample, roll = TRUE]
            bal       <- joined$bal_units
            bal[is.na(bal)] <- 0
            units_vec <- units_vec + bal
        }

        # NAV at each sample date, rolled forward over weekends/holidays
        nav_joined <- dt_navs[dt_sample, roll = TRUE]
        nav_vec    <- nav_joined$nav
        nav_vec[is.na(nav_vec)] <- 0

        portfolio_values <- portfolio_values + units_vec * nav_vec
    }

    # Cumulative net investment (investments positive, redemptions negative)
    daily_net <- dt_base[description != 'Cur Value',
                         .(daily_amt = sum(amt)), by = date][order(date)]
    daily_net[, cum_amt := cumsum(daily_amt)]
    setkey(daily_net, date)
    cum_joined   <- daily_net[dt_sample, roll = TRUE]
    cum_invested <- cum_joined$cum_amt
    cum_invested[is.na(cum_invested)] <- 0
    cum_invested <- pmax(cum_invested, 0)   # clamp: can't invest negative

    data.table(
        date            = sample_dates,
        portfolio_value = round(portfolio_values, 2),
        net_invested    = round(cum_invested, 2),
        gains           = round(portfolio_values - cum_invested, 2)
    )
}
