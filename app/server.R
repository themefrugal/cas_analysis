source('cas_reader.R')
library(DT)
library(shiny)
library(memoise)
library(purrr)
library(plotly)
library(reactable)

if (!file.exists('./mf_codes_equity.RData') || !file.exists('./mf_codes.RData')) {
    stop('Scheme data is missing. Run Rscript refresh_mf_codes.R from the app directory first.')
}

read_from_internet <- FALSE
if (read_from_internet){
    mf_list_url <- 'https://api.mfapi.in/mf'
    mf_list <- fromJSON(paste(readLines(mf_list_url), collapse=""))
    dt_mfs <- data.table(do.call(rbind.data.frame, mf_list))

    dt_mfs$schemeName <- as.character(dt_mfs$schemeName)
    dt_mfs <- dt_mfs[order(schemeName)]
    dt_mfs <- unique(dt_mfs)
    save(dt_mfs, file = './mf_codes.RData')
} else {
    load('./mf_codes_equity.RData')   # dt_mfs — equity only, used for benchmark dropdown
    # Full fund list for NAV matching (covers debt, gilt, liquid etc.)
    if (file.exists('./mf_codes.RData')) {
        load('./mf_codes.RData')
        dt_mfs_all <- dt_mfs          # mf_codes.RData also saves as dt_mfs; rename
        load('./mf_codes_equity.RData') # restore dt_mfs to equity-only for dropdown
    } else {
        dt_mfs_all <- dt_mfs          # fallback: equity only
    }
}
scheme_lookup_all <- get_prepared_scheme_lookup(dt_mfs_all)

get_scheme_code <- function(mf_name){
    # Check this: There are multiple scheme codes for the same scheme name (in approx 20 instances)
    # As of now, we are taking the occurrence of first such instance
    scheme_code <- dt_mfs[schemeName == mf_name]$schemeCode[1]
    return (scheme_code)
}
mnav <- memoise(compose(get_navs, get_scheme_code))

dt_options <- function(...) {
    modifyList(
        list(pageLength = 25, autoWidth = TRUE, scrollX = TRUE),
        list(...)
    )
}

dt_class <- 'compact stripe hover order-column'

`%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}

money_label <- function(x) {
    ifelse(is.na(x), 'N/A', paste0('Rs. ', format(round(x, 0), big.mark = ',')))
}

pct_label <- function(x) {
    ifelse(is.na(x), 'N/A', paste0(round(x, 3), '%'))
}

prepare_bar_dt <- function(dt, y_col, sort_order = 'desc') {
    dt <- copy(as.data.table(dt))
    if (nrow(dt) == 0 || !y_col %in% names(dt)) return(dt)
    if (identical(sort_order, 'asc')) {
        setorderv(dt, y_col, order = 1, na.last = TRUE)
    } else {
        setorderv(dt, y_col, order = -1, na.last = TRUE)
    }
    dt
}

plot_bar <- function(dt, x_col, y_col, title = NULL, color = '#1f5f8b',
                     sort_order = 'desc', percent = FALSE) {
    req(nrow(dt) > 0)
    dt <- prepare_bar_dt(dt, y_col, sort_order)
    suffix <- if (percent) '%' else ''
    hover_fmt <- if (percent) '%{y:.2f}%' else '%{y:,.0f}'
    plot_ly(dt, x = as.formula(paste0('~`', x_col, '`')),
            y = as.formula(paste0('~`', y_col, '`')),
            type = 'bar',
            marker = list(color = color),
            hovertemplate = paste0('%{x}<br>', hover_fmt, '<extra></extra>')) %>%
        layout(title = title,
               xaxis = list(title = '', categoryorder = 'array',
                            categoryarray = dt[[x_col]]),
               yaxis = list(title = suffix),
               margin = list(b = 90))
}

html_table <- function(df) {
    if (is.null(df) || nrow(df) == 0) return('<p>No data available.</p>')
    header <- paste0('<tr>', paste0('<th>', htmltools::htmlEscape(names(df)), '</th>',
                                    collapse = ''), '</tr>')
    rows <- apply(as.data.frame(df), 1, function(row) {
        paste0('<tr>', paste0('<td>', htmltools::htmlEscape(as.character(row)),
                              '</td>', collapse = ''), '</tr>')
    })
    paste0('<table>', header, paste(rows, collapse = ''), '</table>')
}

format_money_cols <- function(tbl, columns, digits = 2) {
    tbl %>%
        formatCurrency(columns = columns, currency = '', interval = 3,
                       mark = ',', digits = digits) %>%
        formatStyle(columns = columns, textAlign = 'right')
}

format_number_cols <- function(tbl, columns, digits = 3) {
    tbl %>%
        formatRound(columns = columns, digits = digits) %>%
        formatStyle(columns = columns, textAlign = 'right')
}

format_pct_cols <- function(tbl, columns, digits = 3) {
    tbl %>%
        formatRound(columns = columns, digits = digits) %>%
        formatStyle(columns = columns, textAlign = 'right')
}

get_fund_summary_dt <- function(dt_all, fund_name) {
    dt_fund <- dt_all[fund == fund_name]

    # Recalculate days/years using THIS fund's own max date, not the
    # portfolio-wide max date inherited from dt_filtered_txns().
    if (nrow(dt_fund) > 0) {
        dt_fund[, days  := as.numeric(max(dt_fund$date) - date)]
        dt_fund[, years := days / 365.25]
    }

    get_mf_summary(dt_fund, folio_ord_num = 1)
}

function(input, output, session) {
    updateSelectizeInput(session, "mf_name", choices = unique(dt_mfs$schemeName), server=TRUE)
    nav_status_cache <- reactiveVal(NULL)
    analysis_ready <- reactiveVal(FALSE)
    health_tabs_visible <- reactiveVal(FALSE)

    set_health_tabs_visible <- function(visible) {
        health_tabs_visible(visible)
        if (isTRUE(visible)) {
            showTab(inputId = "main_nav", target = "diagnostics", session = session)
            showTab(inputId = "main_nav", target = "nav_status", session = session)
            updateActionButton(session, "toggle_health_tabs", label = "Hide health-check tabs")
        } else {
            if (input$main_nav %in% c("diagnostics", "nav_status")) {
                updateNavbarPage(session, "main_nav", selected = "Summary")
            }
            hideTab(inputId = "main_nav", target = "diagnostics", session = session)
            hideTab(inputId = "main_nav", target = "nav_status", session = session)
            updateActionButton(session, "toggle_health_tabs", label = "Show health-check tabs")
        }
    }

    session$onFlushed(function() {
        set_health_tabs_visible(FALSE)
    }, once = TRUE)

    observeEvent(input$toggle_health_tabs, {
        set_health_tabs_visible(!isTRUE(health_tabs_visible()))
    }, ignoreInit = TRUE)

    init_proc <- reactive({
        req(input$file1, input$file1$datapath)
        parse_cas_pdf(input$file1$datapath, input$password)
    })

    observeEvent(input$file1, {
        analysis_ready(FALSE)
        nav_status_cache(NULL)
        period_warnings(character(0))
    }, ignoreInit = TRUE)

    dt_base_txns <- eventReactive(input$btn_proc, {
        withProgress(message = 'Parsing CAS PDF...', value = 0.5, {
            get_portfolio_transactions(init_proc())
        })
    })

    # Built once per PDF load — pure string matching, no API calls.
    # Pre-computes normalized scheme names ONCE (6,000+ entries), then passes
    # them to match_fund_to_scheme for each fund.  Previously the normalization
    # was repeated inside match_fund_to_scheme for every fund, causing ~120,000+
    # redundant regex operations.
    fund_scheme_map <- eventReactive(input$btn_proc, {
        funds    <- unique(dt_base_txns()[description != 'Cur Value']$fund)
        map      <- lapply(funds, function(f) match_fund_to_scheme(f, scheme_lookup_all))
        names(map) <- funds
        map
    })

    # Category/SubCategory per fund, from AMFI's NAVAll.txt (ISIN-matched,
    # falling back to the already-resolved fund_scheme_map code). Built once
    # per PDF load alongside fund_scheme_map.
    fund_category_map <- eventReactive(input$btn_proc, {
        funds <- unique(dt_base_txns()[description != 'Cur Value']$fund)
        build_fund_category_map(funds, fund_scheme_map(), get_navall_categorized())
    })

    # Pre-warms NAV cache for all funds immediately after PDF loads.
    # Shows a progress bar while fetching from mfapi.in, then stores
    # the per-fund status (Cache / API-new / API-refreshed / No match / failed).
    nav_status_log <- reactive({
        req(nav_status_cache())
        nav_status_cache()
    })

    # Monthly portfolio value curve — computed ONCE per PDF load (eventReactive).
    # Does NOT depend on input$date_range so updating the date picker never
    # triggers an expensive recompute. get_portfolio_curve() reads from the
    # RDS cache directly, so no coupling to nav_status_log is needed.
    dt_portfolio_curve <- eventReactive(input$btn_proc, {
        get_portfolio_curve(dt_base_txns(), fund_scheme_map())
    })

    # Rolling inception-to-date XIRR sampled at each monthly point in the curve.
    # Reuses dt_portfolio_curve() values as the synthetic closing row — no extra
    # NAV fetches needed.  Computed once per PDF load.
    dt_xirr_curve <- eventReactive(input$btn_proc, {
        dt_base  <- dt_base_txns()
        dt_curve <- dt_portfolio_curve()
        # Exclude Switch-Out / Switch-In transactions.  These are intra-portfolio
        # fund reorganisations (voluntary switches or SEBI-mandated scheme mergers)
        # and carry no actual investor cash flow.  Including them can create
        # artificial XIRR spikes when a large merger Switch occurs, because the
        # two legs live in different folio sections and any scheme-mapping
        # asymmetry introduces a spurious imbalance in the cashflow vector.
        dt_txns  <- external_cashflows(dt_base)[, .(date, amt)]

        xirr_vals <- sapply(seq_len(nrow(dt_curve)), function(i) {
            d  <- dt_curve$date[i]
            pv <- dt_curve$portfolio_value[i]
            if (is.na(pv) || pv <= 0) return(NA_real_)

            prior <- dt_txns[date <= d]
            if (nrow(prior) == 0) return(NA_real_)

            dt_cf <- rbind(prior, data.table(date = d, amt = -pv))
            if (!any(dt_cf$amt > 0) || !any(dt_cf$amt < 0)) return(NA_real_)

            dt_cf[, days  := as.numeric(d - date)]
            dt_cf[, years := days / 365.25]
            XIRR(dt_cf)
        })

        data.table(date = dt_curve$date, xirr = xirr_vals * 100)
    })

    period_warnings <- reactiveVal(character(0))

    # Single orchestrated observer for the full post-upload pipeline.
    # All slow work happens here with a visible progress bar so the user
    # always knows what the app is doing.  analysis_ready(TRUE) is set only
    # after every step completes, so no downstream reactive fires prematurely.
    observeEvent(input$btn_proc, {
        analysis_ready(FALSE)

        withProgress(message = 'Processing CAS...', value = 0, {

            setProgress(0.05, detail = 'Parsing PDF...')
            dt <- dt_base_txns()

            setProgress(0.15, detail = 'Matching fund names...')
            fsm <- fund_scheme_map()

            setProgress(0.25, detail = 'Loading category data...')
            fund_category_map()

            # NAV pre-warming — largest chunk, shown per fund
            n <- length(fsm)
            i <- 0L
            nav_status <- pre_warm_navs(fsm, required_date = Sys.Date(),
                progress_fn = function(fname) {
                    i <<- i + 1L
                    setProgress(
                        value  = 0.25 + 0.55 * (i / n),
                        detail = paste0('NAV (', i, '/', n, ') ',
                                        extract_fund_name(fname))
                    )
                })
            nav_status_cache(nav_status)

            setProgress(0.82, detail = 'Computing portfolio curve...')
            dt_portfolio_curve()

            setProgress(0.92, detail = 'Computing XIRR history...')
            dt_xirr_curve()

            setProgress(1.0, detail = 'Done.')
        })

        non_cv    <- dt[description != 'Cur Value']$date
        cas_close <- max(dt[description == 'Cur Value']$date)
        updateDateRangeInput(session, "date_range",
            start = min(non_cv), end = cas_close)
        updateSelectizeInput(session, "fund_detail",
            choices = sort(unique(dt[description != 'Cur Value']$fund)),
            selected = sort(unique(dt[description != 'Cur Value']$fund))[1],
            server = TRUE)
        analysis_ready(TRUE)

    }, ignoreInit = TRUE)

    dt_filtered_txns <- reactive({
        dt <- dt_base_txns()
        start_d <- input$date_range[1]
        end_d <- input$date_range[2]
        if (!is.null(start_d) && !is.null(end_d)) {
            dt <- dt[description == 'Cur Value' | (date >= start_d & date <= end_d)]
        }
        if (nrow(dt) > 0) {
            dt[, days := as.numeric(max(dt$date) - date)]
            dt[, years := days/365.25]
        }
        dt
    })

    observeEvent(input$analytics_custom_cols, {
        raw <- input$analytics_custom_cols
        sanitized <- sanitize_custom_analytics_groups(raw)
        choices <- available_custom_analytics_groups(sanitized)

        if (!identical(raw, sanitized)) {
            updateSelectizeInput(session, 'analytics_custom_cols',
                                 choices = choices, selected = sanitized,
                                 server = TRUE)
        } else {
            updateSelectizeInput(session, 'analytics_custom_cols',
                                 choices = choices, selected = raw,
                                 server = TRUE)
        }
    }, ignoreInit = FALSE)

    analytics_group_cols <- reactive({
        hierarchy_map <- list(
            'AMC -> Category -> Sub-Category -> Scheme' =
                c('AMC', 'Category', 'SubCategory', 'Scheme'),
            'AMC -> Folio -> Category -> Sub-Category -> Scheme' =
                c('AMC', 'Folio', 'Category', 'SubCategory', 'Scheme'),
            'Category -> Sub-Category -> AMC -> Scheme' =
                c('Category', 'SubCategory', 'AMC', 'Scheme'),
            'Category -> AMC -> Sub-Category -> Scheme' =
                c('Category', 'AMC', 'SubCategory', 'Scheme')
        )
        sel <- input$analytics_hierarchy
        if (identical(sel, 'Custom')) {
            raw <- sanitize_custom_analytics_groups(input$analytics_custom_cols)
            group_cols <- analytics_group_label_to_col(raw)
        } else {
            group_cols <- hierarchy_map[[sel]]
        }
        valid_cols <- c('AMC', 'Folio', 'Category', 'SubCategory', 'Scheme')
        group_cols <- intersect(group_cols, valid_cols)
        if (is.null(group_cols) || length(group_cols) == 0) {
            group_cols <- c('AMC', 'Category', 'SubCategory', 'Scheme')
        }
        group_cols
    })

    dt_analytics_source <- reactive({
        req(analysis_ready())
        dt      <- dt_filtered_txns()
        cat_map <- fund_category_map()

        dt <- merge(dt, cat_map[, .(Fund, Category, SubCategory)],
                    by.x = 'fund', by.y = 'Fund', all.x = TRUE)
        dt[, Scheme := extract_fund_name(fund)]
        dt[, AMC    := trimws(amc)]
        dt[, Folio  := folio]
        dt[is.na(Category),    Category    := '(Unknown)']
        dt[is.na(SubCategory), SubCategory := '(Unknown)']
        dt[is.na(AMC),         AMC         := '(Unknown)']
        dt
    })

    # Leaf-level summary remains the source for cards and insight widgets.
    dt_analytics_leaves <- reactive({
        dt <- dt_analytics_source()
        dt[, summarise_position_group(.SD), by = .(AMC, Category, SubCategory, Scheme, Folio)]
    })

    # The Analytics grid displays precomputed hierarchy rows. XIRR cannot be
    # summed, so every hierarchy level is recalculated from its own cash flows.
    dt_analytics_hierarchy <- reactive({
        build_hierarchy_xirr_table(dt_analytics_source(), analytics_group_cols())
    })

    dt_mf_xirrs <- reactive({
        req(analysis_ready())
        dt <- dt_filtered_txns()
        funds <- unique(dt[description != 'Cur Value']$fund)
        dt_full <- rbindlist(lapply(funds, function(f) get_fund_summary_dt(dt, f)))
        names(dt_full)[names(dt_full) == 'XIRR'] <- 'XIRR%'
        dt_full <- merge(dt_full, fund_category_map()[, !'SchemeType'], by = 'Fund', all.x = TRUE)
        # Put Category/SubCategory right after Fund rather than wherever merge() lands them
        setcolorder(dt_full, c('Fund', 'Category', 'SubCategory'))
        dt_full
    })

    dt_folio_xirrs <- eventReactive(input$btn_proc, {
        dt_all_txns <- dt_base_txns()
        folio_ids <- unique(dt_all_txns$folio)
        list_table <- list()
        for (folio_id in folio_ids){
            list_table <- c(list_table, list(get_mf_table_for_txns(dt_all_txns, folio_id)))
        }
        dt_full_table <- rbindlist(list_table)
        names(dt_full_table)[names(dt_full_table) == 'XIRR'] <- 'XIRR%'
        dt_full_table
    })

    dt_match_explain <- reactive({
        req(analysis_ready())
        explain_fund_matches(fund_scheme_map(), scheme_lookup_all)
    })

    dt_quality <- reactive({
        req(analysis_ready())
        build_quality_diagnostics(
            dt_base_txns(),
            fund_scheme_map(),
            fund_category_map(),
            nav_status_log(),
            period_warnings()
        )
    })

    dt_contributors <- reactive({
        req(analysis_ready())
        get_performance_contributors(dt_analytics_leaves())
    })

    dt_category_alloc <- reactive({
        req(analysis_ready())
        get_allocation_summary(dt_analytics_leaves(), 'Category')
    })

    dt_amc_alloc <- reactive({
        req(analysis_ready())
        get_allocation_summary(dt_analytics_leaves(), 'AMC')
    })

    dt_top_funds_value <- reactive({
        req(analysis_ready())
        get_allocation_summary(dt_analytics_leaves(), 'Scheme', top_n = 10)
    })

    dt_fund_detail <- reactive({
        req(analysis_ready(), input$fund_detail)
        dt_base_txns()[fund == input$fund_detail]
    })

    dt_gains_table <- reactive({
        req(analysis_ready())
        dt_base    <- dt_base_txns()
        start_d    <- input$date_range[1]
        end_d      <- input$date_range[2]
        cas_close  <- max(dt_base[description == 'Cur Value']$date)
        first_txn  <- min(dt_base[description != 'Cur Value']$date)
        all_warns  <- character(0)

        # Investment / Redemption within the selected period (from filtered transactions)
        period_txns <- external_cashflows(dt_filtered_txns())
        investment  <- sum(period_txns[amt > 0]$amt)
        redemption  <- -sum(period_txns[amt < 0]$amt)
        net_inv     <- investment - redemption

        # Start Value — always 0 when start_d covers the full history
        if (start_d <= first_txn) {
            start_val <- 0
        } else {
            res       <- portfolio_value_at(dt_base, start_d, fund_scheme_map())
            start_val <- res$value
            all_warns <- c(all_warns, res$warnings)
        }

        # End Value — use CAS closing data when end_d reaches the statement date
        if (end_d >= cas_close) {
            end_val <- sum(-dt_base[description == 'Cur Value']$amt)
        } else {
            res       <- portfolio_value_at(dt_base, end_d + 1, fund_scheme_map())
            end_val   <- res$value
            all_warns <- c(all_warns, res$warnings)
        }

        # Also surface funds with no NAV scheme match — they are silently
        # excluded from start/end valuations, which affects period XIRR and gains.
        fsm <- fund_scheme_map()
        unmatched <- names(Filter(is.na, fsm))
        if (length(unmatched) > 0) {
            all_warns <- c(all_warns,
                paste0("No NAV match (excluded from valuation): ",
                       sapply(unmatched, extract_fund_name)))
        }
        period_warnings(all_warns)

        data.frame(
            Metric = c("Start Value", "Investment during period", "Redemption during period",
                       "Net Investment", "End Value", "Total Gains"),
            Amount = c(start_val, investment, redemption, net_inv, end_val,
                       end_val - start_val - net_inv)
        )
    })

    dt_bm_table <- reactive({
        start_d   <- input$date_range[1]
        end_d     <- input$date_range[2]
        dt_base   <- dt_base_txns()
        first_txn <- min(dt_base[description != 'Cur Value']$date)

        # All non-CurValue transactions (needed to compute pre-period holdings)
        dt_all_inv    <- external_cashflows(dt_base)[, .(date, amt)]
        # Only transactions inside the analysis period
        dt_period_inv <- external_cashflows(dt_filtered_txns())[, .(date, amt)]

        list_benchmarks <- list()
        for (mf_name in input$mf_name) {
            dt_navs <- mnav(mf_name)

            # ── Benchmark start value ─────────────────────────────────────────
            # Mirrors portfolio dt_period_xirr: if period starts from the very
            # beginning there is no prior position; otherwise accumulate units
            # from all pre-period transactions and value them at start_d NAV.
            if (start_d <= first_txn) {
                bm_start_val   <- 0
                bm_start_units <- 0
            } else {
                pre_bm         <- merge(dt_all_inv[date < start_d], dt_navs, by = 'date')
                bm_start_units <- sum(pre_bm$units <- pre_bm$amt / pre_bm$nav)
                nav_at_start   <- dt_navs[date <= start_d]
                if (nrow(nav_at_start) == 0) next
                bm_start_val   <- bm_start_units * nav_at_start[.N]$nav
            }

            # ── Period transactions in benchmark units ────────────────────────
            dt_period_bm  <- merge(dt_period_inv, dt_navs, by = 'date')
            dt_period_bm[, units := amt / nav]
            period_units  <- sum(dt_period_bm$units)

            # ── Benchmark end value ───────────────────────────────────────────
            total_units <- bm_start_units + period_units
            nav_at_end  <- dt_navs[date <= end_d]
            if (nrow(nav_at_end) == 0) next
            bm_end_val  <- total_units * nav_at_end[.N]$nav
            bm_end_date <- nav_at_end[.N]$date

            # ── XIRR cash flows — identical structure to dt_period_xirr ──────
            #   +bm_start_val at start_d   (cost of entering existing position)
            #   period transactions        (investments / redemptions)
            #   -bm_end_val   at bm_end_date  (proceeds on exit)
            rows <- list()
            if (bm_start_val > 0)
                rows <- c(rows, list(data.table(date = start_d, amt = bm_start_val)))
            if (nrow(dt_period_bm) > 0)
                rows <- c(rows, list(dt_period_bm[, .(date, amt)]))
            rows <- c(rows, list(data.table(date = bm_end_date, amt = -bm_end_val)))

            dt_bm_xirr <- rbindlist(rows, fill = TRUE)
            if (!any(dt_bm_xirr$amt > 0) || !any(dt_bm_xirr$amt < 0)) {
                bm_xirr <- NA_real_
            } else {
                dt_bm_xirr[, days  := as.numeric(max(dt_bm_xirr$date) - date)]
                dt_bm_xirr[, years := days / 365.25]
                bm_xirr <- XIRR(dt_bm_xirr)
            }

            invested <- sum(dt_period_inv[amt > 0]$amt)
            redeemed <- -sum(dt_period_inv[amt < 0]$amt)

            list_benchmarks[[mf_name]] <- data.table(
                Benchmark        = mf_name,
                StartDate        = start_d,
                EndDate          = end_d,
                `BM.StartValue`  = round(bm_start_val, 2),
                Invested         = round(invested, 2),
                Redeemed         = round(redeemed, 2),
                `BM.EndValue`    = round(bm_end_val, 2),
                `BM.Gains`       = round(bm_end_val - bm_start_val - invested + redeemed, 2),
                `BenchmarkXIRR%` = round(bm_xirr * 100, 3)
            )
        }
        rbindlist(list_benchmarks)
    })

    dt_port_xirr <- eventReactive(input$btn_proc, {
        dt_base <- dt_base_txns()
        dt_xirr <- rbindlist(list(
            external_cashflows(dt_base)[, .(date, amt)],
            dt_base[description == 'Cur Value', .(date, amt)]
        ), fill = TRUE)
        XIRR(recalc_xirr_basis(dt_xirr))
    })

    dt_period_xirr <- reactive({
        req(analysis_ready())
        dt_base   <- dt_base_txns()
        start_d   <- input$date_range[1]
        end_d     <- input$date_range[2]
        first_txn <- min(dt_base[description != 'Cur Value']$date)
        cas_close <- max(dt_base[description == 'Cur Value']$date)

        # Start Value: 0 if period covers the full history, else fetch via NAV
        if (start_d <= first_txn) {
            start_val <- 0
        } else {
            start_val <- portfolio_value_at(dt_base, start_d, fund_scheme_map())$value
        }

        # Actual investments/redemptions within the period
        period_txns <- external_cashflows(dt_filtered_txns())[, .(date, amt)]

        # Build synthetic cash-flow table for XIRR:
        #   +start_val at start_d  (cost of "acquiring" the existing portfolio)
        #   actual period transactions
        #   -end value (proceeds from "liquidating" the portfolio)
        rows <- list()
        if (start_val > 0) rows <- c(rows, list(data.table(date = start_d, amt =  start_val)))
        if (nrow(period_txns) > 0) rows <- c(rows, list(period_txns))

        # When the period reaches the statement date, reuse each fund's own
        # Cur Value row (same date/amount as Overall XIRR) instead of lumping
        # everything onto end_d — keeps the two XIRRs identical for the
        # default full-history period.
        if (end_d >= cas_close) {
            rows <- c(rows, list(dt_base[description == 'Cur Value', .(date, amt)]))
        } else {
            end_val <- portfolio_value_at(dt_base, end_d + 1, fund_scheme_map())$value
            rows <- c(rows, list(data.table(date = end_d, amt = -end_val)))
        }

        dt_xirr <- rbindlist(rows, fill = TRUE)
        # Need at least one positive and one negative cash flow for XIRR to work
        if (!any(dt_xirr$amt > 0) || !any(dt_xirr$amt < 0)) return(NA_real_)

        dt_xirr[, days  := as.numeric(max(dt_xirr$date) - date)]
        dt_xirr[, years := days / 365.25]
        XIRR(dt_xirr)
    })

    dt_port_txns <- reactive({
        req(input$btn_proc > 0)
        dt_all_txns <- dt_filtered_txns()
        dt_all_txns <- dt_all_txns[, c('amc', 'fund', 'advisor', 'folio', 'pan', 'date', 'description', 'amt', 'nav', 'units',  'bal_units')]
        names(dt_all_txns) <-  c('AMC', 'Fund', 'Advisor', 'Folio', 'PAN', 'Date', 'Description', 'Amount', 'NAV', 'TransactionUnits', 'BalanceUnits')
        factor_cols <- c('AMC', 'Fund', 'Advisor', 'Folio', 'PAN')
        for (col in factor_cols){
            dt_all_txns[, (col) := as.factor(get(col))]
        }
        dt_all_txns
    })

    output$workflow_status <- renderUI({
        if (isTRUE(analysis_ready())) {
            return(div(class = 'workflow-status done', 'Analysis ready'))
        }
        if (is.null(input$file1)) {
            return(div(class = 'workflow-status idle', 'Waiting for CAS PDF'))
        }
        if (input$btn_proc > 0) {
            return(div(class = 'workflow-status ready', 'Processing analysis...'))
        }
        div(class = 'workflow-status ready', 'File selected. Click Analyze.')
    })

    output$fund_detail_kpis <- renderUI({
        dt <- dt_fund_detail()
        req(nrow(dt) > 0)
        summary <- get_mf_summary(recalc_xirr_basis(copy(dt)), folio_ord_num = 1)
        div(
            class = 'detail-grid',
            div(class = 'kpi-card',
                div(class = 'kpi-label', 'Current value'),
                div(class = 'kpi-value', money_label(summary$Cur.Value))),
            div(class = 'kpi-card',
                div(class = 'kpi-label', 'Invested'),
                div(class = 'kpi-value', money_label(summary$Invested))),
            div(class = 'kpi-card',
                div(class = 'kpi-label', 'Unrealized gains'),
                div(class = 'kpi-value', money_label(summary$UnrealizedGains))),
            div(class = 'kpi-card',
                div(class = 'kpi-label', 'XIRR'),
                div(class = 'kpi-value', pct_label(summary$XIRR)))
        )
    })

    output$fund_match_detail <- DT::renderDataTable({
        req(input$fund_detail)
        dt <- dt_match_explain()[Fund == extract_fund_name(input$fund_detail)]
        datatable(dt, rownames = FALSE, class = dt_class,
                  options = dt_options(dom = 't', paging = FALSE, scrollX = TRUE)) %>%
            format_pct_cols(columns = 'Confidence', digits = 3)
    })

    output$fund_detail_transactions <- DT::renderDataTable({
        dt <- copy(dt_fund_detail())
        req(nrow(dt) > 0)
        dt <- dt[, .(Date = date, Description = description, Amount = amt,
                     NAV = nav, TransactionUnits = units,
                     BalanceUnits = bal_units)]
        datatable(dt, rownames = FALSE, filter = 'top', class = dt_class,
                  options = dt_options(pageLength = input$settings_page_size %||% 25)) %>%
            format_money_cols(columns = 'Amount', digits = 2) %>%
            format_number_cols(columns = c('NAV', 'TransactionUnits', 'BalanceUnits'),
                               digits = 3)
    })

    output$top_contributors <- renderPlotly({
        dt <- copy(dt_contributors()$top)
        percent <- identical(input$insights_display_mode, 'percent')
        y_col <- if (percent) 'Gain Share' else 'Gains'
        plot_bar(dt, 'Scheme', y_col, color = '#2f7d57',
                 sort_order = input$insights_sort_order %||% 'desc',
                 percent = percent)
    })

    output$bottom_contributors <- renderPlotly({
        dt <- copy(dt_contributors()$bottom)
        percent <- identical(input$insights_display_mode, 'percent')
        y_col <- if (percent) 'Gain Share' else 'Gains'
        plot_bar(dt, 'Scheme', y_col, color = '#b3261e',
                 sort_order = input$insights_sort_order %||% 'desc',
                 percent = percent)
    })

    output$category_allocation <- renderPlotly({
        dt <- dt_category_alloc()
        percent <- identical(input$insights_display_mode, 'percent')
        y_col <- if (percent) 'Weight' else 'Cur Value'
        plot_bar(dt, 'Group', y_col, color = '#1f5f8b',
                 sort_order = input$insights_sort_order %||% 'desc',
                 percent = percent)
    })

    output$amc_allocation <- renderPlotly({
        dt <- dt_amc_alloc()
        percent <- identical(input$insights_display_mode, 'percent')
        y_col <- if (percent) 'Weight' else 'Cur Value'
        plot_bar(dt, 'Group', y_col, color = '#546a7b',
                 sort_order = input$insights_sort_order %||% 'desc',
                 percent = percent)
    })

    output$top_funds_value <- renderPlotly({
        dt <- copy(dt_top_funds_value())
        percent <- identical(input$insights_display_mode, 'percent')
        y_col <- if (percent) 'Weight' else 'Cur Value'
        plot_bar(dt, 'Group', y_col, color = '#1f5f8b',
                 sort_order = input$insights_sort_order %||% 'desc',
                 percent = percent)
    })

    output$quality_diagnostics <- DT::renderDataTable({
        datatable(dt_quality(), rownames = FALSE, class = dt_class,
                  options = dt_options(dom = 't', paging = FALSE, scrollX = FALSE)) %>%
            formatStyle('Severity',
                color = styleEqual(c('OK', 'Info', 'Medium', 'High'),
                                   c('#067647', '#175cd3', '#b54708', '#b42318')),
                fontWeight = 'bold'
            )
    })

    output$match_explainability <- DT::renderDataTable({
        datatable(dt_match_explain(), rownames = FALSE, filter = 'top',
                  class = dt_class,
                  options = dt_options(pageLength = input$settings_page_size %||% 25)) %>%
            format_pct_cols(columns = 'Confidence', digits = 3)
    })

    output$gains <- DT::renderDataTable(
        datatable(dt_gains_table(), rownames = FALSE, class = dt_class,
                  options = dt_options(dom = 't', ordering = FALSE,
                                       paging = FALSE, scrollX = FALSE)) %>%
            format_money_cols(columns = c('Amount'), digits = 2)
    )

    output$period_warnings <- renderUI({
        w <- period_warnings()
        if (length(w) == 0) return(NULL)
        div(class = 'warning-box', paste("Warning:", paste(w, collapse = "\n")))
    })

    output$benchmark <- DT::renderDataTable({
        dt <- dt_bm_table()
        req(nrow(dt) > 0)
        datatable(dt, rownames = FALSE, class = dt_class,
                  options = dt_options(dom = 't', ordering = FALSE,
                                       paging = FALSE)) %>%
            format_money_cols(columns = c('BM.StartValue', 'Invested', 'Redeemed',
                                          'BM.EndValue', 'BM.Gains'),
                              digits = 2) %>%
            format_pct_cols(columns = 'BenchmarkXIRR%', digits = 3)
    })

    output$summary <- DT::renderDataTable(
        datatable(dt_mf_xirrs(), filter = 'top', class = dt_class,
                  options = dt_options(pageLength = input$settings_page_size %||% 25)) %>%
            format_money_cols(columns = c('Cur.Value', 'Invested', 'Redeemed',
                                          'RealizedGains', 'UnrealizedGains'),
                              digits = 2) %>%
            format_pct_cols(columns = 'XIRR%', digits = 3)
    )

    output$folio_level_summary <- DT::renderDataTable(
        datatable(dt_folio_xirrs(), filter = 'top', class = dt_class,
                  options = dt_options(pageLength = 10)) %>%
            format_money_cols(columns = c('Cur.Value', 'Invested', 'Redeemed',
                                          'RealizedGains', 'UnrealizedGains'),
                              digits = 2) %>%
            format_pct_cols(columns = 'XIRR%', digits = 3)
    )

    output$transactions <- DT::renderDataTable(
        datatable(dt_port_txns(), filter = 'top',
                  extensions = 'Buttons',
                  options = dt_options(
                      paging = TRUE,
                      searching = TRUE,
                      ordering = TRUE,
                      dom = 'Bfrtip',
                      buttons = c('copy', 'csv', 'excel'),
                      pageLength = input$settings_page_size %||% 100
                  ),
                  class = dt_class) %>%
            format_money_cols(columns = c('Amount'), digits = 2) %>%
            format_number_cols(columns = c('NAV', 'TransactionUnits', 'BalanceUnits'),
                               digits = 3)
    )

    output$analytics_table <- renderReactable({
        req(analysis_ready())
        df <- as.data.frame(dt_analytics_hierarchy())

        # ── Column definitions ────────────────────────────────────────────────
        # Header colour ramp for hierarchy depth (dark → lighter blue)
        hier_blues <- c('#1565C0', '#1976D2', '#1E88E5', '#42A5F5', '#90CAF9')
        text_white <- list(color = 'white', fontWeight = 'bold')
        all_5      <- c('AMC', 'Category', 'SubCategory', 'Scheme', 'Folio')
        hidden_cols <- intersect(unique(c('Level', 'Path', all_5)), names(df))

        data_hdr  <- list(background = '#37474F', color = 'white', fontWeight = 'bold')
        money_fmt <- colFormat(separators = TRUE, digits = 2, currency = NULL)

        hidden_defs <- setNames(lapply(hidden_cols, function(col) colDef(show = FALSE)), hidden_cols)

        data_defs <- list(
            Group         = colDef(name = 'Hierarchy', minWidth = 300,
                                   headerStyle = c(list(background = hier_blues[1]), text_white),
                                   style = function(value, index) {
                                       if (!is.null(df$Level) && df$Level[index] <= 2) {
                                           list(fontWeight = '700')
                                       }
                                   }),
            `Cur Value`   = colDef(name = 'Cur. Value',
                                   format = money_fmt,      headerStyle = data_hdr),
            Invested      = colDef(format = money_fmt,
                                   headerStyle = data_hdr),
            Redeemed      = colDef(format = money_fmt,
                                   headerStyle = data_hdr),
            `Net Invested`= colDef(name = 'Net Invested',
                                   format = money_fmt,      headerStyle = data_hdr),
            Gains         = colDef(format = money_fmt,
                                   headerStyle = data_hdr,
                                   style = function(value) {
                                       if (!is.na(value) && is.numeric(value))
                                           list(color = if (value >= 0) '#2e7d32' else '#c62828')
                                   }),
            `XIRR%`       = colDef(name = 'XIRR%',
                                   format = colFormat(digits = 3),
                                   headerStyle = data_hdr,
                                   style = function(value) {
                                       if (!is.na(value) && is.numeric(value))
                                           list(color      = if (value >= 0) '#2e7d32' else '#c62828',
                                                fontWeight = 'bold')
                                   })
        )

        reactable(df,
            columns         = c(hidden_defs, data_defs),
            bordered        = TRUE,
            highlight       = TRUE,
            compact         = TRUE,
            searchable      = TRUE,
            defaultPageSize = input$settings_page_size %||% 50,
            theme           = reactableTheme(
                headerStyle      = list(background = '#37474F', color = 'white'),
                rowSelectedStyle = list(background = '#e3f2fd'),
                borderColor      = '#dee2e6'
            )
        )
    })

    output$pf_xirr <- renderText({
        val <- dt_port_xirr()
        if (is.na(val)) return("N/A")
        paste0(round(val * 100, 3), "%")
    })

    output$period_xirr <- renderText({
        req(input$btn_proc > 0)
        val <- dt_period_xirr()
        if (is.na(val)) return("N/A")
        paste0(round(val * 100, 3), "%")
    })

    output$text_ovr_sum <- renderText({
        ifelse(input$btn_proc, 'Overall Summary', '')
    })

    output$text_fol_sum <- renderText({
        ifelse(input$btn_proc, 'Fund Level Summary', '')
    })

    output$portfolio_curve <- renderPlotly({
        dt <- dt_portfolio_curve()
        req(nrow(dt) > 0)

        plot_ly(dt, x = ~date) %>%
            add_trace(
                y            = ~net_invested,
                name         = 'Amount Invested',
                type         = 'scatter',
                mode         = 'none',
                fill         = 'tozeroy',
                fillcolor    = 'rgba(31, 119, 180, 0.55)',
                line         = list(color = 'rgba(31, 119, 180, 1)'),
                hovertemplate = 'Invested: \u20b9%{y:,.0f}<extra></extra>'
            ) %>%
            add_trace(
                y            = ~portfolio_value,
                name         = 'Portfolio Value',
                type         = 'scatter',
                mode         = 'lines',
                fill         = 'tonexty',
                fillcolor    = 'rgba(44, 160, 44, 0.45)',
                line         = list(color = 'rgba(44, 160, 44, 1)', width = 2),
                hovertemplate = 'Portfolio: \u20b9%{y:,.0f}<extra></extra>'
            ) %>%
            layout(
                title     = 'Portfolio Growth Over Time',
                xaxis     = list(title = ''),
                yaxis     = list(title = 'Value (\u20b9)', tickformat = ',.0f'),
                hovermode = 'x unified',
                legend    = list(orientation = 'h', x = 0, y = -0.12),
                margin    = list(t = 60, b = 60)
            )
    })

    output$xirr_over_time <- renderPlotly({
        dt <- dt_xirr_curve()
        dt <- dt[!is.na(xirr)]
        req(nrow(dt) > 0)

        # Colour the line: green above 0, red below (via a zero-baseline area)
        plot_ly(dt, x = ~date) %>%
            add_trace(
                y             = ~pmax(xirr, 0),
                name          = 'XIRR (positive)',
                type          = 'scatter',
                mode          = 'none',
                fill          = 'tozeroy',
                fillcolor     = 'rgba(44, 160, 44, 0.35)',
                showlegend    = FALSE,
                hoverinfo     = 'skip'
            ) %>%
            add_trace(
                y             = ~pmin(xirr, 0),
                name          = 'XIRR (negative)',
                type          = 'scatter',
                mode          = 'none',
                fill          = 'tozeroy',
                fillcolor     = 'rgba(214, 39, 40, 0.35)',
                showlegend    = FALSE,
                hoverinfo     = 'skip'
            ) %>%
            add_trace(
                y             = ~xirr,
                name          = 'XIRR',
                type          = 'scatter',
                mode          = 'lines',
                line          = list(color = 'rgba(31, 119, 180, 1)', width = 2),
                hovertemplate = 'XIRR: %{y:.2f}%<extra></extra>'
            ) %>%
            layout(
                title     = list(
                    text = paste0(
                        'Portfolio XIRR Over Time (Inception to Date)',
                        '<br><sup style="color:#888;font-size:11px">',
                        'Inception-to-date XIRR at each point. Extreme values in early periods are',
                        ' mathematically expected when large new SIP investments coincide with',
                        ' market corrections \u2014 the XIRR is dominated by recent large cash flows.',
                        '</sup>'
                    )
                ),
                xaxis     = list(title = ''),
                yaxis     = list(title = 'XIRR (%)', tickformat = '.1f',
                                 zeroline = TRUE, zerolinecolor = '#888',
                                 zerolinewidth = 1),
                hovermode = 'x unified',
                legend    = list(orientation = 'h', x = 0, y = -0.12),
                margin    = list(t = 80, b = 60)
            )
    })

    output$nav_status <- DT::renderDataTable({
        dt <- nav_status_log()
        datatable(dt, rownames = FALSE, class = dt_class,
                  options = dt_options(pageLength = input$settings_page_size %||% 50, dom = 't',
                                       ordering = FALSE)) %>%
            formatStyle('Source',
                backgroundColor = styleEqual(
                    c('Cache', 'API - new', 'API - refreshed',
                      'No match', 'Fetch failed', 'Cache (stale, fetch failed)'),
                    c('#d4edda', '#cce5ff', '#cce5ff',
                      '#f8d7da', '#f8d7da', '#fff3cd')
                )
            )
    })

    output$download_report <- downloadHandler(
        filename = function() {
            paste0('cas-portfolio-report-', Sys.Date(), '.html')
        },
        content = function(file) {
            req(analysis_ready())
            warnings <- period_warnings()
            css <- paste0(
                'body{font-family:Inter,Arial,sans-serif;color:#172033;margin:32px;}',
                'h1,h2{color:#172033;}',
                'table{border-collapse:collapse;width:100%;margin:12px 0 28px;}',
                'th,td{border:1px solid #d0d5dd;padding:6px 8px;font-size:12px;}',
                'th{background:#f2f4f7;text-align:left;}',
                '.warn{background:#fffaeb;border:1px solid #fedf89;padding:12px;border-radius:8px;white-space:pre-wrap;}'
            )
            html <- paste0(
                '<!doctype html><html><head><meta charset="utf-8"><title>CAS Portfolio Report</title>',
                '<style>', css, '</style></head><body>',
                '<h1>CAS Portfolio Report</h1>',
                '<p>Generated ', htmltools::htmlEscape(as.character(Sys.time())), '</p>',
                '<h2>Summary</h2>', html_table(dt_gains_table()),
                '<h2>Top funds by current value</h2>',
                html_table(head(dt_mf_xirrs()[order(-Cur.Value)], 20)),
                '<h2>Top gain contributors</h2>',
                html_table(dt_contributors()$top),
                '<h2>Data quality diagnostics</h2>',
                html_table(dt_quality()),
                '<h2>Scheme match explainability</h2>',
                html_table(dt_match_explain()),
                if (length(warnings) > 0) {
                    paste0('<h2>Warnings</h2><div class="warn">',
                           htmltools::htmlEscape(paste(warnings, collapse = '\n')),
                           '</div>')
                } else {
                    '<h2>Warnings</h2><p>No selected-period warnings.</p>'
                },
                '</body></html>'
            )
            writeLines(html, file, useBytes = TRUE)
        }
    )

}
