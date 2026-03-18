source('cas_reader.R')
library(DT)
library(shiny)
library(memoise)
library(purrr)
library(plotly)

read_from_internet <- FALSE
if (read_from_internet){
    mf_list_url <- 'https://api.mfapi.in/mf'
    mf_list <- fromJSON(paste(readLines(mf_list_url), collapse=""))
    dt_mfs <- data.table(do.call(rbind.data.frame, mf_list))

    dt_mfs$schemeName <- sapply(dt_mfs$schemeName, first_upper)
    dt_mfs$schemeName <- sapply(dt_mfs$schemeName, prune_left)
    dt_mfs$schemeName <- sapply(dt_mfs$schemeName, remove_extra_space)
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

get_scheme_code <- function(mf_name){
    # Check this: There are multiple scheme codes for the same scheme name (in approx 20 instances)
    # As of now, we are taking the occurrence of first such instance
    scheme_code <- dt_mfs[schemeName == mf_name]$schemeCode[1]
    return (scheme_code)
}
mnav <- memoise(compose(get_navs, get_scheme_code))

get_fund_summary_dt <- function(dt_all, fund_name) {
    dt_fund <- dt_all[fund == fund_name]

    # Recalculate days/years using THIS fund's own max date, not the
    # portfolio-wide max date inherited from dt_filtered_txns().
    if (nrow(dt_fund) > 0) {
        dt_fund[, days  := as.numeric(max(dt_fund$date) - date)]
        dt_fund[, years := days / 365.25]
    }

    cur_value <- -sum(dt_fund[description == 'Cur Value']$amt)
    cash_in   <- sum(dt_fund[amt > 0]$amt)
    cash_out  <- -sum(dt_fund[amt < 0]$amt)
    xirr_val  <- XIRR(dt_fund)
    txn_dates <- dt_fund[description != 'Cur Value']$date

    # Proportional average-cost allocation for realized/unrealized split
    redemptions <- cash_out - cur_value
    total_out   <- redemptions + cur_value
    cost_of_redemptions <- if (total_out > 0) cash_in * redemptions / total_out else 0
    realized_gains   <- redemptions - cost_of_redemptions
    unrealized_gains <- cur_value - (cash_in - cost_of_redemptions)

    # Propagate NA rather than computing NA * 100
    xirr_pct <- if (is.na(xirr_val)) NA_real_ else xirr_val * 100

    data.frame(
        Fund            = fund_name,
        Cur.Value       = cur_value,
        Invested        = cash_in,
        Redeemed        = redemptions,
        RealizedGains   = realized_gains,
        UnrealizedGains = unrealized_gains,
        XIRR            = xirr_pct,
        StartDate       = if (length(txn_dates) > 0) min(txn_dates) else NA,
        RecentDate      = max(dt_fund$date)
    )
}

function(input, output, session) {
    updateSelectizeInput(session, "mf_name", choices = unique(dt_mfs$schemeName), server=TRUE)

    init_proc <- reactive({
        pages <- pdf_text(input$file1$datapath, upw=input$password)
        all_lines <<- c()
        for (i in 1:length(pages)){
            lines <- str_split(pages[i], pattern="\n")
            all_lines <<- c(all_lines, lines[[1]])
        }

        folio_lines <<- which(grepl("Folio No\\s*:", all_lines, ignore.case=TRUE))
        amc_lines <<- which(grepl("Mutual Fund", all_lines, ignore.case=TRUE))
        opening_lines <<- which(grepl("Opening Unit Balance:", all_lines, ignore.case=TRUE))
        closing_lines <<- which(grepl("Closing Unit Balance:", all_lines, ignore.case=TRUE))
    })

    dt_base_txns <- eventReactive(input$btn_proc, {
        withProgress(message = 'Parsing CAS PDF...', value = 0.5, {
            init_proc()
            get_portfolio_transactions(folio_lines)
        })
    })

    # Built once per PDF load — pure string matching, no API calls.
    # Pre-computes normalized scheme names ONCE (6,000+ entries), then passes
    # them to match_fund_to_scheme for each fund.  Previously the normalization
    # was repeated inside match_fund_to_scheme for every fund, causing ~120,000+
    # redundant regex operations.
    fund_scheme_map <- eventReactive(input$btn_proc, {
        funds    <- unique(dt_base_txns()[description != 'Cur Value']$fund)
        norm_mfs <- sapply(as.character(dt_mfs_all$schemeName), normalize_fund_name)
        codes    <- as.character(dt_mfs_all$schemeCode)
        map      <- lapply(funds, function(f) match_fund_to_scheme(f, norm_mfs, codes))
        names(map) <- funds
        map
    })

    # Pre-warms NAV cache for all funds immediately after PDF loads.
    # Shows a progress bar while fetching from mfapi.in, then stores
    # the per-fund status (Cache / API-new / API-refreshed / No match / failed).
    nav_status_log <- eventReactive(input$btn_proc, {
        map <- fund_scheme_map()
        n   <- length(map)
        i   <- 0L
        withProgress(message = 'Loading NAV data...', value = 0, {
            pre_warm_navs(map, required_date = Sys.Date(),
                progress_fn = function(fname) {
                    i <<- i + 1L
                    setProgress(
                        value  = i / n,
                        detail = paste0('(', i, '/', n, ') ', extract_fund_name(fname))
                    )
                })
        })
    })

    # Monthly portfolio value curve — computed ONCE per PDF load (eventReactive).
    # Does NOT depend on input$date_range so updating the date picker never
    # triggers an expensive recompute. get_portfolio_curve() reads from the
    # RDS cache directly, so no coupling to nav_status_log is needed.
    dt_portfolio_curve <- eventReactive(input$btn_proc, {
        get_portfolio_curve(dt_base_txns(), fund_scheme_map())
    })

    period_warnings <- reactiveVal(character(0))

    # Gate: FALSE until input$date_range has been auto-populated with real dates.
    # Prevents expensive XIRR/gains computations from running in the first reactive
    # flush (when date_range is still the "1900-01-01" sentinel), so each PDF load
    # triggers only ONE computation instead of two.
    analysis_ready <- reactiveVal(FALSE)

    # Reset the gate every time a new PDF is processed.
    observeEvent(input$btn_proc, {
        analysis_ready(FALSE)
    }, ignoreInit = TRUE)

    # Populate the date range from the loaded data and open the analysis gate.
    # analysis_ready(TRUE) fires here — after dt_base_txns() resolves with real
    # data — so no sentinel date check is needed.
    # NAV pre-warming (nav_status_log) is NOT forced here; it runs lazily when
    # the NAV Status tab is viewed.  This keeps the UI responsive.
    observe({
        dt <- dt_base_txns()
        non_cv    <- dt[description != 'Cur Value']$date
        cas_close <- max(dt[description == 'Cur Value']$date)
        updateDateRangeInput(session, "date_range",
            start = min(non_cv), end = cas_close)
        analysis_ready(TRUE)
    })

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

    dt_mf_xirrs <- reactive({
        req(analysis_ready())
        dt <- dt_filtered_txns()
        funds <- unique(dt[description != 'Cur Value']$fund)
        dt_full <- rbindlist(lapply(funds, function(f) get_fund_summary_dt(dt, f)))
        names(dt_full)[names(dt_full) == 'XIRR'] <- 'XIRR%'
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

    dt_gains_table <- reactive({
        req(analysis_ready())
        dt_base    <- dt_base_txns()
        start_d    <- input$date_range[1]
        end_d      <- input$date_range[2]
        cas_close  <- max(dt_base[description == 'Cur Value']$date)
        first_txn  <- min(dt_base[description != 'Cur Value']$date)
        all_warns  <- character(0)

        # Investment / Redemption within the selected period (from filtered transactions)
        period_txns <- dt_filtered_txns()[description != 'Cur Value']
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
        cas_close <- max(dt_base[description == 'Cur Value']$date)

        # Transactions within the analysis period only (no Cur Value rows)
        dt_inv_txns <- dt_filtered_txns()[description != 'Cur Value',
                                          .(date, description, amt)]
        invested <- sum(dt_inv_txns[amt > 0]$amt)

        list_benchmarks <- list()
        for (mf_name in input$mf_name) {
            dt_navs <- mnav(mf_name)

            # Mirror the portfolio's period transactions into the benchmark fund
            dt_bm_txns <- merge(dt_inv_txns, dt_navs, by = 'date')
            dt_bm_txns[, units := amt / nav]
            total_units <- -sum(dt_bm_txns$units)   # negative: net units held (sign convention)

            # Benchmark value at analysis period END (not CAS close).
            # Use the last available NAV on or before end_d.
            nav_at_end <- dt_navs[date <= end_d]
            if (nrow(nav_at_end) == 0 || total_units == 0) next
            end_nav    <- nav_at_end[.N]$nav
            end_date   <- nav_at_end[.N]$date
            bm_value   <- total_units * end_nav     # negative (XIRR sign convention)

            # Synthetic closing row at period end
            dt_bm_final <- rbindlist(list(
                dt_bm_txns,
                data.table(date = end_date, description = 'BM Cur Value',
                           amt = bm_value, nav = end_nav, units = total_units)
            ))
            dt_bm_final[, days  := as.numeric(max(dt_bm_final$date) - date)]
            dt_bm_final[, years := days / 365.25]
            bm_xirr <- XIRR(dt_bm_final)

            list_benchmarks[[mf_name]] <- data.table(
                Benchmark        = mf_name,
                StartDate        = start_d,
                EndDate          = end_d,
                Invested         = round(invested, 2),
                `BM.Value`       = round(-bm_value, 2),
                `BM.Gains`       = round(-bm_value - invested, 2),
                `BenchmarkXIRR%` = round(bm_xirr * 100, 3)
            )
        }
        rbindlist(list_benchmarks)
    })

    dt_port_xirr <- eventReactive(input$btn_proc, {
        XIRR(dt_base_txns())
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

        # End Value: use CAS closing data when period reaches the statement date
        if (end_d >= cas_close) {
            end_val <- sum(-dt_base[description == 'Cur Value']$amt)
        } else {
            end_val <- portfolio_value_at(dt_base, end_d + 1, fund_scheme_map())$value
        }

        # Actual investments/redemptions within the period
        period_txns <- dt_filtered_txns()[description != 'Cur Value', .(date, amt)]

        # Build synthetic cash-flow table for XIRR:
        #   +start_val at start_d  (cost of "acquiring" the existing portfolio)
        #   actual period transactions
        #   -end_val   at end_d    (proceeds from "liquidating" the portfolio)
        rows <- list()
        if (start_val > 0) rows <- c(rows, list(data.table(date = start_d, amt =  start_val)))
        if (nrow(period_txns) > 0) rows <- c(rows, list(period_txns))
        rows <- c(rows, list(data.table(date = end_d, amt = -end_val)))

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

    output$gains <- DT::renderDataTable(
        datatable(dt_gains_table(), rownames = FALSE,
                  options = list(dom = 't', ordering = FALSE)) %>%
            formatRound(columns = c('Amount'), digits = 2)
    )

    output$period_warnings <- renderText({
        w <- period_warnings()
        if (length(w) == 0) return(NULL)
        paste("Warning:", paste(w, collapse = "\n"))
    })

    output$benchmark <- DT::renderDataTable({
        dt <- dt_bm_table()
        req(nrow(dt) > 0)
        datatable(dt, rownames = FALSE,
                  options = list(dom = 't', ordering = FALSE)) %>%
            formatRound(columns = c('Invested', 'BM.Value', 'BM.Gains', 'BenchmarkXIRR%'),
                        digits = 2)
    })

    output$summary <- DT::renderDataTable(
        datatable(dt_mf_xirrs(), filter='top', options = list(pageLength = 25)) %>%
            formatRound(columns=c('Cur.Value', 'Invested', 'Redeemed',
                'RealizedGains', 'UnrealizedGains', 'XIRR%'), digits=3)
    )

    output$folio_level_summary <- DT::renderDataTable(
        datatable(dt_folio_xirrs(), filter='top', options = list(pageLength = 10)) %>%
            formatRound(columns=c('Cur.Value', 'Invested', 'Redeemed',
                'RealizedGains', 'UnrealizedGains', 'XIRR%'), digits=3)
    )

    output$transactions <- DT::renderDataTable(
        datatable(dt_port_txns(), filter='top',
                            extensions = 'Buttons',
                            options = list(
                                paging = TRUE,
                                searching = TRUE,
                                fixedColumns = TRUE,
                                autoWidth = TRUE,
                                ordering = TRUE,
                                dom = 'tB',
                                buttons = c('copy', 'csv', 'excel'),
                                pageLength = 100
                            ),
                            class='display'
#            options = list(dom = '<"top" p>', pageLength = 25)
            ) %>%
            formatRound(columns=c('Amount', 'NAV', 'TransactionUnits', 'BalanceUnits'), digits=3)
    )

    output$out_text <- renderText({
        input$password
    })

    output$pf_xirr <- renderText({
        val <- dt_port_xirr()
        if (is.na(val)) return("Overall Portfolio XIRR: N/A")
        paste0("Overall Portfolio XIRR: ", round(val * 100, 3), "%")
    })

    output$period_xirr <- renderText({
        req(input$btn_proc > 0)
        val <- dt_period_xirr()
        if (is.na(val)) return("Analysis Period XIRR: N/A")
        paste0("Analysis Period XIRR: ", round(val * 100, 3), "%")
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

    output$nav_status <- DT::renderDataTable({
        dt <- nav_status_log()
        datatable(dt, rownames = FALSE,
                  options = list(pageLength = 50, dom = 't', ordering = FALSE)) %>%
            formatStyle('Source',
                backgroundColor = styleEqual(
                    c('Cache', 'API - new', 'API - refreshed',
                      'No match', 'Fetch failed', 'Cache (stale, fetch failed)'),
                    c('#d4edda', '#cce5ff', '#cce5ff',
                      '#f8d7da', '#f8d7da', '#fff3cd')
                )
            )
    })

}
