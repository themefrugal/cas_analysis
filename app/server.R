source('cas_reader.R')
library(DT)
library(shiny)
library(memoise)
library(purrr)

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
    load('./mf_codes.RData')
}

get_scheme_code <- function(mf_name){
    # Check this: There are multiple scheme codes for the same scheme name (in approx 20 instances)
    # As of now, we are taking the occurrence of first such instance
    scheme_code <- dt_mfs[schemeName == mf_name]$schemeCode[1]
    return (scheme_code)
}
mnav <- memoise(compose(get_navs, get_scheme_code))

function(input, output, session) {
    updateSelectizeInput(session, "mf_name", choices = unique(dt_mfs$schemeName), server=TRUE)
    init_proc <- reactive({
        pages <- pdf_text(input$file1$datapath, upw=input$password)
        all_lines <<- c()
        for (i in 1:length(pages)){
            lines <- str_split(pages[i], pattern="\n")
            all_lines <<- c(all_lines, lines[[1]])
        }

        folio_lines <<- which(grepl("Folio No:", all_lines, ignore.case=TRUE))
        amc_lines <<- which(grepl("Mutual Fund", all_lines, ignore.case=TRUE))
        opening_lines <<- which(grepl("Opening Unit Balance:", all_lines, ignore.case=TRUE))
        closing_lines <<- which(grepl("Closing Unit Balance:", all_lines, ignore.case=TRUE))
    })

    dt_mf_xirrs <- eventReactive(input$btn_proc, {
        init_proc()
        dt_full_table <- rbindlist(lapply(c(1:length(folio_lines)), get_mf_table))
        names(dt_full_table)[names(dt_full_table) == 'XIRR'] <- 'XIRR%'
        dt_full_table
    })

    dt_folio_xirrs <- eventReactive(input$btn_proc, {
        init_proc()
        dt_all_txns <- get_portfolio_transactions(folio_lines)
        folio_ids <- unique(dt_all_txns$folio)
        list_table <- list()
        for (folio_id in folio_ids){
            list_table <- c(list_table, list(get_mf_table_for_txns(dt_all_txns, folio_id)))
        }
        dt_full_table <- rbindlist(list_table)
        names(dt_full_table)[names(dt_full_table) == 'XIRR'] <- 'XIRR%'
        dt_full_table
    })

    dt_gains_table <- eventReactive(input$btn_proc, {
        dt_full_table <- dt_mf_xirrs()
        dt_gains <- data.table(TotalInvestment = sum(dt_full_table$Invested),
            TotalRedemption = sum(dt_full_table$Redeemed),
            NetInvestment = sum(dt_full_table$Invested) - sum(dt_full_table$Redeemed),
            CurrentValue = sum(dt_full_table$Cur.Value),
            TotalGains = sum(dt_full_table$Cur.Value) - (sum(dt_full_table$Invested) - sum(dt_full_table$Redeemed)))
        df_gains_t <- data.frame(t(dt_gains))
        names(df_gains_t) <- 'Amount'
        row.names(df_gains_t) <- names(dt_gains)
        df_gains_t
    })

    dt_bm_table <- reactive({
        # HDFC Nify 50 Fund Regular Growth
        # scheme_code <- '101525'
        list_benchmarks <- list()
        for (mf_name in input$mf_name){
            # scheme_code <- get_scheme_code(mf_name)
            # dt_navs <- get_navs(scheme_code)
            dt_navs <- mnav(mf_name)
            dt_all_txns <- get_portfolio_transactions(folio_lines)

            # To do: Instead of comparing with a single fund, give option to compare against any fund
            # selected
            dt_inv_txns <- dt_all_txns[description != 'Cur Value'][, c('date', 'description', 'amt')]
            dt_bm_txns <- merge(dt_inv_txns, dt_navs, by='date')
            dt_bm_txns[, units := amt/nav]
            cur_date <- dt_all_txns[description == 'Cur Value']$date[1]
            cur_nav <- dt_navs[date == cur_date]$nav
            total_units <- -sum(dt_bm_txns$units)
            cur_value <- total_units * cur_nav
            dt_bm_final <- rbindlist(list(dt_bm_txns, list(cur_date, 'BM Cur Value', cur_value, cur_nav, total_units)))
            dt_bm_final[, days :=  as.numeric(max(dt_bm_final$date) - date)]
            dt_bm_final[, years := days/365.25]
            bm_xirr <- XIRR(dt_bm_final)

            dt_benchmark <- data.table(
                Benchmark = mf_name,
                BenchmarkEqv = round(-cur_value, 3),
                BenchmarkXIRR = round(bm_xirr * 100, 3))
            #df_benchmark_t <- data.frame(t(dt_benchmark))
            #names(df_benchmark_t) <- 'Value'
            #row.names(df_benchmark_t) <- names(dt_benchmark)
            #df_benchmark_t
            list_benchmarks[[mf_name]] <- dt_benchmark
        }
        rbindlist(list_benchmarks)
    })

    dt_port_xirr <- eventReactive(input$btn_proc, {
        dt_all_txns <- get_portfolio_transactions(folio_lines)
        xirr_all <- XIRR(dt_all_txns)
        xirr_all
    })

    dt_port_txns <- eventReactive(input$btn_proc, {
        init_proc()
        dt_all_txns <- get_portfolio_transactions(folio_lines)
        dt_all_txns <- dt_all_txns[, c('amc', 'fund', 'advisor', 'folio', 'pan', 'date', 'description', 'amt', 'nav', 'units',  'bal_units')]
        # dt_all_txns$amc <- as.factor(dt_all_txns$amc)
        names(dt_all_txns) <-  c('AMC', 'Fund', 'Advisor', 'Folio', 'PAN', 'Date', 'Description', 'Amount', 'NAV', 'TransactionUnits', 'BalanceUnits')
        factor_cols <- c('AMC', 'Fund', 'Advisor', 'Folio', 'PAN')
        for (col in factor_cols){
            dt_all_txns[, (col) := as.factor(get(col))]
        }
        dt_all_txns
    })

    output$gains <- DT::renderDataTable(
        datatable(dt_gains_table()) %>% formatRound(columns=c('Amount'), digits=3)
    )

    output$benchmark <- DT::renderDataTable(
        datatable(dt_bm_table())
    )

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
        paste0("Overall Portfolio XIRR: ", round(dt_port_xirr() * 100, 3), "%")
    })

    output$text_ovr_sum <- renderText({
        ifelse(input$btn_proc, 'Overall Summary', '')
    })

    output$text_fol_sum <- renderText({
        ifelse(input$btn_proc, 'Folio Level Summary', '')
    })

}