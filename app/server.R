source('cas_reader.R')
library(DT)
library(shiny)

function(input, output) {
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

    dt_gains_table <- eventReactive(input$btn_proc, {
        dt_full_table <- dt_mf_xirrs()
        dt_gains <- data.table(TotalInvestment = sum(dt_full_table$Invested),
            TotalRedemption = sum(dt_full_table$Redeemed),
            NetInvestment = sum(dt_full_table$Invested) - sum(dt_full_table$Redeemed),
            CurrentValue = sum(dt_full_table$Cur.Value),
            TotalGains = sum(dt_full_table$Cur.Value) - (sum(dt_full_table$Invested) - sum(dt_full_table$Redeemed)))
        dt_gains_t <- transpose(dt_gains)
        names(dt_gains_t) <- 'Amount'
        row.names(dt_gains_t) <- names(dt_gains)
        dt_gains_t
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

    output$summary <- DT::renderDataTable(
        datatable(dt_mf_xirrs(), filter='top', options = list(pageLength = 25)) %>%
            formatRound(columns=c('Cur.Value', 'Invested', 'Redeemed',
                'RealizedGains', 'UnrealizedGains', 'XIRR%'), digits=3)
    )

    output$transactions <- DT::renderDataTable(
        datatable(dt_port_txns(), filter='top', options = list(pageLength = 25)) %>%
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