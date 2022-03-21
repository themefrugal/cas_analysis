source('../src/cas_reader.R')
library(DT)

function(input, output) {
    init_proc <- reactive({
        pages <- pdf_text(input$file1$datapath, upw=input$password)
        all_lines <<- c()
        for (i in 1:length(pages)){
            lines <- str_split(pages[i], pattern="\n")
            all_lines <<- c(all_lines, lines[[1]])
        }

        folio_lines <<- which(grepl("Folio No:", all_lines, fixed = TRUE))
        amc_lines <<- which(grepl("Mutual Fund", all_lines, fixed = TRUE))
        opening_lines <<- which(grepl("Opening Unit Balance:", all_lines, fixed = TRUE))
        closing_lines <<- which(grepl("Closing Unit Balance:", all_lines, fixed = TRUE))
    })
    dt_mf_xirrs <- eventReactive(input$btn_proc, {
        init_proc()
        dt_full_table <- rbindlist(lapply(c(1:length(folio_lines)), get_mf_table))
        dt_full_table
    })
    dt_port_txns <- eventReactive(input$btn_proc, {
        init_proc()
        dt_all_txns <- get_portfolio_transactions(folio_lines)
        dt_all_txns <- dt_all_txns[, c('amc', 'fund', 'advisor', 'folio', 'pan', 'date', 'description', 'amt', 'nav', 'units',  'bal_units')]
        names(dt_all_txns) <-  c('AMC', 'Fund', 'Advisor', 'Folio', 'PAN', 'Date', 'Description', 'Amount', 'NAV', 'TransactionUnits', 'BalanceUnits')
        dt_all_txns
    })
    output$summary <- DT::renderDataTable(
        datatable(dt_mf_xirrs(), filter='top', options = list(pageLength = 25)) %>%
            formatRound(columns=c('Cur.Value', 'Invested', 'Redeemed',
                'RealizedGains', 'UnrealizedGains', 'XIRR'), digits=3)
    )
    output$transactions <- DT::renderDataTable(
        datatable(dt_port_txns(), filter='top', options = list(pageLength = 25)) %>%
            formatRound(columns=c('Amount', 'NAV', 'TransactionUnits', 'BalanceUnits'), digits=3)
    )
    output$out_text <- renderText({
        input$password
    })
}