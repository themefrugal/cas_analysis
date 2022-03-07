navbarPage(
    title = 'Mutual Fund Analysis and Report',
    tabPanel('Summary', DT::dataTableOutput('summary')),
    tabPanel('Transactions', DT::dataTableOutput('transactions'))
)