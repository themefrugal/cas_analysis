library(shiny)
navbarPage(
    title = 'Mutual Fund Analysis and Report',
    tabPanel('MF Summary',
        fluidPage(
            sidebarPanel(
                fileInput("file1", "Upload the CAS PDF File"),
                passwordInput("password", "Password for the PDF:"),
                # textOutput('out_text'),
                actionButton("btn_proc", "Go!")
            ),
            mainPanel(
                h3(textOutput('pf_xirr')),
                br(), br(),
                h3(textOutput('text_ovr_sum')),
                DT::dataTableOutput('gains'),
                br(), br(),
                h3(textOutput('text_fol_sum')),
                DT::dataTableOutput('summary'),
                h3('Folio Level Summary'),
                DT::dataTableOutput('folio_level_summary')
            )
        )
    ),
    tabPanel('Transactions',
        fluidPage(
            DT::dataTableOutput('transactions')
        )
    )

)