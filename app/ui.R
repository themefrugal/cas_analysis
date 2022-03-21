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
                DT::dataTableOutput('summary')
            )
        )
    ),
    tabPanel('Transactions',
        fluidPage(
            DT::dataTableOutput('transactions')
        )
    )

)