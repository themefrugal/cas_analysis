library(shiny)
fluidPage(
    sidebarPanel(
        fileInput("file1", "Upload the CAS PDF File"),
        passwordInput("password", "Password for the PDF:"),
        textOutput('out_text')
    ),
    mainPanel(
        navbarPage(
            title = 'Mutual Fund Analysis and Report',
            tabPanel('Summary', DT::dataTableOutput('summary'))
        )
    )
)