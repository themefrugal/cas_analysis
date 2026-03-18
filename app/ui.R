# To do:
# 1. comparative chart with any selected mutual fund or index.encrypted
# 2. Benchmarks to come only after the page is loaded.

library(shiny)
library(shinythemes)
library(plotly)
# try both shinythemes and bslib libraries
navbarPage(
    title = 'Mutual Fund Analysis and Report',
    tabPanel('MF Summary',
        fluidPage(theme = shinytheme("spacelab"),
            sidebarPanel(
                fileInput("file1", "Upload the CAS PDF File"),
                passwordInput("password", "Password for the PDF:"),
                # textOutput('out_text'),
                actionButton("btn_proc", "Go!"),
                conditionalPanel(
                    condition = "input.btn_proc > 0",
                    dateRangeInput("date_range", "Analysis Period:",
                        start="1900-01-01", end="2099-12-31")
                )
            ),
            mainPanel(
                h3(textOutput('pf_xirr')),
                h3(textOutput('period_xirr')),
                br(), br(),
                h3(textOutput('text_ovr_sum')),
                DT::dataTableOutput('gains'),
                verbatimTextOutput('period_warnings'),
                br(), br(),
                conditionalPanel(
                    condition = "input.btn_proc > 0",
                    selectizeInput("mf_name", "Benchmarks:", choices=c(), multiple=TRUE),
                    DT::dataTableOutput('benchmark')
                ),
                br(), br(),
                h3(textOutput('text_fol_sum')),
                DT::dataTableOutput('summary')
                # h3('Folio Level Summary'),
                # DT::dataTableOutput('folio_level_summary')
            )
        )
    ),
    tabPanel('Portfolio',
        fluidPage(
            br(),
            plotlyOutput('portfolio_curve', height = '480px'),
            br(),
            plotlyOutput('xirr_over_time', height = '380px')
        )
    ),
    tabPanel('Transactions',
        fluidPage(
            DT::dataTableOutput('transactions')
        )
    ),
    tabPanel('NAV Status',
        fluidPage(
            br(),
            p('Shows each fund in your CAS, the matched mfapi.in scheme code, and whether NAV data came from the local cache or was freshly fetched from the internet.'),
            DT::dataTableOutput('nav_status')
        )
    ),
    tabPanel('ReadMe',
        fluidPage(
            tags$a(href="https://www.camsonline.com/Investors/Statements/Consolidated-Account-Statement",
                "CAMS KFinTech Consolidated Statement"),
            p('Visit the above link and have the options set as shown below and submit'),
            p('CAMS will email an encrypted pdf of the consolidated statement using the password provided'),
            p('Upload the encrypted pdf, and key-in the password to get the CAS analysis'),
            img(src='cams_screenshot.png', align = "left", width=672, height=500)
        )
    )

)