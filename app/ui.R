# To do: comparative chart with any selected mutual fund or index.encrypted
library(shiny)
library(shinythemes)
# try both shinythemes and bslib libraries
navbarPage(
    title = 'Mutual Fund Analysis and Report',
    tabPanel('MF Summary',
        fluidPage(theme = shinytheme("spacelab"),
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
                DT::dataTableOutput('benchmark'),
                br(), br(),
                h3(textOutput('text_fol_sum')),
                DT::dataTableOutput('summary')
                # h3('Folio Level Summary'),
                # DT::dataTableOutput('folio_level_summary')
            )
        )
    ),
    tabPanel('Transactions',
        fluidPage(
            DT::dataTableOutput('transactions')
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