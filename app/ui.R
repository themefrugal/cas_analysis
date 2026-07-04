library(shiny)
library(bslib)
library(plotly)
library(reactable)

app_theme <- bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1f5f8b",
    success = "#2f7d57",
    danger = "#b3261e",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
)

app_css <- "
body {
    background: #f6f8fb;
}
.navbar {
    box-shadow: 0 1px 0 rgba(16, 24, 40, 0.08);
}
.bslib-sidebar-layout {
    --bslib-sidebar-main-bg: #f6f8fb;
}
.card {
    border: 1px solid #e4e8ef;
    box-shadow: 0 1px 2px rgba(16, 24, 40, 0.04);
}
.card-header {
    background: #fff;
    border-bottom: 1px solid #edf0f5;
    font-weight: 650;
}
.control-note {
    color: #667085;
    font-size: 0.86rem;
    line-height: 1.35;
}
.section-stack {
    display: flex;
    flex-direction: column;
    gap: 1rem;
}
.kpi-grid {
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 0.85rem;
}
.kpi-card {
    background: #fff;
    border: 1px solid #e4e8ef;
    border-radius: 8px;
    padding: 1rem;
    min-height: 92px;
}
.kpi-label {
    color: #667085;
    font-size: 0.78rem;
    font-weight: 650;
    text-transform: uppercase;
}
.kpi-value {
    color: #172033;
    font-size: 1.15rem;
    font-weight: 700;
    margin-top: 0.35rem;
}
.help-image {
    max-width: 100%;
    height: auto;
    border: 1px solid #e4e8ef;
    border-radius: 8px;
}
.dataTables_wrapper {
    font-size: 0.88rem;
}
.dataTables_wrapper .dataTables_filter input,
.dataTables_wrapper .dataTables_length select {
    border: 1px solid #d0d5dd;
    border-radius: 6px;
    padding: 0.25rem 0.45rem;
}
table.dataTable thead th {
    white-space: nowrap;
}
table.dataTable tbody td {
    vertical-align: middle;
}
"

analysis_controls <- sidebar(
    width = 330,
    fileInput("file1", "CAS PDF"),
    passwordInput("password", "PDF password"),
    actionButton("btn_proc", "Analyze", class = "btn-primary w-100"),
    conditionalPanel(
        condition = "input.btn_proc > 0",
        hr(),
        dateRangeInput(
            "date_range",
            "Analysis period",
            start = "1900-01-01",
            end = "2099-12-31"
        )
    ),
    hr(),
    div(
        class = "control-note",
        "Upload a CAMS CAS PDF and run the analysis. NAV and scheme metadata are cached locally for faster repeat runs."
    )
)

page_navbar(
    title = "CAS Portfolio Analytics",
    theme = app_theme,
    header = tags$head(tags$style(HTML(app_css))),

    nav_panel(
        "Summary",
        layout_sidebar(
            sidebar = analysis_controls,
            div(
                class = "section-stack",
                div(
                    class = "kpi-grid",
                    div(
                        class = "kpi-card",
                        div(class = "kpi-label", "Overall return"),
                        div(class = "kpi-value", textOutput("pf_xirr", inline = TRUE))
                    ),
                    div(
                        class = "kpi-card",
                        div(class = "kpi-label", "Period return"),
                        div(class = "kpi-value", textOutput("period_xirr", inline = TRUE))
                    )
                ),
                card(
                    card_header(textOutput("text_ovr_sum", inline = TRUE)),
                    DT::dataTableOutput("gains"),
                    verbatimTextOutput("period_warnings")
                ),
                conditionalPanel(
                    condition = "input.btn_proc > 0",
                    card(
                        card_header("Benchmark comparison"),
                        selectizeInput("mf_name", "Benchmarks", choices = c(), multiple = TRUE),
                        DT::dataTableOutput("benchmark")
                    )
                ),
                card(
                    card_header(textOutput("text_fol_sum", inline = TRUE)),
                    DT::dataTableOutput("summary")
                )
            )
        )
    ),

    nav_panel(
        "Portfolio",
        div(
            class = "section-stack",
            card(
                card_header("Portfolio growth"),
                plotlyOutput("portfolio_curve", height = "480px")
            ),
            card(
                card_header("XIRR over time"),
                plotlyOutput("xirr_over_time", height = "380px")
            )
        )
    ),

    nav_panel(
        "Analytics",
        div(
            class = "section-stack",
            card(
                card_header("Grouping"),
                layout_columns(
                    col_widths = c(4, 8),
                    selectInput(
                        "analytics_hierarchy",
                        "Hierarchy",
                        choices = c(
                            "AMC -> Category -> Sub-Category -> Scheme",
                            "AMC -> Folio -> Category -> Sub-Category -> Scheme",
                            "Category -> Sub-Category -> AMC -> Scheme",
                            "Category -> AMC -> Sub-Category -> Scheme",
                            "Custom"
                        ),
                        selected = "AMC -> Category -> Sub-Category -> Scheme"
                    ),
                    conditionalPanel(
                        condition = "input.analytics_hierarchy === 'Custom'",
                        selectizeInput(
                            "analytics_custom_cols",
                            "Group levels",
                            choices = c("AMC", "Category", "Sub-Category", "Scheme", "Folio"),
                            selected = c("Category", "Sub-Category"),
                            multiple = TRUE,
                            options = list(
                                plugins = list("drag_drop", "remove_button"),
                                placeholder = "Pick and order grouping levels"
                            )
                        )
                    )
                ),
                div(
                    class = "control-note",
                    "Financial columns are summed at each level. XIRR% is shown at the leaf level."
                )
            ),
            card(
                card_header("Portfolio analytics"),
                reactable::reactableOutput("analytics_table")
            )
        )
    ),

    nav_panel(
        "Transactions",
        card(
            card_header("Transaction ledger"),
            DT::dataTableOutput("transactions")
        )
    ),

    nav_panel(
        "NAV Status",
        card(
            card_header("NAV cache and scheme matching"),
            div(
                class = "control-note",
                "Shows each fund in your CAS, the matched mfapi.in scheme code, and whether NAV data came from cache or the API."
            ),
            DT::dataTableOutput("nav_status")
        )
    ),

    nav_panel(
        "Help",
        card(
            card_header("Import guide"),
            tags$p(
                tags$a(
                    href = "https://www.camsonline.com/Investors/Statements/Consolidated-Account-Statement",
                    "CAMS KFinTech Consolidated Account Statement"
                )
            ),
            tags$p("Request the encrypted CAS PDF from CAMS, then upload it here with the PDF password."),
            tags$img(src = "cams_screenshot.png", class = "help-image")
        )
    )
)
