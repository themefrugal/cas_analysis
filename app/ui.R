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
.app-brand {
    margin-bottom: 1rem;
}
.app-brand-title {
    color: #172033;
    font-size: 1.05rem;
    font-weight: 800;
    line-height: 1.1;
}
.app-brand-subtitle {
    color: #667085;
    font-size: 0.78rem;
    font-weight: 650;
    letter-spacing: 0;
    margin-top: 0.25rem;
    text-transform: uppercase;
}
.app-nav {
    display: flex;
    flex-direction: column;
    gap: 0.25rem;
    margin-bottom: 1rem;
}
.app-nav .btn {
    border: 0;
    border-radius: 8px;
    color: #344054;
    font-weight: 650;
    justify-content: flex-start;
    padding: 0.55rem 0.7rem;
    text-align: left;
}
.app-nav .btn:hover,
.app-nav .btn:focus {
    background: #eaf4ff;
    color: #1f5f8b;
}
.app-nav .btn.active {
    background: #1f5f8b;
    color: #ffffff;
    box-shadow: inset 3px 0 0 #bfd7ea;
}
.nav-tabs .nav-link {
    color: #475467;
    border: 0;
    border-bottom: 3px solid transparent;
}
.nav-tabs .nav-link:hover,
.nav-tabs .nav-link:focus {
    color: #1f5f8b;
    border-bottom-color: #bfd7ea;
}
.nav-tabs .nav-link.active {
    color: #1f5f8b !important;
    background: transparent !important;
    border-bottom-color: #1f5f8b !important;
    font-weight: 650;
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
.workflow-status {
    border-radius: 8px;
    font-size: 0.88rem;
    font-weight: 600;
    margin-top: 0.75rem;
    padding: 0.7rem 0.8rem;
}
.workflow-status.idle {
    background: #f2f4f7;
    color: #475467;
}
.workflow-status.ready {
    background: #eaf4ff;
    color: #175cd3;
}
.workflow-status.done {
    background: #ecfdf3;
    color: #067647;
}
.warning-box {
    background: #fffaeb;
    border: 1px solid #fedf89;
    border-radius: 8px;
    color: #93370d;
    font-size: 0.9rem;
    margin-top: 0.75rem;
    padding: 0.75rem 0.9rem;
    white-space: pre-wrap;
}
.empty-state {
    align-items: center;
    color: #667085;
    display: flex;
    min-height: 180px;
    text-align: center;
}
.empty-state strong {
    color: #172033;
    display: block;
    font-size: 1.05rem;
    margin-bottom: 0.35rem;
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
.detail-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
    gap: 0.85rem;
}
.diagnostic-ok {
    color: #067647;
    font-weight: 650;
}
.diagnostic-warn {
    color: #b54708;
    font-weight: 650;
}
.diagnostic-high {
    color: #b42318;
    font-weight: 650;
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
.sample-pdf-links {
    border: 1px solid #e4e8ef;
    border-radius: 8px;
    padding: 0.7rem 0.8rem;
    background: #ffffff;
}
.sample-pdf-links summary {
    cursor: pointer;
    font-weight: 700;
}
.sample-pdf-links a {
    display: block;
    margin-top: 0.4rem;
    font-size: 0.88rem;
}
.card,
.bslib-card,
.section-stack,
.bslib-grid,
.bslib-sidebar-layout {
    overflow: visible;
}
.selectize-control {
    position: relative;
    z-index: 20;
}
.selectize-dropdown,
.selectize-dropdown.form-control {
    z-index: 10000 !important;
}
"

sample_pdf_files <- list.files(
    file.path('www', 'samples'),
    pattern = '\\.pdf$',
    full.names = FALSE
)
sample_pdf_links <- if (length(sample_pdf_files) > 0) {
    tags$details(
        class = 'sample-pdf-links',
        tags$summary('Sample PDFs'),
        tags$small('Use blank password. All investor data is fictional.'),
        lapply(sample_pdf_files, function(file) {
            label <- tools::file_path_sans_ext(gsub('-', ' ', file))
            tags$a(href = file.path('samples', file), download = file, tools::toTitleCase(label))
        })
    )
} else {
    NULL
}

nav_button <- function(id, label) {
    actionButton(id, label, class = "btn-light w-100 app-nav-button")
}

app_sidebar <- sidebar(
    width = 310,
    div(
        class = "app-brand",
        div(class = "app-brand-title", "CAS Portfolio Analytics"),
        div(class = "app-brand-subtitle", "Mutual fund workspace")
    ),
    div(
        class = "app-nav",
        nav_button("nav_benchmark", "Benchmark"),
        nav_button("nav_portfolio", "Portfolio"),
        nav_button("nav_analytics", "Analytics"),
        nav_button("nav_fund_detail", "Fund Detail"),
        nav_button("nav_insights", "Insights"),
        nav_button("nav_transactions", "Transactions"),
        nav_button("nav_report", "Report"),
        nav_button("nav_help", "Help"),
        uiOutput("health_nav_links")
    ),
    hr(),
    fileInput("file1", "CAS PDF"),
    passwordInput("password", "PDF password"),
    sample_pdf_links,
    actionButton("btn_proc", "Analyze", class = "btn-primary w-100"),
    uiOutput("workflow_status"),
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

settings_script <- tags$script(HTML("
document.addEventListener('shiny:connected', function() {
  ['analytics_hierarchy', 'settings_page_size', 'mf_name'].forEach(function(id) {
    const value = localStorage.getItem('cas_' + id);
    if (value !== null) {
      setTimeout(function() {
        try {
          const parsed = JSON.parse(value);
          $('#' + id).val(parsed).trigger('change');
          Shiny.setInputValue(id, parsed, {priority: 'event'});
        } catch(e) {
          $('#' + id).val(value).trigger('change');
          Shiny.setInputValue(id, value, {priority: 'event'});
        }
      }, 900);
    }
  });
});
$(document).on('change', '#analytics_hierarchy, #settings_page_size, #mf_name', function() {
  localStorage.setItem('cas_' + this.id, JSON.stringify($(this).val()));
});
Shiny.addCustomMessageHandler('set-active-nav', function(id) {
  $('.app-nav .btn').removeClass('active');
  $('#' + id).addClass('active');
});
"))

page_sidebar(
    title = "CAS Portfolio Analytics",
    theme = app_theme,
    sidebar = app_sidebar,
    tags$head(tags$style(HTML(app_css)), settings_script),
    navset_hidden(
        id = "main_nav",
        selected = "benchmark",

    nav_panel(
        "Benchmark",
        value = "benchmark",
        div(
            class = "section-stack",
            conditionalPanel(
                condition = "input.btn_proc === 0",
                card(
                    div(
                        class = "empty-state",
                        div(
                            strong("Upload a CAS PDF to begin"),
                            "The analysis workspace will populate after parsing, fund matching, NAV loading, and portfolio calculations complete."
                        )
                    )
                )
            ),
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
                uiOutput("period_warnings")
            ),
            card(
                card_header("Benchmark comparison"),
                selectizeInput("mf_name", "Benchmarks", choices = c(), multiple = TRUE,
                               options = list(dropdownParent = "body")),
                uiOutput("benchmark_context"),
                uiOutput("benchmark_empty"),
                DT::dataTableOutput("benchmark")
            )
        )
    ),

    nav_panel(
        "Portfolio",
        value = "portfolio",
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
        value = "analytics",
        div(
            class = "section-stack",
            card(
                card_header("Grouping"),
                layout_columns(
                    col_widths = c(4, 8),
                    selectizeInput(
                        "analytics_hierarchy",
                        "Hierarchy",
                        choices = c(
                            "AMC -> Category -> Sub-Category -> Scheme",
                            "AMC -> Folio -> Category -> Sub-Category -> Scheme",
                            "Category -> Sub-Category -> AMC -> Scheme",
                            "Category -> AMC -> Sub-Category -> Scheme",
                            "Custom"
                        ),
                        selected = "AMC -> Category -> Sub-Category -> Scheme",
                        options = list(dropdownParent = "body")
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
                                dropdownParent = "body",
                                plugins = list("drag_drop", "remove_button"),
                                placeholder = "Pick and order grouping levels"
                            )
                        ),
                        div(
                            class = "control-note",
                            "After Scheme, only Folio can be added as a lower grouping level."
                        )
                    )
                ),
                div(
                    class = "control-note",
                    "Each hierarchy row recalculates XIRR% from its underlying cash flows."
                )
            ),
            card(
                card_header("Portfolio analytics"),
                reactable::reactableOutput("analytics_table")
            )
        )
    ),

    nav_panel(
        "Fund Detail",
        value = "fund_detail",
        div(
            class = "section-stack",
            navset_tab(
                id = "fund_detail_section",
                nav_panel(
                    "Funds Summary",
                    card(
                        card_header("Funds summary"),
                        div(
                            class = "control-note",
                            "Select a row to open that fund in the drilldown section."
                        ),
                        DT::dataTableOutput("summary")
                    )
                ),
                nav_panel(
                    "Drilldown",
                    div(
                        class = "section-stack",
                        card(
                            card_header("Drilldown"),
                            selectizeInput("fund_detail", "Fund", choices = c(), multiple = FALSE,
                                           options = list(dropdownParent = "body")),
                            uiOutput("fund_detail_kpis")
                        ),
                        card(
                            card_header("Match explanation"),
                            DT::dataTableOutput("fund_match_detail")
                        ),
                        card(
                            card_header("Fund transactions"),
                            DT::dataTableOutput("fund_detail_transactions")
                        )
                    )
                )
            )
        )
    ),

    nav_panel(
        "Insights",
        value = "insights",
        div(
            class = "section-stack",
            card(
                card_header("Chart controls"),
                layout_columns(
                    col_widths = c(6, 6),
                    selectizeInput(
                        "insights_sort_order",
                        "Sort order",
                        choices = c("Descending" = "desc", "Ascending" = "asc"),
                        selected = "desc",
                        options = list(dropdownParent = "body")
                    ),
                    radioButtons(
                        "insights_display_mode",
                        "Display",
                        choices = c("Value" = "value", "Percentage" = "percent"),
                        selected = "value",
                        inline = TRUE
                    )
                )
            ),
            layout_columns(
                col_widths = c(6, 6),
                card(
                    card_header("Top gain contributors"),
                    plotlyOutput("top_contributors", height = "360px")
                ),
                card(
                    card_header("Weakest contributors"),
                    plotlyOutput("bottom_contributors", height = "360px")
                )
            ),
            layout_columns(
                col_widths = c(6, 6),
                card(
                    card_header("Category allocation"),
                    plotlyOutput("category_allocation", height = "360px")
                ),
                card(
                    card_header("AMC allocation"),
                    plotlyOutput("amc_allocation", height = "360px")
                )
            ),
            card(
                card_header("Top funds by current value"),
                plotlyOutput("top_funds_value", height = "380px")
            )
        )
    ),

    nav_panel(
        "Diagnostics",
        value = "diagnostics",
        div(
            class = "section-stack",
            card(
                card_header("Data quality dashboard"),
                DT::dataTableOutput("quality_diagnostics")
            ),
            card(
                card_header("Scheme match explainability"),
                DT::dataTableOutput("match_explainability")
            )
        )
    ),

    nav_panel(
        "Report",
        value = "report",
        div(
            class = "section-stack",
            card(
                card_header("Export report"),
                div(
                    class = "control-note",
                    "Download a standalone HTML snapshot of the current portfolio summary, attribution, diagnostics, and warnings."
                ),
                br(),
                downloadButton("download_report", "Download HTML report", class = "btn-primary")
            ),
            card(
                card_header("Preferences"),
                numericInput("settings_page_size", "Default table page size", value = 25,
                             min = 10, max = 200, step = 5),
                div(
                    class = "control-note",
                    "Benchmark, hierarchy, and page size preferences are saved in this browser."
                ),
                hr(),
                actionButton("toggle_health_tabs", "Show health-check tabs",
                             class = "btn-outline-secondary w-100"),
                div(
                    class = "control-note",
                    "Diagnostics and NAV Status are intended for troubleshooting and are hidden during normal use."
                )
            )
        )
    ),

    nav_panel(
        "Transactions",
        value = "transactions",
        card(
            card_header("Transaction ledger"),
            DT::dataTableOutput("transactions")
        )
    ),

    nav_panel(
        "NAV Status",
        value = "nav_status",
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
        value = "help",
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
)
