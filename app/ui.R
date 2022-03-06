navbarPage(
    title = 'Sample Page',
    tabPanel('Sample Tab1', DT::dataTableOutput('ex1')),
    tabPanel('Sample Tab2', DT::dataTableOutput('ex2'))
)