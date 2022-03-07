source('../src/params_local.R')
source('../src/cas_reader.R')
function(input, output) {
    # display 10 rows initially
    output$summary <- DT::renderDataTable(
        DT::datatable(dt_full_table, options = list(pageLength = 25))
    )
    output$transactions <- DT::renderDataTable(
        DT::datatable(dt_all_txns, options = list(pageLength = 25))
    )
}