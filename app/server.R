function(input, output) {
    # display 10 rows initially
    output$ex1 <- DT::renderDataTable(
        DT::datatable(iris, options = list(pageLength = 25))
    )
    output$ex2 <- DT::renderDataTable(
        DT::datatable(mtcars, options = list(pageLength = 25))
    )
}