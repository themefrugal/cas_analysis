source('../src/params_local.R')
source('../src/cas_reader.R')

function(input, output) {
    dt_mf_xirrs <- reactive({
        pages <- pdf_text(input$file1$datapath, upw=input$password)
        all_lines <- c()
        for (i in 1:length(pages)){
            lines <- str_split(pages[i], pattern="\n")
            all_lines <- c(all_lines, lines[[1]])
        }

        folio_lines <- which(grepl("Folio No:", all_lines, fixed = TRUE))
        amc_lines <- which(grepl("Mutual Fund", all_lines, fixed = TRUE))
        opening_lines <- which(grepl("Opening Unit Balance:", all_lines, fixed = TRUE))
        closing_lines <- which(grepl("Closing Unit Balance:", all_lines, fixed = TRUE))
        dt_full_table <- rbindlist(lapply(c(1:length(folio_lines)), get_mf_table))
        dt_full_table
    })
    output$summary <- DT::renderDataTable(
        DT::datatable(dt_mf_xirrs(), options = list(pageLength = 25))
    )
    output$out_text <- renderText({
        input$password
    })
}