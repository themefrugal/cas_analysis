test_that("bundled sample PDFs parse through the CAS parser", {
    skip_if_not_installed('pdftools')

    sample_dir <- file.path(repo_root, 'app', 'www', 'samples')
    sample_files <- list.files(sample_dir, pattern = '\\.pdf$', full.names = TRUE)
    expect_true(length(sample_files) >= 3)

    for (sample_file in sample_files) {
        pages <- pdftools::pdf_text(sample_file)
        state <- cas_state_from_pages(pages)
        dt <- get_portfolio_transactions(state)

        expect_gt(nrow(dt), 0)
        expect_true('Cur Value' %in% dt[, description])
        expect_true(all(grepl('^[A-E]{5}[0-4]{4}[A-E]$', unique(dt[, pan]))))
        expect_true(all(grepl('^90000000[1-5]', unique(dt[, folio]))))
    }
})
