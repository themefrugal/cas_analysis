# Compatibility facade for the CAS analysis app.
# The implementation is split into:
#   cas_parser.R   - PDF/text parsing into transactions
#   market_data.R  - scheme matching, ISIN/category/NAV APIs and caches
#   analytics.R    - cash-flow analytics, XIRR, valuation and curves

list.of.packages <- c(
    'rjson', 'data.table', 'pdftools', 'zeallot', 'stringr', 'dplyr', 'tidyr',
    'tvm', 'DBI', 'RSQLite', 'httr', 'reactable', 'bslib'
)

missing_packages <- list.of.packages[
    !vapply(list.of.packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
    stop('Missing required R packages: ', paste(missing_packages, collapse = ', '),
         '. Install them before running the app.')
}
suppressPackageStartupMessages(
    invisible(lapply(list.of.packages, library, character.only = TRUE))
)

cas_reader_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
cas_reader_dir <- if (!is.na(cas_reader_file)) dirname(cas_reader_file) else getwd()

source(file.path(cas_reader_dir, 'cas_parser.R'))
source(file.path(cas_reader_dir, 'market_data.R'))
source(file.path(cas_reader_dir, 'analytics.R'))
