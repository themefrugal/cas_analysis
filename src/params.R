source('params_local.R')
pages <- pdf_text(file_path)

folio_ord_num <- 12
# debt_folios <- folio_lines[c(1, 7, 8, 9, 18, 20, 35, 36, 39)]
debt_folios <- folio_lines[c(1, 7, 18, 20, 35, 36, 39)]
#debt_folios <- folio_lines[c(11, 12, 13, 15, 17, 18, 32)]
equity_folios <- setdiff(folio_lines, debt_folios)

