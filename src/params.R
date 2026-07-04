# Optional helper for exploratory scripts.
# Pass the explicit parser state returned by parse_cas_pdf().

build_folio_groups <- function(cas_state, debt_indices = c(1, 7, 18, 20, 35, 36, 39)) {
    stopifnot(is.list(cas_state), 'folio_lines' %in% names(cas_state))
    debt_indices <- debt_indices[debt_indices <= length(cas_state$folio_lines)]
    debt_folios <- cas_state$folio_lines[debt_indices]
    equity_folios <- setdiff(cas_state$folio_lines, debt_folios)
    list(debt_folios = debt_folios, equity_folios = equity_folios)
}
