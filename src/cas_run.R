source('./params_local.R')
source('../app/cas_reader.R')

cas_state <- parse_cas_pdf(file_path)

# Processing IRR for a single transaction
folio_ord_num <- 12
dt_txn <- get_transactions(folio_ord_num, cas_state)
xirr_folio <- XIRR(dt_txn)

dt_all_txns <- get_portfolio_transactions(cas_state)
xirr_all <- XIRR(dt_all_txns)

selectors <- list(amc=c('Tata Mutual Fund', 'HDFC Mutual Fund'), advisor='DIRECT')
presence <- c('IN', 'NOT_IN')
dt_txn <- get_select_transactions(selectors, presence, dt_all_txns)
xirr_sel <- XIRR(dt_txn)

# dt_equity_txns <- get_portfolio_transactions(equity_folios)
# xirr_equity <- XIRR(dt_equity_txns)

# dt_debt_txns <- get_portfolio_transactions(debt_folios)
# xirr_debt <- XIRR(dt_debt_txns)

# Creating MF Summary table
dt_full_table <- rbindlist(lapply(seq_along(cas_state$folio_lines), get_mf_table,
                                  state = cas_state))
