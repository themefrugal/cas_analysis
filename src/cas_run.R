source('./params_local.R')
source('./cas_reader.R')

pages <- pdf_text(file_path)
all_lines <- c()
for (i in 1:length(pages)){
    lines <- str_split(pages[i], pattern="\n")
    all_lines <- c(all_lines, lines[[1]])
}

folio_lines <- which(grepl("Folio No:", all_lines, fixed = TRUE))
amc_lines <- which(grepl("Mutual Fund", all_lines, fixed = TRUE))
opening_lines <- which(grepl("Opening Unit Balance:", all_lines, fixed = TRUE))
closing_lines <- which(grepl("Closing Unit Balance:", all_lines, fixed = TRUE))

# Processing IRR for a single transaction
folio_ord_num <- 12
dt_txn <- get_transactions(folio_ord_num)
xirr_folio <- XIRR(dt_txn)

dt_all_txns <- get_portfolio_transactions(folio_lines)
xirr_all <- XIRR(dt_all_txns)

selectors <- list(amc=c('Tata Mutual Fund', 'HDFC Mutual Fund'), advisor='DIRECT')
presence <- c('IN', 'NOT_IN')
dt_txn <- get_select_transactions(selectors, presence)
xirr_sel <- XIRR(dt_txn)

# dt_equity_txns <- get_portfolio_transactions(equity_folios)
# xirr_equity <- XIRR(dt_equity_txns)

# dt_debt_txns <- get_portfolio_transactions(debt_folios)
# xirr_debt <- XIRR(dt_debt_txns)

# Creating MF Summary table
dt_full_table <- rbindlist(lapply(c(1:length(folio_lines)), get_mf_table))
