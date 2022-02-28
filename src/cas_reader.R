library(pdftools)
library(stringr)
library(data.table)
library(dplyr)
library(tidyr)
library(tvm)

XIRR <- function(dt_txn){
    out <- tryCatch(
    {
        fvs <- function(r){ sum(dt_txn$amt * (1 + r) ^ (dt_txn$years)) }
        unx <- uniroot(fvs, c(-1,1))
        return (unx$root)
    },
    error = function(cond){
        return (0)
    },
    warning = function(cond){
        return (0)
    },
    finally = {

    })
    return (out)
}

scheme_name <- function(folio_ord_num){
    folio_to_txn_lines <- all_lines[folio_lines[folio_ord_num]:opening_lines[folio_ord_num]]
    fund_name_line <- folio_to_txn_lines[which(grepl("^[A-Z0-9]+-[A-Za-z&]+\\s", folio_to_txn_lines))]
    # print(paste(folio_ord_num, fund_name_line))
    mf_name <- trimws(strsplit(fund_name_line, "\\s{6}")[[1]][1])
    return (mf_name)
}

get_transactions <- function(folio_ord_num){
    folio_range <- folio_lines[folio_ord_num]:closing_lines[folio_ord_num]
    working_set <- all_lines[folio_range]

    df_txns <- data.frame(date=character(), description=character(), amount=double())
    df_info <- data.frame(description=character())
    for (i in 1:length(working_set)){
        grouped_str <- gsub('(\\d{2}-[A-Za-z]{3}-\\d{4})(.*)((\\s+[(),.0-9]+){4})', '\\1xx\\2xx\\3', working_set[i])
        dt_desc_nums <- str_split(grouped_str, pattern = "xx")
        separated_words <- unlist(lapply(dt_desc_nums[[1]], trimws))

        if (length(separated_words) == 3){
            df_txns[nrow(df_txns) + 1, ]  <- separated_words
        } else {
            if(grepl('\\*+\\s*IDCW.*', separated_words)){
                idcw_line <- gsub('(\\d{2}-[A-Za-z]{3}-\\d{4})\\s+\\*+(.*)\\*+(\\s+)([,.0-9]+)',
                    '\\1:::\\2:::\\4', separated_words)
                dt_desc_value <- str_split(idcw_line, pattern = ':::')
                idcw_words <- unlist(lapply(dt_desc_value[[1]], trimws))
                idcw_words[3] <- paste(paste0('(', idcw_words[3], ')'), '0', '0', '0')
                df_txns[nrow(df_txns) + 1, ]  <- idcw_words
            }
            df_info[nrow(df_info) + 1, ] <- separated_words
        }
    }

    df_txns <- df_txns %>% separate(amount, c("amt", "units", "nav", "bal_units"), "\\s+")
    dt_txns <- data.table(df_txns)
    dt_txns[, amt := gsub('\\(([,.0-9]+)\\)', '-\\1', amt)]
    dt_txns[, units := gsub('\\(([,.0-9]+)\\)', '-\\1', units)]

    dt_txns[, amt := as.numeric(gsub(',', '', amt))]
    dt_txns[, units := as.numeric(gsub(',', '', units))]
    dt_txns[, nav := as.numeric(gsub(',', '', nav))]
    dt_txns[, bal_units := as.numeric(gsub(',', '', bal_units))]
    dt_txns[, date := as.Date(date, format="%d-%b-%Y")]

    dt_txn_dr <- dt_txns[description %like% 'IDCW Reinvest']
    if(nrow(dt_txn_dr) > 0){
        dt_txn_dr$amt <- -dt_txn_dr$amt
        dt_txns <- rbind(dt_txns, dt_txn_dr)
    }
    dt_txns <- dt_txns[order(date)]  # Sorting with Earliest Date first

    closing_line_pattern <-
        'Closing\\s+Unit\\s+Balance:\\s+([0-9,.]+).*INR\\s+([0-9,.]+).*Valuation\\s+on\\s+(\\d{2}-[A-Za-z]{3}-\\d{4}):\\s+INR\\s+([0-9,.]+)'
    closing_line <- gsub(closing_line_pattern, '\\1 \\2 \\3 \\4', all_lines[closing_lines[folio_ord_num]])
    closing_strings <- str_split(closing_line, '\\s+')[[1]]
    #print(closing_strings)
    cur_value <- as.numeric(gsub(',', '', closing_strings[4]))
    if (cur_value != 0){
        dt_txns <- rbind(dt_txns, data.table(
            date=as.Date(closing_strings[3], format="%d-%b-%Y"), description="Cur Value", amt=-cur_value,
            units=0, nav=as.numeric(gsub(',', '', closing_strings[2])),
            bal_units=as.numeric(gsub(',', '', closing_strings[1]))))
    }

    dt_txns[, days :=  as.numeric(max(dt_txns$date) - date)]
    dt_txns[, years := days/365.25]

    folio_pan_split <- str_split(all_lines[folio_lines[folio_ord_num]], '\\s+PAN:\\s+')[[1]]
    folio_num <- str_split(folio_pan_split, 'Folio No:\\s+')[[1]][2]
    pan_num <- substr(folio_pan_split[2], 1, 10)
    fund_name <- scheme_name(folio_ord_num)
    amc_name <- amc_lines[folio_ord_num]

    dt_txns[, amc :=  amc_name]
    dt_txns[, fund :=  fund_name]
    dt_txns[, folio :=  folio_num]
    dt_txns[, pan :=  pan_num]

    return (dt_txns)
}

get_mf_table <- function(folio_ord_num){
    dt_txns <- get_transactions(folio_ord_num)
    first_date <- min(dt_txns$date)
    last_date <- max(dt_txns$date)

    closing_line_pattern <-
        'Closing Unit Balance: ([0-9,.]+).*INR ([0-9,.]+).*Valuation on (\\d{2}-[A-Za-z]{3}-\\d{4}): INR ([0-9,.]+)'
    closing_line <- gsub(closing_line_pattern, '\\1 \\2 \\3 \\4', all_lines[closing_lines[folio_ord_num]])
    closing_strings <- str_split(closing_line, '\\s+')[[1]]
    #print(closing_strings)
    cur_value <- as.numeric(gsub(',', '', closing_strings[4]))

    folio_to_txn_lines <- all_lines[folio_lines[folio_ord_num]:opening_lines[folio_ord_num]]
    fund_name_line <- folio_to_txn_lines[which(grepl("^[A-Z0-9]+-[A-Za-z&]+\\s", folio_to_txn_lines))]
    # print(paste(folio_ord_num, fund_name_line))
    mf_name <- trimws(strsplit(fund_name_line, "\\s{6}")[[1]][1])
    # mf_name <- trimws(gsub('[A-Z0-9]+-(.*)\\s+Registrar :.*', '\\1', all_lines[folio_lines[folio_ord_num] + 1]))
    xirr_val <- XIRR(dt_txns)
    df_mf <- rbind(data.frame(Name = mf_name, Value = cur_value, Xirr = xirr_val, First=first_date, Recent=last_date))
    return (df_mf)
    # print(paste(mf_name, cur_value, xirr_val))
}

get_portfolio_transactions <- function(f_lines){
    # Getting all transactions
    dt_txns <- rbindlist(lapply(c(1:length(f_lines)), get_transactions))
    dt_txns <- dt_txns[order(date)]  # Sorting with Earliest Date first
    dt_txns[, days :=  as.numeric(max(dt_txns$date) - date)]
    dt_txns[, years := days/365.25]

    return (dt_txns)
}

source('./params_local.R')
pages <- pdf_text(file_path)
all_lines <- c()
for (i in 1:length(pages)){
    lines <- str_split(pages[i], pattern="\n")
    all_lines <- c(all_lines, lines[[1]])
}

folio_lines <- which(grepl("Folio No:", all_lines, fixed = TRUE))
amc_lines <- folio_lines - 1
opening_lines <- which(grepl("Opening Unit Balance:", all_lines, fixed = TRUE))
closing_lines <- which(grepl("Closing Unit Balance:", all_lines, fixed = TRUE))

# Processing IRR for a single transaction
dt_txn <- get_transactions(folio_ord_num)
xirr_folio <- XIRR(dt_txn)

dt_all_txns <- get_portfolio_transactions(folio_lines)
xirr_all <-XIRR(dt_all_txns)

dt_equity_txns <- get_portfolio_transactions(equity_folios)
xirr_equity <- XIRR(dt_equity_txns)

dt_debt_txns <- get_portfolio_transactions(debt_folios)
xirr_debt <- XIRR(dt_debt_txns)

# Creating MF Summary table
dt_full_table <- rbindlist(lapply(c(1:length(folio_lines)), get_mf_table))
