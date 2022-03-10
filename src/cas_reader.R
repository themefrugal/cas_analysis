library(data.table)
library(pdftools)
library(zeallot)
library(stringr)
library(dplyr)
library(tidyr)
library(tvm)

source('../src/cas_regex.R')
source('../src/params_local.R')

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

fund_and_advisor <- function(folio_ord_num){
    folio_to_txn_lines <- all_lines[folio_lines[folio_ord_num]:opening_lines[folio_ord_num]]
    fund_name_line <- folio_to_txn_lines[which(grepl(fund_name_pattern, folio_to_txn_lines))]
    # print(paste(folio_ord_num, fund_name_line))
    mf_name <- trimws(strsplit(fund_name_line, "\\s{6}")[[1]][1])

    fund_advisor <- str_split(gsub(fund_advisor_pattern, '\\1:::\\2', mf_name), ':::')[[1]]
    fund_part <- trimws(fund_advisor[1])
    if (length(fund_advisor) == 1){
        advisor_part <- ''
    } else {
        advisor_part <- trimws(fund_advisor[2])
    }
    return (c(fund_part, advisor_part))
}

get_transactions <- function(folio_ord_num){
    folio_range <- folio_lines[folio_ord_num]:closing_lines[folio_ord_num]
    working_set <- all_lines[folio_range]

    df_txns <- data.frame(date=character(), description=character(), amount=double())
    df_info <- data.frame(description=character())
    for (i in 1:length(working_set)){
        grouped_str <- gsub(transaction_pattern, '\\1xx\\2xx\\3', working_set[i])
        dt_desc_nums <- str_split(grouped_str, pattern = "xx")
        separated_words <- unlist(lapply(dt_desc_nums[[1]], trimws))

        if (length(separated_words) == 3){
            df_txns[nrow(df_txns) + 1, ]  <- separated_words
        } else {
            if(grepl('\\*+\\s*IDCW.*', separated_words)){
                idcw_line <- gsub(idcw_pattern, '\\1:::\\2:::\\4', separated_words)
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

    folio_pan_split <- str_split(all_lines[folio_lines[folio_ord_num]], folio_pan_pattern)[[1]]
    folio_num <- str_split(folio_pan_split, 'Folio No:\\s+')[[1]][2]
    pan_num <- substr(folio_pan_split[2], 1, 10)
    c(fund_part, advisor_part) %<-% fund_and_advisor(folio_ord_num)
    amc_name <- all_lines[tail(amc_lines[which(amc_lines < folio_lines[folio_ord_num])], 1)]

    dt_txns[, amc :=  amc_name]
    dt_txns[, fund :=  fund_part]
    dt_txns[, advisor :=  advisor_part]
    dt_txns[, folio :=  folio_num]
    dt_txns[, pan :=  pan_num]

    return (dt_txns)
}

get_mf_table <- function(folio_ord_num){
    dt_txns <- get_transactions(folio_ord_num)
    first_date <- min(dt_txns$date)
    last_date <- max(dt_txns$date)

    closing_line <- gsub(closing_line_pattern, '\\1 \\2 \\3 \\4', all_lines[closing_lines[folio_ord_num]])
    closing_strings <- str_split(closing_line, '\\s+')[[1]]
    #print(closing_strings)
    cur_value <- as.numeric(gsub(',', '', closing_strings[4]))

    c(fund_part, advisor_part) %<-% fund_and_advisor(folio_ord_num)
    xirr_val <- XIRR(dt_txns)
    df_mf <- rbind(data.frame(Fund = fund_part, Value = cur_value, Xirr = xirr_val, First=first_date, Recent=last_date))
    return (df_mf)
}

get_portfolio_transactions <- function(f_lines){
    # Getting all transactions
    dt_txns <- rbindlist(lapply(c(1:length(f_lines)), get_transactions))
    dt_txns <- dt_txns[order(date)]  # Sorting with Earliest Date first
    dt_txns[, days :=  as.numeric(max(dt_txns$date) - date)]
    dt_txns[, years := days/365.25]

    return (dt_txns)
}

get_select_transactions <- function(selectors, presence){
    stopifnot(length(selectors) == length(presence))
    dt_cur_txns <- dt_all_txns
    for (i in c(1:length(selectors))){
        selector <- selectors[i]
        inclusion <- presence[i]
        if (inclusion == 'IN'){
            dt_cur_txns <- dt_cur_txns[get(names(selector)) %in% unlist(selector)]
        } else {
            dt_cur_txns <- dt_cur_txns[!get(names(selector)) %in% unlist(selector)]
        }
    }
    if(nrow(dt_cur_txns) > 0){
        dt_cur_txns[, days :=  as.numeric(max(dt_cur_txns$date) - date)]
        dt_cur_txns[, years := days/365.25]
    }
    return (dt_cur_txns)
}

