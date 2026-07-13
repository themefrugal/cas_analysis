# Market-data layer: scheme lookup, ISIN/category metadata, NAV APIs, and caches.

NAV_CACHE_DIR <- './nav_cache'

.nav_memory_cache <- new.env(parent = emptyenv())
.isin_memory_cache <- new.env(parent = emptyenv())

get_navs <- function(scheme_code) {
    mf_url <- paste0('https://api.mfapi.in/mf/', scheme_code)
    max_attempts <- 3L
    json_data    <- NULL
    last_error   <- NULL

    for (attempt in seq_len(max_attempts)) {
        tryCatch({
            json_data <- fromJSON(paste(readLines(mf_url, warn = FALSE), collapse = ''))
        }, error = function(e) {
            last_error <<- e
        })
        if (!is.null(json_data)) break
        if (attempt < max_attempts) Sys.sleep(2 ^ (attempt - 1))
    }
    if (is.null(json_data)) {
        stop('Failed to fetch NAV data for scheme ', scheme_code,
             ' after ', max_attempts, ' attempts. Last error: ',
             conditionMessage(last_error))
    }

    dt_navs <- data.table(do.call(rbind.data.frame, json_data[[2]]))
    dt_navs[, date := as.Date(date, format = '%d-%m-%Y')]
    dt_navs[, nav := as.numeric(nav)]
    dt_navs <- dt_navs[order(date)]

    all_dates <- seq.Date(min(dt_navs$date), max(dt_navs$date), by = 1)
    dt_all_dates <- data.table(date = all_dates)
    dt_navs <- merge(dt_all_dates, dt_navs, by = 'date', all.x = TRUE)
    dt_navs$nav <- nafill(dt_navs$nav, type = 'locf')
    dt_navs <- dt_navs[nav != 0]
    dt_navs
}

get_cached_navs <- function(scheme_code, required_date = Sys.Date()) {
    scheme_code <- as.character(scheme_code)
    cache_key <- paste0('scheme_', scheme_code)
    if (exists(cache_key, envir = .nav_memory_cache, inherits = FALSE)) {
        dt_mem <- get(cache_key, envir = .nav_memory_cache)
        if (max(dt_mem$date) >= required_date - 7) return(dt_mem)
    }

    if (!dir.exists(NAV_CACHE_DIR)) dir.create(NAV_CACHE_DIR, recursive = TRUE)
    cache_file <- file.path(NAV_CACHE_DIR, paste0(scheme_code, '.rds'))

    if (file.exists(cache_file)) {
        dt_cached <- readRDS(cache_file)
        assign(cache_key, dt_cached, envir = .nav_memory_cache)
        if (max(dt_cached$date) >= required_date - 7) return(dt_cached)
    }

    tryCatch({
        dt_navs <- get_navs(scheme_code)
        saveRDS(dt_navs, cache_file)
        assign(cache_key, dt_navs, envir = .nav_memory_cache)
        dt_navs
    }, error = function(e) {
        if (file.exists(cache_file)) {
            dt_cached <- readRDS(cache_file)
            assign(cache_key, dt_cached, envir = .nav_memory_cache)
            dt_cached
        } else {
            NULL
        }
    })
}

NAVALL_URL        <- 'https://portal.amfiindia.com/spages/NAVAll.txt'
NAVALL_CACHE_PATH <- './navall_categorized.rds'
NAVALL_MAX_HOURS  <- 20L

parse_navall <- function(lines) {
    header_pattern <- '^(Open|Close|Interval)\\s+Ended\\s+Schemes\\s*\\((.+)\\)\\s*$'

    cur_type        <- NA_character_
    cur_category    <- NA_character_
    cur_subcategory <- NA_character_
    cur_amc         <- NA_character_
    rows <- vector('list', length(lines))
    n <- 0L

    for (line in lines) {
        trimmed <- trimws(line)
        if (nchar(trimmed) == 0) next

        header_m <- regmatches(trimmed, regexec(header_pattern, trimmed))[[1]]
        if (length(header_m) > 0) {
            cur_type <- paste(header_m[2], 'Ended Schemes')
            inner <- trimws(header_m[3])
            parts <- str_split(inner, '\\s+-\\s+', n = 2)[[1]]
            cur_category <- trimws(parts[1])
            cur_subcategory <- if (length(parts) > 1) trimws(parts[2]) else NA_character_
            next
        }

        if (!grepl(';', trimmed)) {
            cur_amc <- trimmed
            next
        }

        fields <- str_split(trimmed, ';')[[1]]
        if (length(fields) < 6) next
        if (trimws(fields[1]) == 'Scheme Code') next

        n <- n + 1L
        rows[[n]] <- data.table(
            SchemeCode    = trimws(fields[1]),
            ISIN_Growth   = trimws(fields[2]),
            ISIN_Reinvest = trimws(fields[3]),
            SchemeName    = trimws(fields[4]),
            SchemeType    = cur_type,
            Category      = cur_category,
            SubCategory   = cur_subcategory,
            AMC           = cur_amc
        )
    }
    rbindlist(rows[seq_len(n)], fill = TRUE)
}

get_navall_categorized <- function(force_refresh = FALSE) {
    if (!dir.exists(NAV_CACHE_DIR)) dir.create(NAV_CACHE_DIR, recursive = TRUE)

    needs_download <- force_refresh || !file.exists(NAVALL_CACHE_PATH) ||
        as.numeric(Sys.time() - file.mtime(NAVALL_CACHE_PATH), units = 'hours') > NAVALL_MAX_HOURS

    if (needs_download) {
        tryCatch({
            lines <- readLines(NAVALL_URL, warn = FALSE)
            dt <- parse_navall(lines)
            saveRDS(dt, NAVALL_CACHE_PATH)
        }, error = function(e) {
            if (!file.exists(NAVALL_CACHE_PATH))
                stop('Could not download NAVAll.txt: ', conditionMessage(e))
            warning('Could not refresh NAVAll.txt, using stale cache: ', conditionMessage(e))
        })
    }
    readRDS(NAVALL_CACHE_PATH)
}

build_fund_category_map <- function(funds, fund_scheme_map, dt_navall) {
    dt_isin_long <- rbindlist(list(
        dt_navall[!is.na(ISIN_Growth) & ISIN_Growth != '-',
                  .(ISIN = toupper(ISIN_Growth), SchemeType, Category, SubCategory)],
        dt_navall[!is.na(ISIN_Reinvest) & ISIN_Reinvest != '-',
                  .(ISIN = toupper(ISIN_Reinvest), SchemeType, Category, SubCategory)]
    ))
    dt_isin_long <- unique(dt_isin_long, by = 'ISIN')
    setkey(dt_isin_long, ISIN)

    dt_code_lookup <- unique(dt_navall[, .(SchemeCode = as.character(SchemeCode),
                                           SchemeType, Category, SubCategory)],
                             by = 'SchemeCode')
    setkey(dt_code_lookup, SchemeCode)

    rows <- lapply(funds, function(f) {
        isin_m <- regmatches(f, regexpr('INF[A-Z0-9]{9}', f, ignore.case = TRUE))
        cat_row <- NULL
        if (length(isin_m) == 1L && nchar(isin_m) > 0) {
            cat_row <- dt_isin_long[ISIN == toupper(isin_m)]
        }
        if (is.null(cat_row) || nrow(cat_row) == 0) {
            scheme_code <- fund_scheme_map[[f]]
            if (!is.null(scheme_code) && !is.na(scheme_code)) {
                cat_row <- dt_code_lookup[SchemeCode == as.character(scheme_code)]
            }
        }
        if (is.null(cat_row) || nrow(cat_row) == 0) {
            data.table(Fund = f, SchemeType = NA_character_,
                       Category = NA_character_, SubCategory = NA_character_)
        } else {
            data.table(Fund = f, SchemeType = cat_row$SchemeType[1],
                       Category = cat_row$Category[1],
                       SubCategory = cat_row$SubCategory[1])
        }
    })
    rbindlist(rows)
}

ISIN_DB_DIR      <- './isin_db'
ISIN_DB_PATH     <- file.path(ISIN_DB_DIR, 'isin.db')
ISIN_DB_URL      <- 'https://casparser.atomcoder.com/isin.db'
ISIN_DB_MAX_DAYS <- 30L

ensure_isin_db <- function() {
    if (!dir.exists(ISIN_DB_DIR)) dir.create(ISIN_DB_DIR, recursive = TRUE)

    needs_download <- !file.exists(ISIN_DB_PATH) ||
        as.numeric(Sys.time() - file.mtime(ISIN_DB_PATH), units = 'days') > ISIN_DB_MAX_DAYS

    if (needs_download) {
        tryCatch({
            resp <- httr::GET(ISIN_DB_URL,
                httr::add_headers(
                    `User-Agent` = 'casparser-isin 2025.3.1',
                    `X-origin-casparser` = 'true'
                ))
            if (httr::status_code(resp) == 200L) {
                writeBin(httr::content(resp, 'raw'), ISIN_DB_PATH)
            } else if (!file.exists(ISIN_DB_PATH)) {
                warning('Could not download isin.db: HTTP ', httr::status_code(resp))
            }
        }, error = function(e) {
            if (!file.exists(ISIN_DB_PATH))
                warning('Could not download isin.db: ', conditionMessage(e))
        })
    }
    invisible(file.exists(ISIN_DB_PATH))
}

isin_to_amfi <- function(isin) {
    if (is.na(isin) || nchar(trimws(isin)) == 0) return(NA_character_)
    isin <- toupper(isin)
    if (exists(isin, envir = .isin_memory_cache, inherits = FALSE)) {
        return(get(isin, envir = .isin_memory_cache))
    }
    if (!ensure_isin_db()) return(NA_character_)
    out <- tryCatch({
        con <- DBI::dbConnect(RSQLite::SQLite(), ISIN_DB_PATH, flags = RSQLite::SQLITE_RO)
        on.exit(DBI::dbDisconnect(con), add = TRUE)
        res <- DBI::dbGetQuery(con,
            'SELECT amfi_code FROM scheme WHERE isin = ? LIMIT 1',
            params = list(isin))
        if (nrow(res) == 0 || is.na(res$amfi_code[1])) NA_character_
        else as.character(res$amfi_code[1])
    }, error = function(e) NA_character_)
    assign(isin, out, envir = .isin_memory_cache)
    out
}

extract_fund_name <- function(name) {
    name <- gsub('^[A-Z0-9]+\\s*-\\s*', '', name)
    name <- gsub('\\s*-?\\s*ISIN:.*$', '', name, ignore.case = TRUE)
    name <- gsub('\\(formerly[^)]*\\)', '', name, ignore.case = TRUE)
    name <- gsub('\\(Erstwhile[^)]*\\)', '', name, ignore.case = TRUE)
    name <- gsub('\\(Non.Demat\\)', '', name, ignore.case = TRUE)
    name <- gsub('\\(Advisor:.*', '', name, ignore.case = TRUE)
    name <- gsub('\\s*Registrar\\s*:.*', '', name, ignore.case = TRUE)
    trimws(name)
}

normalize_fund_name <- function(name) {
    name <- tolower(name)
    name <- gsub('-', ' ', name)
    name <- gsub('[^a-z0-9 ]', ' ', name)
    name <- gsub('\\s+', ' ', trimws(name))
    name <- gsub('\\b([a-z]) ([a-z])\\b', '\\1\\2', name)
    name <- gsub('\\b([a-z]) ([a-z])\\b', '\\1\\2', name)
    name <- gsub('([a-z])([0-9])', '\\1 \\2', name)
    name <- gsub('([0-9])([a-z])', '\\1 \\2', name)
    name <- gsub('\\bfund of funds?\\b', 'fof', name)
    gsub('\\s+', ' ', trimws(name))
}

prepare_scheme_lookup <- function(dt_mfs_all) {
    norm_names <- normalize_fund_name(as.character(dt_mfs_all$schemeName))
    words <- strsplit(norm_names, ' ', fixed = TRUE)
    words <- lapply(words, unique)
    list(
        norm_names = norm_names,
        codes      = as.character(dt_mfs_all$schemeCode),
        words      = words
    )
}

get_prepared_scheme_lookup <- function(dt_mfs_all, cache_path = './nav_cache/scheme_lookup.rds',
                                       source_path = './mf_codes.RData') {
    if (!dir.exists(dirname(cache_path))) dir.create(dirname(cache_path), recursive = TRUE)
    source_mtime <- if (file.exists(source_path)) file.mtime(source_path) else NA

    if (file.exists(cache_path)) {
        cached <- tryCatch(readRDS(cache_path), error = function(e) NULL)
        if (!is.null(cached) &&
            identical(cached$source_mtime, source_mtime) &&
            identical(cached$n_schemes, nrow(dt_mfs_all))) {
            return(cached$lookup)
        }
    }

    lookup <- prepare_scheme_lookup(dt_mfs_all)
    saveRDS(list(source_mtime = source_mtime,
                 n_schemes = nrow(dt_mfs_all),
                 lookup = lookup),
            cache_path)
    lookup
}

match_fund_to_scheme <- function(cas_fund_name, norm_mfs, codes = NULL) {
    if (is.list(norm_mfs) && all(c('norm_names', 'codes', 'words') %in% names(norm_mfs))) {
        lookup <- norm_mfs
    } else {
        lookup <- list(
            norm_names = norm_mfs,
            codes = as.character(codes),
            words = lapply(strsplit(norm_mfs, ' ', fixed = TRUE), unique)
        )
    }

    cleaned   <- extract_fund_name(cas_fund_name)
    norm_cas  <- normalize_fund_name(cleaned)
    cas_words <- unique(strsplit(norm_cas, ' ', fixed = TRUE)[[1]])

    mf_stopwords <- c('direct', 'regular', 'plan', 'growth', 'idcw', 'fund',
                      'scheme', 'option', 'dividend', 'bonus', 'monthly',
                      'quarterly', 'annual', 'reinvest', 'payout', 'weekly',
                      'daily', 'of', 'the', 'and', 'fof')

    overlap_score <- function(mw) {
        inter <- intersect(cas_words, mw)
        n_inter <- length(inter)
        if (n_inter < 4L) return(0)
        fwd <- n_inter / length(mw)
        if (fwd >= 0.8) return(fwd)
        rev <- n_inter / length(cas_words)
        if (rev >= 0.9 && any(!inter %in% mf_stopwords)) return(rev)
        0
    }

    isin_m <- regmatches(cas_fund_name,
                         regexpr('INF[A-Z0-9]{9}', cas_fund_name, ignore.case = TRUE))
    if (length(isin_m) == 1L && nchar(isin_m) > 0) {
        amfi <- isin_to_amfi(isin_m)
        if (!is.na(amfi) && amfi %in% lookup$codes) {
            resolved_name <- lookup$norm_names[which(lookup$codes == amfi)[1]]
            if (!is.na(resolved_name)) {
                rw <- unique(strsplit(resolved_name, ' ', fixed = TRUE)[[1]])
                n_brand <- length(setdiff(intersect(cas_words, rw), mf_stopwords))
                if (n_brand >= 2L) return(amfi)
            } else {
                return(amfi)
            }
        }
    }

    exact_idx <- which(lookup$norm_names == norm_cas)
    if (length(exact_idx) > 0) return(lookup$codes[exact_idx[1]])

    approx_idx <- agrep(norm_cas, lookup$norm_names, ignore.case = TRUE, max.distance = 0.3)
    if (length(approx_idx) > 0) {
        ap_scores <- vapply(lookup$words[approx_idx], overlap_score, numeric(1))
        best_pos <- which.max(ap_scores)
        if (length(best_pos) > 0 && ap_scores[best_pos] > 0)
            return(lookup$codes[approx_idx[best_pos]])
    }

    ov_scores <- vapply(lookup$words, overlap_score, numeric(1))
    best_idx <- which.max(ov_scores)
    if (length(best_idx) > 0 && ov_scores[best_idx] > 0)
        return(lookup$codes[best_idx])

    query <- paste(head(strsplit(cleaned, '\\s+')[[1]], 4), collapse = ' ')
    search_url <- paste0('https://api.mfapi.in/mf/search?q=', URLencode(query))
    tryCatch({
        results <- fromJSON(paste(readLines(search_url, warn = FALSE), collapse = ''))
        if (length(results) == 0) return(NA_character_)
        api_names <- vapply(results, function(r) normalize_fund_name(r$schemeName), character(1))
        api_codes <- vapply(results, function(r) as.character(r$schemeCode), character(1))
        api_words <- lapply(strsplit(api_names, ' ', fixed = TRUE), unique)
        api_scores <- vapply(api_words, overlap_score, numeric(1))
        api_best <- which.max(api_scores)
        if (length(api_best) > 0 && api_scores[api_best] > 0)
            return(api_codes[api_best])
    }, error = function(e) NULL)

    NA_character_
}

explain_fund_match <- function(cas_fund_name, lookup, scheme_code = NA_character_) {
    cleaned <- extract_fund_name(cas_fund_name)
    norm_cas <- normalize_fund_name(cleaned)
    cas_words <- unique(strsplit(norm_cas, ' ', fixed = TRUE)[[1]])
    stopwords <- c('direct', 'regular', 'plan', 'growth', 'idcw', 'fund',
                   'scheme', 'option', 'dividend', 'bonus', 'monthly',
                   'quarterly', 'annual', 'reinvest', 'payout', 'weekly',
                   'daily', 'of', 'the', 'and', 'fof')

    score_words <- function(mw) {
        inter <- intersect(cas_words, mw)
        if (length(inter) == 0) return(0)
        max(length(inter) / length(mw), length(inter) / length(cas_words))
    }

    method <- 'No match'
    score <- NA_real_
    matched_name <- NA_character_
    isin <- regmatches(cas_fund_name,
                       regexpr('INF[A-Z0-9]{9}', cas_fund_name, ignore.case = TRUE))
    if (length(isin) != 1L || nchar(isin) == 0) isin <- NA_character_

    if (!is.na(scheme_code)) {
        idx <- which(lookup$codes == as.character(scheme_code))[1]
        if (!is.na(idx)) {
            matched_name <- lookup$norm_names[idx]
            exact_idx <- which(lookup$norm_names == norm_cas)
            if (length(exact_idx) > 0 && lookup$codes[exact_idx[1]] == as.character(scheme_code)) {
                method <- 'Exact normalized name'
                score <- 1
            } else if (!is.na(isin)) {
                amfi <- isin_to_amfi(isin)
                if (!is.na(amfi) && amfi == as.character(scheme_code)) {
                    method <- 'ISIN'
                    score <- 1
                }
            }
            if (method == 'No match') {
                mw <- lookup$words[[idx]]
                inter <- intersect(cas_words, mw)
                brand_words <- setdiff(inter, stopwords)
                score <- score_words(mw)
                method <- if (score >= 0.9 && length(brand_words) > 0) {
                    'High-confidence word overlap'
                } else if (score >= 0.6) {
                    'Approximate word overlap'
                } else {
                    'Fallback match'
                }
            }
        }
    }

    data.table(
        Fund = extract_fund_name(cas_fund_name),
        SchemeCode = ifelse(is.na(scheme_code), NA_character_, as.character(scheme_code)),
        Method = method,
        Confidence = ifelse(is.na(score), NA_real_, round(score, 3)),
        ISIN = isin,
        MatchedName = matched_name
    )
}

explain_fund_matches <- function(fund_scheme_map, lookup) {
    rows <- lapply(names(fund_scheme_map), function(f) {
        explain_fund_match(f, lookup, fund_scheme_map[[f]])
    })
    rbindlist(rows, fill = TRUE)
}

pre_warm_navs <- function(fund_scheme_map, required_date = Sys.Date(),
                          progress_fn = NULL) {
    if (!dir.exists(NAV_CACHE_DIR)) dir.create(NAV_CACHE_DIR, recursive = TRUE)

    rows <- lapply(names(fund_scheme_map), function(f) {
        if (!is.null(progress_fn)) progress_fn(f)

        scheme_code <- fund_scheme_map[[f]]
        clean_name  <- extract_fund_name(f)

        if (is.na(scheme_code)) {
            return(data.table(Fund = clean_name, SchemeCode = NA_character_,
                              Source = 'No match', NAVsUpTo = NA_character_))
        }

        cache_file <- file.path(NAV_CACHE_DIR, paste0(scheme_code, '.rds'))
        cache_key <- paste0('scheme_', scheme_code)

        if (file.exists(cache_file)) {
            dt_cached <- readRDS(cache_file)
            assign(cache_key, dt_cached, envir = .nav_memory_cache)
            max_cached <- max(dt_cached$date)
            if (max_cached >= required_date - 7) {
                return(data.table(Fund = clean_name, SchemeCode = scheme_code,
                                  Source = 'Cache',
                                  NAVsUpTo = as.character(max_cached)))
            }
            source_label <- 'API - refreshed'
        } else {
            source_label <- 'API - new'
        }

        tryCatch({
            dt_navs <- get_navs(scheme_code)
            saveRDS(dt_navs, cache_file)
            assign(cache_key, dt_navs, envir = .nav_memory_cache)
            data.table(Fund = clean_name, SchemeCode = scheme_code,
                       Source = source_label,
                       NAVsUpTo = as.character(max(dt_navs$date)))
        }, error = function(e) {
            if (file.exists(cache_file)) {
                dt_fb <- readRDS(cache_file)
                assign(cache_key, dt_fb, envir = .nav_memory_cache)
                data.table(Fund = clean_name, SchemeCode = scheme_code,
                           Source = 'Cache (stale, fetch failed)',
                           NAVsUpTo = as.character(max(dt_fb$date)))
            } else {
                data.table(Fund = clean_name, SchemeCode = scheme_code,
                           Source = 'Fetch failed', NAVsUpTo = NA_character_)
            }
        })
    })
    rbindlist(rows, fill = TRUE)
}
