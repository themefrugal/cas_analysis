test_that("scheme lookup matches exact normalized names", {
    lookup <- prepare_scheme_lookup(data.table(
        schemeName = 'Alpha Growth Fund Direct Plan Growth',
        schemeCode = 123
    ))
    expect_identical(
        match_fund_to_scheme('Alpha Growth Fund Direct Plan Growth', lookup),
        '123'
    )
})

test_that("match explanation reports exact normalized match", {
    lookup <- prepare_scheme_lookup(data.table(
        schemeName = 'Alpha Growth Fund Direct Plan Growth',
        schemeCode = 123
    ))
    exp <- explain_fund_match('Alpha Growth Fund Direct Plan Growth', lookup, '123')
    expect_equal(exp$Method, 'Exact normalized name')
    expect_equal(exp$Confidence, 1)
})

test_that("NAV cache reads local RDS without fetching", {
    old_dir <- NAV_CACHE_DIR
    tmp <- tempfile('nav-cache-')
    dir.create(tmp)
    NAV_CACHE_DIR <<- tmp
    on.exit({
        NAV_CACHE_DIR <<- old_dir
        rm(list = ls(envir = .nav_memory_cache), envir = .nav_memory_cache)
    }, add = TRUE)

    saveRDS(data.table(date = as.Date(c('2024-01-01', '2024-01-02')),
                       nav = c(10, 10.1)),
            file.path(tmp, '123.rds'))
    dt <- get_cached_navs('123', required_date = as.Date('2024-01-02'))
    expect_equal(nrow(dt), 2)
    expect_equal(dt$nav[2], 10.1)
})
