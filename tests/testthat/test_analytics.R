test_that("XIRR handles normal annual return", {
    dt <- data.table(
        date = as.Date(c('2024-01-01', '2025-01-01')),
        amt = c(100, -110)
    )
    dt <- recalc_xirr_basis(dt)
    expect_equal(round(XIRR(dt), 3), 0.1)
})

test_that("fund summaries include switch-in cash flows for position returns", {
    dt <- data.table(
        date = as.Date(c('2018-06-15', '2020-12-10', '2026-07-03')),
        description = c('Switch In (Decimal Change)', 'Purchase (Continuous Offer)', 'Cur Value'),
        amt = c(300192.390, 149992.500, -683094.010),
        fund = 'Kotak Liquid Fund'
    )
    dt <- recalc_xirr_basis(dt)
    summary <- get_mf_summary(dt, folio_ord_num = 1)
    expect_equal(round(summary$Invested, 2), 450184.89)
    expect_equal(round(summary$XIRR, 3), 5.911)
})

test_that("external cash flows exclude switches for portfolio-level returns", {
    dt <- data.table(
        description = c('Switch In', 'Purchase', 'Switch Out', 'Cur Value'),
        amt = c(100, 50, -80, -90)
    )
    cf <- external_cashflows(dt)
    expect_equal(cf$amt, 50)
})

test_that("analytics hierarchy recalculates XIRR at each group level", {
    dt <- data.table(
        AMC = 'Kotak Mutual Fund',
        Category = 'Debt Scheme',
        SubCategory = 'Liquid Fund',
        Scheme = 'Kotak Liquid Fund Direct Plan Growth',
        folio = 'F1',
        date = as.Date(c('2018-06-15', '2020-12-10', '2026-07-03')),
        description = c(
            'Switch In (Decimal Change)',
            'Purchase (Continuous Offer)',
            'Cur Value'
        ),
        amt = c(300192.390, 149992.500, -683094.010)
    )

    hierarchy <- build_hierarchy_xirr_table(
        dt,
        c('AMC', 'Category', 'SubCategory', 'Scheme')
    )

    expect_equal(sort(unique(hierarchy$Level)), 1:4)
    expect_equal(hierarchy[Level == 1]$`Cur Value`, 683094.01)
    expect_equal(hierarchy[Level == 1]$Invested, 450184.89)
    expect_equal(hierarchy[Level == 1]$`XIRR%`, 5.911)
    expect_equal(hierarchy[Level == 4]$`XIRR%`, 5.911)

    dt[, Folio := folio]
    folio_hierarchy <- build_hierarchy_xirr_table(
        dt,
        c('AMC', 'Folio', 'Category', 'SubCategory', 'Scheme')
    )

    expect_equal(sort(unique(folio_hierarchy$Level)), 1:5)
    expect_equal(folio_hierarchy[Level == 2]$`XIRR%`, 5.911)
})

test_that("analytics hierarchy supports partial column selections", {
    dt <- data.table(
        AMC = 'Kotak Mutual Fund',
        Category = 'Debt Scheme',
        SubCategory = 'Liquid Fund',
        Scheme = 'Kotak Liquid Fund Direct Plan Growth',
        Folio = 'F1',
        folio = 'F1',
        date = as.Date(c('2024-01-01', '2026-01-01')),
        description = c('Purchase (Continuous Offer)', 'Cur Value'),
        amt = c(100000, -112000)
    )

    hierarchy <- build_hierarchy_xirr_table(dt, c('AMC', 'Category'))
    hidden_cols <- intersect(
        unique(c('Level', 'Path', 'AMC', 'Category', 'SubCategory', 'Scheme', 'Folio')),
        names(as.data.frame(hierarchy))
    )

    expect_setequal(hidden_cols, c('Level', 'Path', 'AMC', 'Category'))
    expect_false(any(c('SubCategory', 'Scheme', 'Folio') %in% hidden_cols))
})

test_that("diagnostics count unmatched funds and switches", {
    dt <- data.table(
        fund = c('Fund A', 'Fund A', 'Fund B'),
        description = c('Purchase', 'Switch In', 'Purchase'),
        date = as.Date('2024-01-01'),
        amt = c(1, 1, 1)
    )
    fsm <- list('Fund A' = '123', 'Fund B' = NA_character_)
    cat_map <- data.table(Fund = c('Fund A', 'Fund B'),
                          Category = c('Equity Scheme', NA_character_),
                          SubCategory = c('Large Cap Fund', NA_character_))
    nav_log <- data.table(Fund = c('Fund A', 'Fund B'),
                          Source = c('Cache', 'No match'))
    diag <- build_quality_diagnostics(dt, fsm, cat_map, nav_log)
    expect_equal(diag[Check == 'Unmatched NAV schemes']$Count, 1)
    expect_equal(diag[Check == 'Switch transactions']$Count, 1)
})
