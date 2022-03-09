closing_line_pattern <-
    'Closing\\s+Unit\\s+Balance:\\s+([0-9,.]+).*INR\\s+([0-9,.]+).*Valuation\\s+on\\s+(\\d{2}-[A-Za-z]{3}-\\d{4}):\\s+INR\\s+([0-9,.]+)'
fund_name_pattern <- "^[A-Z0-9]+-[A-Za-z&]+\\s"
fund_advisor_pattern <- '(.+)\\(Advisor:\\s+(.*)\\)','\\1:::\\2'
transaction_pattern <- '(\\d{2}-[A-Za-z]{3}-\\d{4})(.*)((\\s+[(),.0-9]+){4})', '\\1xx\\2xx\\3'
idcw_pattern <- '(\\d{2}-[A-Za-z]{3}-\\d{4})\\s+\\*+(.*)\\*+(\\s+)([,.0-9]+)'
folio_pan_pattern <- '\\s+PAN:\\s+'
