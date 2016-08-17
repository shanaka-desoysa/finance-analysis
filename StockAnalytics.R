stockDataJson <- function(symbol) {
    # http://www.google.com/finance/info?infotype=infoquoteall&q=NASDAQ:AAPL
    # http://finance.google.com/finance/info?client=ig&q=AAPL
    # http://www.google.com/finance/info?q=NASDAQ:AAPL
    require(RJSONIO)
    quotes <-
        readLines(paste(
            'http://www.google.com/finance/info?q=NASDAQ:',
            symbol,
            sep = ""
        ))
    quotes <- quotes[2:length(quotes)]
    quotes[1] <- '['
    quotes <- paste(quotes, collapse = '')
    json_file <- fromJSON(quotes, simplify = TRUE)
    json_file
}
