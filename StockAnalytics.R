stockDataJson <- function(symbols) {
    # http://www.google.com/finance/info?infotype=infoquoteall&q=NASDAQ:AAPL
    # http://finance.google.com/finance/info?client=ig&q=AAPL
    # http://www.google.com/finance/info?q=NASDAQ:PIH,NASDAQ:FLWS,NASDAQ:FCCY
    require(RJSONIO)
    quotes <-
        readLines(paste('http://www.google.com/finance/info?q=',
                        symbols,
                        sep = ""))
    quotes <- quotes[2:length(quotes)]
    quotes[1] <- '['
    quotes <- paste(quotes, collapse = '')
    json_file <- fromJSON(quotes, simplify = TRUE)
    json_file <- t(as.data.frame(json_file))
    rownames(json_file) <- NULL

    json_file <- as.data.frame(json_file)
}

getCommaSeperatedListOfSymbols <-
    function(exchange, take = as.integer(5)) {
        if (!is.numeric(take)) {
            stop("Parameter take must be an integer.")
        }
        csvOut <- downloadStockSymbols(exchange)
        csvOut <- trimws(csvOut$Symbol)
        csvOut <- paste("NASDAQ:", csvOut, sep = "")

        #  Split by number of items to take
        records <- length(csvOut)
        print(paste("Total number of symbols:", records))

        loIdx <- 1
        hiIdx <- loIdx + take - 1

        output <- vector()
        while (hiIdx <= records) {
            # print(paste("Concat From", loIdx, hiIdx))
            concat <- csvOut[loIdx:hiIdx]
            concat <- paste(concat, collapse = ",")
            # print(concat)
            output <- c(output, concat)
            loIdx <- (loIdx + take)
            hiIdx <- loIdx + take - 1
            if ((loIdx < records) & ( hiIdx > records)) {
                hiIdx <- records
            }
        }
        output
    }

downloadStockSymbols <- function(exchange) {
    dataFolder <- "./data"
    # nasdaqUrl <- "http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=nasdaq&render=download"
    # nyseUrl <- "http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=nyse&render=download"
    # amexUrl <- "http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=amex&render=download"
    dataFileUrl <-
        "http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=EXCNAME&render=download"
    dataFile <-
        paste(dataFolder, "/EXCNAME-companylist.csv", sep = "")

    # Create data folder
    if (!file.exists(dataFolder)) {
        dir.create(dataFolder)
    }
    # Download data file
    dataFileUrl <- sub("EXCNAME", exchange, dataFileUrl)
    dataFile <- sub("EXCNAME", exchange, dataFile)

    # print(dataFileUrl)
    if (!file.exists(dataFile)) {
        download.file(dataFileUrl, destfile = dataFile)
    }

    if (!file.exists(dataFile)) {
        stop("Data file does not exist.")
    }

    csvData <- read.csv(dataFile, strip.white = T)
}

x <- getCommaSeperatedListOfSymbols("nasdaq", take = 90)
result <- lapply(x, stockDataJson)
result <- do.call(rbind, lapply(result, data.frame, stringsAsFactors=FALSE))
write.csv(result, file = "./data/output.csv", row.names = F)

# x <- print(stockDataJson(x))
# stc <- stockDataJson(x)
# write.csv(stc, file = "./data/output.csv", row.names = F)
