setwd("C:\\Documents and Settings\\Macro\\Desktop\\Ivandata\\Predicting-Stock-Market-Returns")
library(tseries)
library(xts)

# As the function get.hist.quote() returns an object of class zoo, we have
# again used the function as.xts() to coerce it to xts.
    GSPC <- as.xts(get.hist.quote("^GSPC",start="1970-01-02",end="2009-09-15",
                                  quote=c("Open", "High", "Low", "Close","Volume","AdjClose")))
    GSPC[c(1,nrow(GSPC))]

# quantmod
    library(quantmod)
    getSymbols("^GSPC", from = "1970-01-01", to = "2009-09-15")
    colnames(GSPC) <- c("Open", "High", "Low", "Close", "Volume", "AdjClose")
    
    setSymbolLookup(IBM=list(name='IBM',src='yahoo'),
                    USDEUR=list(name='USD/EUR',src='oanda'))
    getSymbols(c('IBM','USDEUR'))
    head(IBM)
    head(USDEUR)
    # saveSymbolLookup()
    # loadSymbolLookup()

# indicator function
    T.ind <- function(quotes, tgt.margin = 0.025, n.days = 10) {
        v <- apply(HLC(quotes), 1, mean)
        r <- matrix(NA, ncol = n.days, nrow = NROW(quotes))
        for (x in 1:n.days) r[, x] <- Next(Delt(v, k = x), x)
        x <- apply(r, 1, function(x) sum(x[x > tgt.margin | x < -tgt.margin]))
        if (is.xts(quotes))
            xts(x, time(quotes))
        else x
    }

# newTA() can be used to create new
# plotting functions for indicators that we wish to include in candlestick graphs.
png("indicator.png")
candleChart(last(GSPC, "3 months"), theme = "white", TA = NULL)
avgPrice <- function(p) apply(HLC(p), 1, mean)
addAvgPrice <- newTA(FUN = avgPrice, col = 1, legend = "AvgPrice")
addT.ind <- newTA(FUN = T.ind, col = "red", legend = "tgtRet")
addAvgPrice(on = 1)
addT.ind()
dev.off()