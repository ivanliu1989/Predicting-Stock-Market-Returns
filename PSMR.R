setwd("C:\\Documents and Settings\\Macro\\Desktop\\Ivandata\\Predicting-Stock-Market-Returns")
library(xts)

## As the function get.hist.quote() returns an object of class zoo, we have
## again used the function as.xts() to coerce it to xts.
#     library(tseries)
#     TAP.AX <- as.xts(get.hist.quote("TAP.AX",start="1970-01-02", # end="2009-09-15",
#                                   quote=c("Open", "High", "Low", "Close","Volume","AdjClose")))
#     TAP.AX[c(1,nrow(GSPC))]

## quantmod
    library(quantmod)
#     getSymbols("TAP.AX", from = "1970-01-01", to = "2009-09-15")
    setSymbolLookup(TAP=list(name='TAP.AX',src='yahoo'),
                    IBM=list(name='IBM',src='yahoo'),
                    USDEUR=list(name='USD/EUR',src='oanda'))
    getSymbols(c('TAP.AX'))
    colnames(TAP.AX) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    head(TAP.AX)
#     saveSymbolLookup()
#     loadSymbolLookup()

## indicator function
    T.ind <- function(quotes, tgt.margin = 0.025, n.days = 10) {
        v <- apply(HLC(quotes), 1, mean) # HLC()-subset High, Low and Close. Apply - calculate avg of those three by row
        r <- matrix(NA, ncol = n.days, nrow = nrow(quotes)) # nrow * n.days matrix with NA
        for (x in 1:n.days) r[, x] <- Next(Delt(v, k = x), x) # Delt() - calculate the k-period percent diff between n and n+x. 
        x <- apply(r, 1, function(x) sum(x[x > tgt.margin | x < -tgt.margin])) # sum % change larger than 0.025 in r, if >0 good, <0 bad.
        if (is.xts(quotes))
            xts(x, time(quotes))
        else x
    }

# newTA() can be used to create new
# plotting functions for indicators that we wish to include in candlestick graphs.
    png("TAP.png")
    candleChart(last(TAP.AX, "3 months"), theme = "white", TA = NULL)
    avgPrice <- function(p) apply(HLC(p), 1, mean)
    addAvgPrice <- newTA(FUN = avgPrice, col = 1, legend = "AvgPrice")
    addT.ind <- newTA(FUN = T.ind, col = "red", legend = "tgtRet")
    addAvgPrice(on = 1)
    addT.ind()
    dev.off()

# a representative set of technical indicators, from those available in package TTR
    library(TTR)
    myATR <- function(x) ATR(HLC(x))[, "atr"]
    mySMI <- function(x) SMI(HLC(x))[, "SMI"]
    myADX <- function(x) ADX(HLC(x))[, "ADX"]
    myAroon <- function(x) aroon(x[, c("High", "Low")])$oscillator
    myBB <- function(x) BBands(HLC(x))[, "pctB"]
    myChaikinVol <- function(x) Delt(chaikinVolatility(x[, c("High", "Low")]))[, 1]
    myCLV <- function(x) EMA(CLV(HLC(x)))[, 1]
    myEMV <- function(x) EMV(x[, c("High", "Low")], x[, "Volume"])[,2]
    myMACD <- function(x) MACD(Cl(x))[, 2]
    myMFI <- function(x) MFI(x[, c("High", "Low", "Close")], x[, "Volume"])
    mySAR <- function(x) SAR(x[, c("High", "Close")])[, 1]
    myVolat <- function(x) volatility(OHLC(x), calc = "garman")[,1]

# Build a random forest using the data available for training:
    library(randomForest)
    TAP.AX$Volume <- TAP.AX$Volume+0.000000001
    # specifyModel - Create a single reusable model specification for subsequent buildModel calls. a quantmod object.
    # getModelData() to obtain a refresh of the object (data.model)
    data.model <- specifyModel(T.ind(TAP.AX) ~ Delt(Cl(TAP.AX),k=1:10) +
                                   myATR(TAP.AX) + mySMI(TAP.AX) + myADX(TAP.AX) + myAroon(TAP.AX) +
                                   myBB(TAP.AX) + myChaikinVol(TAP.AX) + myCLV(TAP.AX) +
                                   CMO(Cl(TAP.AX)) + EMA(Delt(Cl(TAP.AX))) + myEMV(TAP.AX) +
                                   myVolat(TAP.AX) + myMACD(TAP.AX) + myMFI(TAP.AX) + RSI(Cl(TAP.AX)) +
                                   mySAR(TAP.AX) + runMean(Cl(TAP.AX)) + runSD(Cl(TAP.AX)))
    set.seed(1234)
    # The function buildModel() uses the resulting model specication
    # and obtains a model with the corresponding data.
    rf <- buildModel(data.model,method='randomForest',
                     training.per=c(start(TAP.AX),index(TAP.AX["2012-12-31"])),
                     ntree=50, importance=T)

    # IBM
    ex.model <- specifyModel(T.ind(IBM) ~ Delt(Cl(IBM), k = 1:3))
    # obtain the data using the function modelData()
    # and use it with your favorite modeling function
    data <- modelData(ex.model, data.window = c("2009-01-01", "2009-08-10"))
    # m <- myFavouriteModellingTool(ex.model@model.formula, as.data.frame(data))

    # The generic function buildModel() returns the obtained model as a slot (fitted.model) of the quantmod object it produces
    varImpPlot(rf@fitted.model, type = 1)
    imp <- importance(rf@fitted.model, type = 1) # obtains the concrete scores for each variable
    rownames(imp)[which(imp > 10)]
    
    # signal = sell(T < -0.1), hold, buy(T > -0.1), trading.signals() in book package

    Tdata.train <- as.data.frame(modelData(data.model, data.window=c('1970-01-02','2012-12-31')))
    Tdata.eval <- na.omit(as.data.frame(modelData(data.model, data.window=c('2013-01-01','2014-08-18'))))
    Tform <- as.formula('T.ind.TAP.AX ~ .')




