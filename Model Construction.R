model_construction <- function(user.quote){
    
    T.ind <- function(quotes, tgt.margin = 0.025, n.days = 10) {
        v <- apply(HLC(quotes), 1, mean) # HLC()-subset High, Low and Close. Apply - calculate avg of those three by row
        r <- matrix(NA, ncol = n.days, nrow = nrow(quotes)) # nrow * n.days matrix with NA
        for (x in 1:n.days) r[, x] <- Next(Delt(v, k = x), x) # Delt() - calculate the k-period percent diff between n and n+x. 
        x <- apply(r, 1, function(x) sum(x[x > tgt.margin | x < -tgt.margin])) # sum % change larger than 0.025 in r, if >0 good, <0 bad.
        if (is.xts(quotes))
            xts(x, time(quotes))
        else x
    }
    
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
    
    quote.data$Volume <- quote.data$Volume + 10^-15
    data.model <- specifyModel(T.ind(quote.data) ~ Delt(Cl(quote.data),k=1:10) +
                                   myATR(quote.data) + mySMI(quote.data) + myADX(quote.data) + myAroon(quote.data) +
                                   myBB(quote.data) + myChaikinVol(quote.data) + myCLV(quote.data) +
                                   CMO(Cl(quote.data)) + EMA(Delt(Cl(quote.data))) + myEMV(quote.data) +
                                   myVolat(quote.data) + myMACD(quote.data) + myMFI(quote.data) + RSI(Cl(quote.data)) +
                                   mySAR(quote.data) + runMean(Cl(quote.data)) + runSD(Cl(quote.data)))
    return(data.model)
    
}