setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Predicting-Stock-Market-Returns\\final model")

# getting the quotes for the testing period
user.quote <- "TAP.AX"
library(xts)
library(tseries)
library(quantmod)
quote.data <- as.xts(get.hist.quote(user.quote,start="1970-01-02",quote=c("Open", "High", "Low", "Close","Volume","AdjClose")))
colnames(quote.data) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
quote.data[c(1,nrow(quote.data))]
nrow(quote.data)

# Train and test periods
start <- 1
len.tr <- as.integer(0.8*nrow(quote.data))
len.ts <- nrow(quote.data)-len.tr
tr <- start:(start+len.tr-1)
ts <- (start+len.tr):(start+len.tr+len.ts-1)

# creating T.indicator 
T.ind <- function(quotes, tgt.margin = 0.025, n.days = 10) {
    v <- apply(HLC(quotes), 1, mean) # HLC()-subset High, Low and Close. Apply - calculate avg of those three by row
    r <- matrix(NA, ncol = n.days, nrow = nrow(quotes)) # nrow * n.days matrix with NA
    for (x in 1:n.days) r[, x] <- Next(Delt(v, k = x), x) # Delt() - calculate the k-period percent diff between n and n+x. 
    x <- apply(r, 1, function(x) sum(x[x > tgt.margin | x < -tgt.margin])) # sum % change larger than 0.025 in r, if >0 good, <0 bad.
    if (is.xts(quotes))
        xts(x, time(quotes))
    else x
}

# define financial indicators of stock
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

# define the dependent and independent variables of the model
quote.data$Volume <- quote.data$Volume + 10^-15
data.model <- specifyModel(T.ind(quote.data) ~ Delt(Cl(quote.data),k=1:10) +
                               myATR(quote.data) + mySMI(quote.data) + myADX(quote.data) + myAroon(quote.data) +
                               myBB(quote.data) + myChaikinVol(quote.data) + myCLV(quote.data) +
                               CMO(Cl(quote.data)) + EMA(Delt(Cl(quote.data))) + myEMV(quote.data) +
                               myVolat(quote.data) + myMACD(quote.data) + myMFI(quote.data) + RSI(Cl(quote.data)) +
                               mySAR(quote.data) + runMean(Cl(quote.data)) + runSD(Cl(quote.data)))

# transfer training and testing stock data into data.frame
Tdata.train <- as.data.frame(modelData(data.model, data.window=c("1996-09-30","2011-07-05")))
Tdata.eval <- na.omit(as.data.frame(modelData(data.model, data.window=c("2011-07-06","2014-08-18"))))
Tform <- as.formula('T.ind.quote.data ~ .')

# Subsetting the train data
date <- rownames(Tdata.train[nrow(Tdata.train),])
market <- quote.data[paste(date,'/',sep='')][2:len.ts]

# learning the model and obtaining its signal predictions
set.seed(1234)
library(nnet)
library(DMwR)
source("Monte Carlo.R")
grow

norm.data <- scale(Tdata.train)
nn <- nnet(Tform, norm.data[1:1000, ], size = 10, decay = 0.01,maxit = 1000, linout = T, trace = F)
norm.preds <- predict(nn, norm.data[1001:nrow(norm.data), ])
preds <- unscale(norm.preds, norm.data)
sigs.nn <- trading.signals(preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[1001:nrow(Tdata.train), "T.ind.quote.data"],0.1, -0.1)
sigs.PR(sigs.nn, true.sigs) 

# now using the simulated trader
source("Trading policy.R")
t1 <- trading.simulator(market,true.sigs,'policy.1',list(exp.prof=0.05,bet=0.2,hold.time=30))
t1
summary(t1)
tradingEvaluation(t1)
plot(t1, market, theme = "white", name = "ASX200")

source("Trading policy2.R")
t2 <- trading.simulator(market, true.sigs, "policy.2", list(exp.prof = 0.05, bet = 0.3))
summary(t2)
tradingEvaluation(t2)
plot(t2, market, theme = "white", name = "ASX200")
