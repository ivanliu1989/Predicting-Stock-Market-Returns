# Train and test periods
start <- 1
len.tr <- 1000
len.ts <- 500
tr <- start:(start+len.tr-1)
ts <- (start+len.tr):(start+len.tr+len.ts-1)

# getting the quotes for the testing period
library(quantmod)
setSymbolLookup(IBM=list(name='IBM',src='yahoo'))
getSymbols(c('IBM'))
colnames(IBM) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
date <- rownames(Tdata.train[start+len.tr,])
market <- IBM[paste(date,'/',sep='')][1:len.ts]

# learning the model and obtaining its signal predictions
library(e1071)
s <- svm(Tform,Tdata.train[tr,],cost=10,gamma=0.01)
p <- predict(s,Tdata.train[ts,])
sig <- trading.signals(p,0.1,-0.1)
# now using the simulated trader
source("Trading policy.R")
t1 <- trading.simulator(market,sig,'policy.1',list(exp.prof=0.05,bet=0.2,hold.time=30))
t1
summary(t1)
tradingEvaluation(t1)
plot(t1, market, theme = "white", name = "SP500")

source("Trading policy2.R")
t2 <- trading.simulator(market, sig, "policy.2", list(exp.prof = 0.05, bet = 0.3))
summary(t2)
tradingEvaluation(t2)
plot(t2, market, theme = "white", name = "SP500")
