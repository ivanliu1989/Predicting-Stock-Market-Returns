setwd("C:\\Documents and Settings\\Macro\\Desktop\\Ivandata\\Predicting-Stock-Market-Returns")
library(tseries)
library(xts)

ibm <- get.hist.quote("IBM",start="1970-01-02",end="2014-08-17", quote=c("Open", "High", "Low", "Close","Volume"))
plot(ibm[,c('Close','Volume')],main="IBM stock")

ts(rnorm(25), frequency = 4, start = c(1959, 2))
ibm[c(1,nrow(ibm)),]

h.returns <- function(x,h=1) {
    diff(x,lag=h)/x[1:(length(x)-h)]
}
h.returns(c(45,23,4,56,-45,3),h=2)

