setwd("C:\\Documents and Settings\\Macro\\Desktop\\Ivandata\\Predicting-Stock-Market-Returns")
# library(tseries)
# ibm <- get.hist.quote("IBM",start="1970-01-02",end="2014-08-17", quote=c("Open", "High", "Low", "Close","Volume"))
# plot(ibm[,c('Close','Volume')],main="IBM stock")
# 
# ts(rnorm(25), frequency = 4, start = c(1959, 2))
# ibm[c(1,nrow(ibm)),]
# 
# h.returns <- function(x,h=1) {
#     diff(x,lag=h)/x[1:(length(x)-h)]
# }
# h.returns(c(45,23,4,56,-45,3),h=2)

library(xts)
x1 <- xts(rnorm(100),seq(as.POSIXct("2000-01-01"), len = 100, by = "day"))
x1[1:5]
x2 <- xts(rnorm(100),seq(as.POSIXct("2000-01-01 13:00"),len = 100,by="min"))
x2[1:4]
x3 <- xts(rnorm(3), as.Date(c("2005-01-01", "2005-01-10", "1005-01-12")))
x3
