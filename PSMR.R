setwd("C:\\Documents and Settings\\Macro\\Desktop\\Ivandata\\Predicting-Stock-Market-Returns")
library(tseries)
t.ibm <- get.hist.quote("IBM",start="1970-01-02",end="2002-05-17", quote=c("Open", "High", "Low", "Close","Volume"))