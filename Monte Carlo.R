################################################################################
MC.svmR <- function(form, train, test, b.t = 0.1, s.t = -0.1,...) {
    require(e1071)
    t <- svm(form, train, ...)
    p <- predict(t, test)
    trading.signals(p, b.t, s.t)
}
MC.svmC <- function(form, train, test, b.t = 0.1, s.t = -0.1,...) {
    require(e1071)
    tgtName <- all.vars(form)[1]
    train[, tgtName] <- trading.signals(train[, tgtName],b.t, s.t)
    t <- svm(form, train, ...)
    p <- predict(t, test)
    factor(p, levels = c("s", "h", "b"))
}
# Best model
MC.nnetR <- function(form, train, test, b.t = 0.1, s.t = -0.1,...) {
    require(nnet)
    t <- nnet(form, train, ...)
    p <- predict(t, test)
    trading.signals(p, b.t, s.t)
}
MC.nnetC <- function(form, train, test, b.t = 0.1, s.t = -0.1,...) {
    require(nnet)
    tgtName <- all.vars(form)[1]
    train[, tgtName] <- trading.signals(train[, tgtName],b.t, s.t)
    t <- nnet(form, train, ...)
    p <- predict(t, test, type = "class")
    factor(p, levels = c("s", "h", "b"))
}
MC.earth <- function(form, train, test, b.t = 0.1, s.t = -0.1,...) {
    require(earth)
    t <- earth(form, train, ...)
    p <- predict(t, test)
    trading.signals(p, b.t, s.t)
}
################################################################################
single <- function(form, train, test, learner, policy.func,...) {
    p <- do.call(paste("MC", learner, sep = "."), list(form,train, test, ...))
    eval.stats(form, train, test, p, policy.func = policy.func)
}
slide <- function(form, train, test, learner, relearn.step,policy.func, ...) {
    real.learner <- learner(paste("MC", learner, sep = "."),pars = list(...))
    p <- slidingWindowTest(real.learner, form, train, test,relearn.step)
    p <- factor(p, levels = 1:3, labels = c("s", "h", "b"))
    eval.stats(form, train, test, p, policy.func = policy.func)
}
grow <- function(form, train, test, learner, relearn.step,policy.func, ...) {
    real.learner <- learner(paste("MC", learner, sep = "."),pars = list(...))
    p <- growingWindowTest(real.learner, form, train, test,relearn.step)
    p <- factor(p, levels = 1:3, labels = c("s", "h", "b"))
    eval.stats(form, train, test, p, policy.func = policy.func)
}
################################################################################
eval.stats <- function(form,train,test,preds,b.t=0.1,s.t=-0.1,...) {
    # Signals evaluation
    tgtName <- all.vars(form)[1]
    test[,tgtName] <- trading.signals(test[,tgtName],b.t,s.t)
    st <- sigs.PR(preds,test[,tgtName])
    dim(st) <- NULL
    names(st) <- paste(rep(c('prec','rec'),each=3),c('s','b','sb'),sep='.')
    # Trading evaluation
    date <- rownames(test)[1]
    market <- GSPC[paste(date,"/",sep='')][1:length(preds),]
    trade.res <- trading.simulator(market,preds,...)
    c(st,tradingEvaluation(trade.res))
}
################################################################################
pol1 <- function(signals,market,op,money)
    policy.1(signals,market,op,money,
             bet=0.2,exp.prof=0.025,max.loss=0.05,hold.time=10)
pol2 <- function(signals,market,op,money)
    policy.1(signals,market,op,money,
             bet=0.2,exp.prof=0.05,max.loss=0.05,hold.time=20)
pol3 <- function(signals,market,op,money)
    policy.2(signals,market,op,money,
             bet=0.5,exp.prof=0.05,max.loss=0.05)
################################################################################