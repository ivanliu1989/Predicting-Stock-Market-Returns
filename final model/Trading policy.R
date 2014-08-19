# bet parameter, which specifies the percentage of our current money, that we will invest each time we open a new position;
# exp.prof parameter, which indicates the profit margin we wish for our positions and is used when posting the limit orders;
# max.loss, which indicates the maximum loss we are willing to admit before we close the position, and is used in stop orders
# hold.time parameter, indicates the number of days we are willing to wait to reach the profit margin.
policy.1 <- function(signals,market,opened.pos,money,
                     bet=0.2,hold.time=10,
                     exp.prof=0.025, max.loss= 0.05){
    d <- NROW(market) # this is the ID of today
    orders <- NULL
    nOs <- NROW(opened.pos)
    # nothing to do!
    if (!nOs && signals[d] == 'h') return(orders)
    
    # First lets check if we can open new positions
    # i) long positions
    if (signals[d] == 'b' && !nOs) {
        quant <- round(bet*money/market[d,'Close'],0)
        if (quant > 0)
            orders <- rbind(orders,data.frame(order=c(1,-1,-1),order.type=c(1,2,3),            
                                              val = c(quant,market[d,'Close']*(1+exp.prof),
                                                      market[d,'Close']*(1-max.loss)),
                                              action = c('open','close','close'),
                                              posID = c(NA,NA,NA)
                                             )
                            )
    # ii) short positions
    } else if (signals[d] == 's' && !nOs) {
        # this is the nr of stocks we already need to buy
        # because of currently opened short positions
        need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1, "N.stocks"])*market[d,'Close']
        quant <- round(bet*(money-need2buy)/market[d,'Close'],0)
        if (quant > 0)
            orders <- rbind(orders,data.frame(order=c(-1,1,1),order.type=c(1,2,3),
                                              val = c(quant,market[d,'Close']*(1-exp.prof),
                                                      market[d,'Close']*(1+max.loss)),
                                              action = c('open','close','close'),
                                              posID = c(NA,NA,NA)
                                              )
                            )
    }
    orders
}

