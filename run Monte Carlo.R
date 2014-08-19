# The list of learners we will use
TODO <- c('svmR','svmC','earth','nnetR','nnetC')
# The datasets used in the comparison
DSs <- list(dataset(Tform,Tdata.train,'SP500'))
# Monte Carlo (MC) settings used
MCsetts <- mcSettings(20, # 20 repetitions of the MC exps
                      2540, # ~ 10 years for training
                      1270, # ~ 5 years for testing
                      1234) # random number generator seed
# Variants to try for all learners
VARS <- list()
VARS$svmR <- list(cost=c(10,150),gamma=c(0.01,0.001),
                  policy.func=c('pol1','pol2','pol3'))
VARS$svmC <- list(cost=c(10,150),gamma=c(0.01,0.001),
                  policy.func=c('pol1','pol2','pol3'))
VARS$earth <- list(nk=c(10,17),degree=c(1,2),thresh=c(0.01,0.001),
                   policy.func=c('pol1','pol2','pol3'))
VARS$nnetR <- list(linout=T,maxit=750,size=c(5,10),
                   decay=c(0.001,0.01),
                   policy.func=c('pol1','pol2','pol3'))
VARS$nnetC <- list(maxit=750,size=c(5,10),decay=c(0.001,0.01),
                   policy.func=c('pol1','pol2','pol3'))
# main loop
for(td in TODO) {
    assign(td,experimentalComparison(DSs,c(do.call('variants',
                                                   c(list('single',learner=td),VARS[[td]],
                                                     varsRootName=paste('single',td,sep='.'))),
                                           do.call('variants',
                                                   c(list('slide',learner=td,
                                                          relearn.step=c(60,120)),
                                                     VARS[[td]],varsRootName=paste('slide',td,sep='.'))),
                                           do.call('variants',c(list('grow',learner=td,relearn.step=c(60,120)),
                                                                VARS[[td]],varsRootName=paste('single',td,sep='.')))
                                           ),MCsetts)
           )
    # save the results
    save(list=td,file=paste(td,'Rdata',sep='.'))
}