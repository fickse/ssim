
#CausalImpact
CI <- function(s){

  # note CausalImpact wont work if bfast is loaded :
  #  conflict with as.zoo.data.frame :( https://github.com/joshuaulrich/quantmod/issues/168
  X = dcast(s$D, dayt ~ id, value.var = 'y')
  
  #rearrange
  tt = X$dayt
  ddate <- max( s$D[ id == 'trt' & D == 0, dayt] )
  pre = c(1, max(which(tt <= ddate)))
  post = c( pre[2] + 1, length(tt) )

  X = X[, c('trt', grep('c', names(X), value = TRUE) ), with = F]
  
  y <- CausalImpact::CausalImpact(as.matrix(X), pre.period = pre, post.period  =post)
  
  list( effect = y$series[, c('point.effect', 'point.effect.upper', 'point.effect.lower')],  model  = y)
}

#BFAST
BFAST <- function(s){
    
    # convert to timeseries
    x <- s$D[id == 'trt', ]
    y <- ts(x$y , start = min(year(x$dayt)), freq = round( mean( table( year( x$dayt)))))
    
    b = bfast::bfast(y, season = 'dummy', max.iter = 1)
    
    # extract and format outputs
    ef <- b$output[[1]]$Tt
    tt <- zoo::as.Date.yearmon(time(ef))
    ddate <- max( s$D[ id == 'trt' & D == 0, dayt] )
    baseline = ef[tt <= ddate]
    base = baseline[length(baseline)]
    ef[tt > ddate] <- ef[tt > ddate] - base
    ef[tt <= ddate] <- 0
    
    
    bp <- b$output[[1]]$bp.Vt
    cibp <- b$output[[1]]$ci.Vt
    return(list(bfast = b, time = tt, effect = ef, breakpoints = bp, ci = cibp, base = base))

}


# gSYNTH
gSynth <- function(G,returnModel = FALSE, ...){
    
    require(gsynth)
    arguments <-list(...)
    
    nControl <- length(unique(  G$id[which(!grepl('trt', G$id))] ))
    
    d <- list( force = 'two-way', CV = TRUE, r = c(0,5), se = FALSE, inference = 'parametric', nboots = 1000, parallel=FALSE, cores = 1)
    
    if(length(arguments) > 0){
      for( a in names(arguments) ){
        d[[a]] <- arguments[[a]]
      }
    }
    
    # first try
    cat('\ngsynth first try\n')
      g <- try({ gsynth(y ~ D , data = G$D, index = c("id","time"), force = d$force, CV = d$CV, r = d$r, se = d$se, inference = d$inference, nboots = d$nboots, parallel = d$parallel, cores = d$cores) })
    
    if(class(g) == 'try-error'){
      cat('\n gsynth second try: limiting r\n')
        
          d$r <- c( 0, pmin(0, nControl))
          g <- try({ gsynth(y ~ D , data = G$D, index = c("id","time"), force = d$force, CV = d$CV, r = d$r, se = d$se, inference = d$inference, nboots = d$nboots, parallel = d$parallel, cores = d$cores) })
     
     }
    
    if(class(g) == 'try-error'){
      cat('\n gsynth third try: nonparametric boot r\n')
        
          d$inference = 'nonparametric'
          g <- try({ gsynth(y ~ D , data = G$D, index = c("id","time"), force = d$force, CV = d$CV, r = d$r, se = d$se, inference = d$inference, nboots = d$nboots, parallel = d$parallel, cores = d$cores) })
     
     }
    
    if (returnModel){

        ## saves all boots!
         return(g)

        
    } else {
       return( list(  est.ind = g$est.ind, est.att = g$est.att, att = g$att, eff = g$eff) )
   } 
}



DD <- function(G) {

    ## linear model
    ## prediction  = effect of treatment group + effect of time + effect of treatment*time

    require(data.table)
    g <- data.table(G$D)
    gi <- g[D == 0,]
    m <- lm(y ~  id + as.factor(time), gi)
    
    g0 <- g[id == 'trt',]
    
    p <- predict(m, newdata = g0, interval = 'prediction')
    p <- as.data.frame(p)
    
   k =  data.frame( DDatt =  g0$y - p$fit  , DDupr = g0$y - p$upr  , DDlwr = g0$y -p$lwr )
    
}



evaluate <- function(s){
    
    # s is output from simul_data
    tr <- s$truth
    
    ci <- as.data.table(CI(s)$effect)
    names(ci) <- paste0('CI', names(ci))
    tr <- cbind(tr, ci)
    
    dd <- DD(s)
    tr <- cbind(tr, dd)
    
    gs <- gSynth(s, se=TRUE, force = 'two-way', inference = 'nonparametric',r = c(0,5), CV = TRUE, returnModel = FALSE)
    
    gs <- as.data.frame(gs$est.att[, c(1,3,4)])
    names(gs) <- paste0('GS', names(gs))
    tr <- cbind(tr, gs)

    tr$bfast <- BFAST(s)$effect

    tr

}




evaluate.example <- function(s){
    
    # s is output from simul_data
    tr <- s$truth
    
    ci <- as.data.table(CI(s)$effect)
    names(ci) <- paste0('CI', names(ci))
    tr <- cbind(tr, ci)
    
    dd <- DD(s)
    tr <- cbind(tr, dd)
    
    gs <- gSynth(s, se=TRUE, force = 'two-way', inference = 'parametric',r = c(0,5), CV = TRUE, returnModel = FALSE)
    
    gs <- as.data.frame(gs$est.att[, c(1,3,4)])
    names(gs) <- paste0('GS', names(gs))
    tr <- cbind(tr, gs)
    
    b <- BFAST(s)
    tr$bfast <- as.numeric(b$bfast$output[[1]]$Tt)- b$base

    tr

}

