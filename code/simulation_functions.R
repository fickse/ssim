
# seasonal effect
  seasonal <- function(amp, T, phase = 4.5){

      per <- 2*pi / 360
      
      doy <- yday(T)
      amp*sin(per*doy + phase)
  }  
  
  seasAnom <- function( T, sD = .2, b = 7, c1 = 3, c2 = 3){
    
    T = (yday(T)/16)
    x = ifelse(T > b , exp(-(T-b)^2/c1), exp(-(b-T)^2/c2))
    
    # get local maxima
    n <- localMinima(x)
    # chop into bits
    id <- as.numeric ( cut(1:length(T), c(-1, n, length(n) + 1)))
    rw <- filter(rnorm( 200, 0, sD), filter = c(0,0,1), circular = FALSE)
    a <- na.omit(rw)[1:length(id)]
    r <- a[id]
    r*x
  }

# random noise
noise <- function(sD, T){
   
   rnorm(length(T), 0, sD)

}

# average value
ybar <- function(type, T){

  ifelse(type == 'forest', rep(.8, length(T)), rep(.6, length(T))) 

}

# treatment effect
trt <- function(T,eff = .3, ddate = as.Date('2006-02-15')){
  
  y <- rep(0, length(T))
  i <- which(T >=  ddate)
  x <- -1*eff + (T[i] - ddate)*eff/(360*4)
  y[i] <- x
  j <- which((T - ddate) > (360*4))
  y[j] <- 0
  y  
}

localMinima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-.Machine$integer.max, -1*x)) > 0L
  
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

#climate anomaly confounder
climAnomaly <- function(T, sD){
    
    # two seasons:
        # spring anomaly -  parabola centered on spring date
        sprDate <- yday ( as.Date( '2005-04-15' ))
        
        # fall anomaly - parabola centered on fall date
        fallDate <- yday( as.Date( '2005-07-15' ))

        anom = .2 #scaling factor
        
        spr = dnorm(yday(T) - sprDate,0, 30)
        spr = spr / max(spr)
        
        fall = dnorm(yday(T) - fallDate,0, 30)
        fall = fall / max(fall)
        
        # randomly assign sign and size of climate anomaly
        
        ny <- localMaxima(spr)
        ny2 <- localMaxima(fall)
        n <- sort(c(ny, ny2))
        rw <- filter(rnorm( 200, 0, sD), filter = c(0,0,1), circular = FALSE)
        rw <- na.omit(rw)
        ss <- smooth.spline(x= T[n], y = rw[1:length(n)], spar = .01)
        predict(ss, as.numeric(T))$y
        
}

satellite <- function(tt, lambda = .5){

   sample(c(-.25,0), size = length(tt), replace = TRUE, prob = c(lambda, 1-lambda))

}

parab <- function(x, a, h , k) {
           a*(x-h)^2 + k
}

randWalk <- function(tt, sD = .01){

  cumsum(rnorm(length(tt), 0, sD))
}


  
simul_data <- function(R) {
  

  # R is a list with named parameters
  cat('unpacking R\n')
  for (v in 1:length(R)) assign(names(R)[v], R[[v]])
  cat('current workspace:', ls(), sep = '\n\t')
  
  set.seed(sim)
  
  # produce `long` data.frame with the following columns
    # id = name of pixel
    # y = response value
    # D = treatment occurred
    # time = timestep
  
  
  # Determine number and type of controls
  n1 <- round((1-misMatch)*nControl)
  n2 <- nControl - n1

  if( type == 'grassland'){
    ctrT = c( rep('grassland', n1), rep('forest', n2))
  }else if( type == 'forest'){
    ctrT = c( rep('forest', n1), rep('grassland', n2))
  }

  # generate persistent values
  climAnom <- seasAnom(tt, sD = climSD, b = climCenter, c1 = 22, c2 = 22)
  sat <- satellite(tt, lambda = satLambda)
  rw <- randWalk(tt, rwSD)
  tr <- trt(tt, disturbance, ddate = ddate)


  # generate controls
  ctrl <- matrix(NA, nControl, length(tt))

  # default values
  noi <- c('grassland' = 0.04, 'forest' = 0.02)

  # over-ride default veg values
  if(overrideNoise){
      noi <- c('grassland' = sdNoise, 'forest' = sdNoise)
  } 

  #Add spatial autocorr?
  ac <- rep(1, length(ctrT))
  moransI <- 0
  if( auto_range > 0){
   ac.v <-  sac(auto_range)
   ac <- ac.v$sac
   moransI <- ac.v$moransI
  }
  
  for( i in 1:length(ctrT)){
     
     affinity <- rnorm(1, 0, affinitySD)
     tvar <- rnorm(length(tt), 0, timeVaryingAffinitySD)
     affinity <- 1 + affinity + tvar
     const <- rnorm(1, 0, randConstantSD)
     
    if(ctrT[i] == 'grassland'){
          seas <-  seasonal(.5, tt)
          nse <- noise(noi['grassland'], tt)
          hadj <- ybar('grass', tt)
      } else if(ctrT[i] == 'forest'){
          seas <-  seasonal(.1, tt)
          nse <- noise(noi['forest'], tt)
          hadj <- ybar('forest', tt)
      }

     # add autocor
     
       hadj.v <- ifelse('hadj' %in% auto_type, ac[i], 1)
       nse.v <- ifelse('nse' %in% auto_type, ac[i], 1)     
       sat.v <- ifelse('sat' %in% auto_type, ac[i], 1)
       clim.v <- ifelse('climAnom' %in% auto_type, ac[i], 1)
       rw.v <- ifelse('rw' %in% auto_type, ac[i], 1)     

     
     y = hadj*hadj.v  + nse*nse.v + seas + sat * affinity * sat.v + climAnom*affinity * clim.v + rw * affinity * rw.v + const

     if( 'response' %in% auto_type ){
        y <- y * ac[i]
     }

     ctrl[i,] = y

  }
    
      if(type == 'grassland'){
          seas <-  seasonal(.5, tt)
          nse <- noise(noi['grassland'], tt)
          hadj <- ybar('grass', tt)
      } else if(type == 'forest'){
          seas <-  seasonal(.1, tt)
          nse <- noise(noi['forest'], tt)
          hadj <- ybar('forest', tt)
      }
    
      y = seas+ climAnom + nse + hadj + tr +sat + rw 

    X <- rbind(y, ctrl)
    row.names(X) <- c('trt', paste0('c', 1:nControl))

    trt.oc <- c (  ifelse( (ddate-tt) > 0, 0, 1), rep(0, nrow(ctrl) * ncol(ctrl)))

    D <- data.table( id = rep(row.names(X), each = ncol(X)),
                     y = c(t(X)),
                     D = trt.oc,
                     time = 1:length(tt),
                     dayt = tt)
    truth <- data.table( season = seas, climate = climAnom, treat = tr, satellite = sat, drift = rw, noise = nse, Y = y, date = tt)
    
   list(
    D = D,
    truth = truth,
    sac = moransI
  ) 
}


# v = simul_data(X1[17,])
# D <- melt(v$truth, id.var = 'date')
# ggplot(D, aes(x = date, y = value)) + geom_line(aes(color = variable)) + 
        # facet_grid(variable ~ ., scales = 'free_y') + theme(legend.position = "none")+ theme_bw()


# panelView(y ~ D, data = data.frame(v$D),  index = c("id","time"), type = 'outcome')


simTest <- function(...){

     R <- expand.grid(sim = 1, type = 'grassland',  sdNoise= .05, disturbance = .1,  nControl = 50, misMatch = .5, climSD = .1, climCenter = 10, satLambda = .05, rwSD = .01, affinitySD = .25, timeVaryingAffinitySD = .05, randConstantSD = .05, overrideNoise = TRUE, stringsAsFactors= FALSE, auto_range = 0, auto_type = 'response')

  x <- list(...)
  for(n in names(x)){
    R[[n]] <- x[[n]]
  }

  ddate <- as.Date('2006-02-15')
  tt <- seq( as.Date('2001-01-01'), as.Date('2010-01-01'), by = 16)

  cat( 'simlating data with ', paste0('\n\t', names(R), ' = ',R[1,]), '\n') 
  simul_data(R)

}


simPlot <- function(sim){

  require(ggplot2)
  d <- sim$D
  d$type <- ifelse(grepl('trt', d$id), 'treated', 'control')
  ggplot( d, aes(x = dayt, y = y, color = type, group = id)) + geom_line(alpha = .5)  +
   theme_minimal()

}


evPlot <- function(ev, type = 'abs'){
   require(ggplot2)

    D <-melt(ev, id = c('season', 'climate', 'treat', 'satellite', 'drift','noise', 'Y', 'date'))

    D$method <- substr(D$variable, 1,2)

    xx <- c("CIpoint.effect" = 'y.hat', "CIpoint.effect.upper" = 'up',
            "CIpoint.effect.lower" = 'low', "DDatt"= 'y.hat', "DDupr" = 'up', "DDlwr" = 'low', "GSATT" = 'y.hat', "GSCI.lower" = 'low',
            "GSCI.upper" = 'up', "bfast" = 'y.hat', 'ITS.att' = 'y.hat', 'ITS.upr' = 'up', 'ITS.lwr' = 'low')
    D$variable <- xx[D$variable]

    d <- dcast(D, date + Y + treat +  method ~ variable, value.var = 'value')
    if( type == 'abs'){
      return (ggplot(d, aes(x = date, y = y.hat, group = method, color = method)) + geom_line() +
            geom_ribbon(aes(ymin = low, ymax = up, fill = method), color = NA, alpha = .4) +
            geom_line(aes(x = date, y = treat), color = 'red', linetype = 'dashed') +
            facet_grid(  method ~ .))
    }
    if( type == 'error'){
      d$err <- d$y.hat - d$treat
      d <- d[date > ddate,]
      d$absErr <- abs(d$err)
      d[,cumulErr := cumsum(absErr), by = list(method)]

      ggplot(d, aes(x = date, y=cumulErr, color = method)) + geom_line()
    }
   
}


# generate a spatial field


srf <- function( x = 100, y = 100, sill = .25, r = 10){
          require(gstat)
          require(raster)
          xy <- expand.grid(1:x, 1:y)
          names(xy) <- c('x','y')
          g.dummy <- gstat(formula=z~1, locations=~x+y, dummy=T, beta=1, model=vgm(psill=sill, range=r, model='Exp'), nmax=20)

          yy <- predict(g.dummy, newdata=xy, nsim=1,  debug.level = 0)
          r <- raster(matrix(yy$sim1, y,x))
          r
}



m.i <- function(x,y,z){
          require(ape)
          ozone.dists <- as.matrix(dist(cbind(x, y)))

          ozone.dists.inv <- 1/ozone.dists
          diag(ozone.dists.inv) <- 0


          ape::Moran.I(z, ozone.dists.inv)
    }

sac <- function(rad, side = 20, n = 100){

          require(data.table)
          sr <- srf(side,side, sill = .01, r =rad)

          a <- sampleRandom(sr, n, xy = TRUE)
          morans <- m.i(a[,'x'], a[,'y'], a[,'layer'])$observed
          v <- data.table(a)
          v$moransI <- morans
          setnames(v, 'layer', 'sac')
          v[, list(sac, moransI)]

    }

