
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
  
  ddate <- as.Date('2006-02-15')
  tt <- seq( as.Date('2001-01-01'), as.Date('2010-01-01'), by = 16)

  
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

  for( i in 1:length(ctrT)){
     
     affinity <- rnorm(1, 0, affinitySD)
     tvar <- rnorm(length(tt), 0, timeVaryingAffinitySD)
     affinity <- 1 + affinity + tvar
     const <- rnorm(1, 0, randConstantSD)
     
    if(ctrT[i] == 'grassland'){
          seas <-  seasonal(.5, tt)
          nse <- noise(.04, tt)
          hadj <- ybar('grass', tt)
      } else if(ctrT[i] == 'forest'){
          seas <-  seasonal(.1, tt)
          nse <- noise(.02, tt)
          hadj <- ybar('forest', tt)
      }
      
     y = hadj + nse + seas + sat * affinity + climAnom*affinity + rw * affinity + const
     ctrl[i,] = y

  }
    
      if(type == 'grassland'){
          seas <-  seasonal(.5, tt)
          nse <- noise(.04, tt)
          hadj <- ybar('grass', tt)
      } else if(type == 'forest'){
          seas <-  seasonal(.1, tt)
          nse <- noise(.02, tt)
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
    truth = truth
  ) 
}


# v = simul_data(X1[17,])
# D <- melt(v$truth, id.var = 'date')
# ggplot(D, aes(x = date, y = value)) + geom_line(aes(color = variable)) + 
        # facet_grid(variable ~ ., scales = 'free_y') + theme(legend.position = "none")+ theme_bw()


# panelView(y ~ D, data = data.frame(v$D),  index = c("id","time"), type = 'outcome')


