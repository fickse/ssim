
# Synthetic control exploration

library(raster); library(data.table);  library(gsynth)
setwd('C:/temp/sandbox/')
load("C:\\temp\\sandbox\\dartRunV2.RData")
load("C:\\temp\\sandbox\\extra.RData")
b <- stack('naip/naip_1202.tif')
bg <- function() plotRGB(b, alpha = 220)#, stretch = 'lin')
bb <- raster(padbufpixels['refrast'])
      
buf <- function() plot(bb, add = TRUE, legend = FALSE, col = rgb(0,0,1,.3))

pad <- function() plot(padpolyi, add = TRUE, col = rgb(1,0,.3,.5), border = NA)
ppol <- function() plot(padpoly, add = TRUE, border = 'magenta')
brast <- function() plot(padbufrast, add = TRUE, legend = FALSE, col = rgb(0,0,1,.4))
      


  uni  = unique(padpixels@data[,c('soilps', 'soilec')])
  uni$uni <- paste0(uni$soilps, '-', uni$soilec)
  padpixels$uni <- match(paste0(padpixels@data$soilps, '-', padpixels@data$soilec), uni$uni)
  
 
namePlot <- function(){
      ff <- list.files()
      if(length(ff) == 0){ l <- 0 } else {
        l = as.numeric(gsub('[^0-9]', '', ff))
        if(all(is.na(l))) l = 0 
        l = max(l, na.rm = TRUE)
      }
      jpeg( paste0('img_', formatC(l + 1, width = 3, flag = '0'), '.jpg') )
     }



#* descriptive animation
  dir.create('figs')
  setwd('figs')
  dir.create('anim')
  # 1. Area

      namePlot()
      bg()
      dev.off()

  # 2. Polygon
      namePlot()
      bg(); ppol()
      dev.off()
      
  # 2.5 Interior buffer
      namePlot()
      bg();  pad()
      dev.off()
      
  # 3. Buffer
      namePlot()
      bg(); brast(); pad()
      dev.off()
      
  # 4. Mask
      namePlot()
      bg(); buf() ; pad ()
      dev.off()
      
  # 5. Candidate search

   arrowz <- function(bj, pp){
      
      cl <- clump(aggregate(bj,3))

      # get centers
      z <- na.omit(cbind(xyFromCell(cl, 1:ncell(cl)), cl[]))
      a <- aggregate(z, by = list(z[,3]), FUN = mean)
      xy <- coordinates(pp)
      arrows( xy[,1], xy[,2], a$x, a$y, col = 'yellow', length = .01)
   }


cand <- function(i){
    

      pp <- padpixels[i,]
      u <- uni[padpixels[i,'uni']@data$uni,]
      okps <- padbufsoilpixels[padbufsoilpixels$soilps == u$soilps,]
      okec <- padbufsoilpixels[padbufsoilpixels$soilec < (u$soilec + 3) &padbufsoilpixels$soilec > (u$soilec - 3),]
      
      
      
      pts <- function() points(coordinates(pp), col = 'yellow', pch = 15)
      
      ok <- stack(raster(okps['refrast']), raster(okec['refrast']))
      
      ps <- function() image(ok[[1]], add = TRUE, col = rgb(0,0,1, .3))
      ec <- function() image(ok[[2]], add = TRUE,col = rgb(0,0,1, .4))
      bth <- sum(ok)
      both <- function() image(bth, add = TRUE,  col = rgb(0,0,1,.4))

      j <- toposim$index[,i]
      bj <- raster(padbufsoilpixels[j,'refrast'])
      pbj <- function() image(bj, add = TRUE, col = 'light blue')
      
      # particle size
      #jpeg("foo%03d.jpg")

           namePlot()
#           bg(); pad();buf(); pts()
           bg(); pad(); pts()
           dev.off()
           
           # namePlot()
           # bg(); pad(); ps() ; pts()
           # dev.off()
           
           
           # namePlot()
           # bg(); pad(); ps() ; ec(); pts()
           # dev.off()
           
           # namePlot()
           # bg(); pad();  both(); pts()
            # dev.off()
           
           namePlot()
           bg(); pad();both(); pbj(); pts(); arrowz(bj, pp)
           dev.off()
           namePlot()
           bg(); pad();both(); pbj(); pts(); arrowz(bj, pp)
           dev.off()
           
}    


  # 4. Candidate Search

      i <- 220
      pp <- padpixels[i,]
      u <- uni[padpixels[i,'uni']@data$uni,]
      okps <- padbufsoilpixels[padbufsoilpixels$soilps == u$soilps,]
      okec <- padbufsoilpixels[padbufsoilpixels$soilec < (u$soilec + 3) &padbufsoilpixels$soilec > (u$soilec - 3),]
      pts <- function() points(coordinates(pp), col = 'yellow', pch = 15)
      ok <- stack(raster(okps['refrast']), raster(okec['refrast']))
      ps <- function() image(ok[[1]], add = TRUE, col = rgb(0,0,1, .3))
      ec <- function() image(ok[[2]], add = TRUE,col = rgb(0,0,1, .4))
      bth <- sum(ok)
      both <- function() image(bth, add = TRUE,  col = rgb(0,0,1,.4))

      j <- toposim$index[,i]
      bj <- raster(padbufsoilpixels[j,'refrast'])
      pbj <- function() image(bj, add = TRUE, col = 'light blue')
      
      # pixel
           namePlot()
           bg(); pad(); pts(); buf()
           dev.off()
      # particle size
           namePlot()
           bg(); pad(); ps() ; pts()
           dev.off()
           
      # Ec and particle size
           namePlot()
           bg(); pad();  both(); pts()
           dev.off()
           
      # Selected
           namePlot()
           bg(); pad();both(); pbj(); pts(); arrowz(bj, pp)
           dev.off()

    # create animation:
    setwd('anim')
    for( i in sample(1:nrow(padpixels), size = 50)){
       cand(i)
    }

    make.mov <- function(){
         unlink("plot.mpg")
         system("convert -delay 6 *.jpg plot.mpg")
    }


  # 5. Synthetic control

#*  are SE's available using single responses?

   # get single trt + matches
   dat <- ev$dat
   id <- 355
   i <- which(dat$id == paste0('trt', id))
   j <- which(dat$id %in% paste0('ctr', toposim$index[,id]))
   dx <- dat[union(i,j),]
   dx$grp <- grepl('trt', dx$id)

    # gsynth
    library(gsynth);library(panelView)
    panelView(y ~ D, data = dx,  index = c("id","time"), type = "outcome") 
    
    gs <- gsynth(y ~ D +iqr, data = dx, index = c("id","time"), force = "two-way", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, parallel = FALSE)
    
    #Answer: YES!
    
   #Synth
   id <- 1
   X1 <-  as.matrix(as.numeric(padpixels@data[id,names(dpar$topoVars)]))
   X0 <- apply(padbufsoilpixels@data[toposim$index[,id], names(dpar$topoVars)], 1, as.numeric)
   Y1plot <- matrix(extraction$extractedTarget$bare[id,])
   
   Y0plot <- t(extraction$extractedReference$bare[ match(toposim$index[,id], extraction$refIndex),])
   ptp <- 1:grep(padpoly$yearEnd, 1984:2019)
   Z0 <- Y0plot[ptp,]
   Z1 <- matrix(Y1plot[ptp,])
   dp <- list(Z0 = Z0, Z1 = Z1, X0 = X0, X1 = X1, Y0plot = Y0plot, Y1plot = Y1plot)
   sc <- synth(dp)
   
   pred <- sc$solution.w %*% Y0plot

#*  Can predictions be improved by using time-varying covariates?
   
    gs0 <- gsynth(y ~ D , data = dat, index = c("id","time"), force = "two-way", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, parallel = FALSE)
    
    gs1 <- gsynth(y ~ D + iqr, data = dat, index = c("id","time"), force = "two-way", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", nboots = 1000, parallel = FALSE)

    gs2 <- gsynth(y ~ D , data = dat, index = c("id","time"),  r = c(0), parallel = FALSE)
    
#*  Can predictions be improved by using time-invariant covariates?
    
    dat2 <- dx
    xx <- t(X0)[rep(1:100, each = 35),]
    xx <- rbind( t(X1)[rep(1, 35),], xx)
    xx <- as.data.table(xx)
    dat2 <- cbind(dat2, xx)
    gs2 <- gsynth(y ~ D + iqr + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17, data = dat2, index = c("id","time"), force = "two-way", CV = TRUE, r = c(0, 5), se = FALSE, inference = "parametric", nboots = 1000, parallel = FALSE)
    
    Error : variable V1 is time invariant. Try to remove it. 
    
#* CausalImpact
   
   id <- 50
   Y <- matrix(extraction$extractedTarget$bare[id,])
   Y0 <- t(extraction$extractedReference$bare[ match(toposim$index[,id], extraction$refIndex),])

   library(CausalImpact)
   dat3 <- cbind(Y, Y0)
   ci <- CausalImpact(dat3, pre.period = c(1,29), post.period = c(30,35))
  
   dat4 <- cbind(dat3, t(extraction$extractedReference$iqr[ match(toposim$index[,id], extraction$refIndex),]))
   ci2 <- CausalImpact(dat4, pre.period = c(1,29), post.period = c(30,35), model.args= list(niter = 5000)
  
  ci3 <- CausalImpact(dat4, pre.period = c(1,29), post.period = c(30,35), model.args= list(niter = 5000, dynamic.regression=TRUE))

  ci3 <- CausalImpact(dat4[,1:100], pre.period = c(1,29), post.period = c(30,35))
