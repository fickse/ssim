
# Synthetic control exploration

library(raster); library(data.table);  library(gsynth)
setwd('C:/temp/sandbox/')

load("C:\\temp\\sandbox\\run2\\out\\P.RData")
b <- stack('naip/karl.grd')

polygons <- rgdal::readOGR("C:\\temp\\sandbox\\shay_simpl.kml")
polygons <- spTransform(polygons, projection(b))
 
bg <- function() plotRGB(b, alpha = 220)#, stretch = 'lin')

brastCol <- rgb(0,0,1,.2)
ptsCol <- 'red'
polycol <- 'yellow'
bothCol <- 'light blue'
chosenCol <- '#ed7428'
linesCol <- rgb(1,1,1,.4)

brast <- function() image(padbufrast, add = TRUE, legend = FALSE, col = brastCol)
polys <- function() plot(polygons, add = TRUE, border = polycol)      

   arrowz <- function(bj, pp){
      
      cl <- clump(aggregate(bj,3))

      # get centers
      z <- na.omit(cbind(xyFromCell(cl, 1:ncell(cl)), cl[]))
      a <- aggregate(z, by = list(z[,3]), FUN = mean)
      xy <- coordinates(pp)
      arrows( xy[,1], xy[,2], a$x, a$y, col = linesCol, length = .01)
   }




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



  # 1. Area

      #namePlot()
      bg()
      #dev.off()

  # 2. Polygon
      #namePlot()
      bg(); polys(); pad()
      #dev.off()
      
  # 2.5 Interior buffer
      namePlot()
      bg();  pad()
      dev.off()
      
  # 3. Buffer
      namePlot()
      bg(); brast(); polys()
      dev.off()
      
  # 4. Mask
      namePlot()
      bg(); buf() ; pad ()
      dev.off()
      
  # 5. Candidate search



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
      pbj <- function() image(bj, add = TRUE, col = chosenCol)
      
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

      i <- 16
      pp <- padpixels[i,]
      u <- uni[padpixels[i,'uni']@data$uni,]
      okps <- padbufsoilpixels[padbufsoilpixels$soilps == u$soilps,]
      okec <- padbufsoilpixels[padbufsoilpixels$soilec < (u$soilec + 3) &padbufsoilpixels$soilec > (u$soilec - 3),]
      pts <- function() points(coordinates(pp), col = ptsCol, cex = 1.1, pch = 15)
      ok <- stack(raster(okps['refrast']), raster(okec['refrast']))
      ps <- function() image(ok[[1]], add = TRUE, col = rgb(0,0,1, .3))
      ec <- function() image(ok[[2]], add = TRUE,col = rgb(0,0,1, .4))
      bth <- sum(ok)
      both <- function() plot(bth, add = TRUE,  col = rgb(0,0,1,.4), legend = FALSE)
      both <- function() image(bth, add = TRUE,  col = bothCol)

      j <- toposim$index[,i]
      dj <- toposim$distance[,i]
      padbufsoilpixels$dist <- NA
      padbufsoilpixels$dist[j] <- dj

      bj <- raster(padbufsoilpixels[j,'refrast'])
      pbj <- function() image(bj, add = TRUE, col = '#18db5f')
      pbj <- function() image(bj, add = TRUE, col = chosenCol)
      
      leg <- function() {
      
        legend('bottomleft', fill = c(ptsCol, brastCol, bothCol, chosenCol), 
            legend = c('Focal Pixel', 'Candidate Pixels', 'Salinity & Texture Subset', 'Topo-edaphic Subset'), border = c('red', brastCol, brastCol, bothCol), bg = rgb(1,1,1, .5),
            box.col = rgb(1,1,1, 0) , cex = 1.7)
              
      }
      
      
##############################################################
           pdf('figs/dart.pdf')
           bg(); both();brast(); leg();polys();  pbj(); pts(); arrowz(bj, pp)
           
           dev.off()
##############################################################




# timeseries


 tg <- extraction$extractedTarget$satvi[i,]
 ct <- extraction$extractedReference$satvi

 j <- toposim$index[,i]
 jj <- match(j, unique( c(toposim$index)))
 ct <- ct[jj,]
 ddt <- as.Date( names(tg), format = 'X%Y%m%d')
 
 # plot(ddt, tg, type = 'l', col = 'red', ylab = 'SATVI * 1000', xlab = '')
 # apply(ct, 1, function(x) lines(ddt, x, col = rgb(.65,.65,.65,.45)))
# apply(ct, 1, function(x) lines(x, col = rgb(0,0,0,.05)))
 # lines(ddt, tg,  col = 'red')
 # abline(v = as.Date('2009-06-01'), lty = 3)

 load("C:\\temp\\sandbox\\run2\\out\\output.RData")
 ef <- X[id == 'P' & pixelID == i,]
 yl <- range( c(ef$CIcum.effect.upper, ef$CIcum.effect.lower))
 # plot(ef$dayt, ef$CIcum.effect, type = 'l', ylim = yl, col = 'red', lwd = 2)
 # polygon( c(ef$dayt, rev(ef$dayt)) , c(ef$CIcum.effect.upper, rev(ef$CIcum.effect.lower)), col = rgb(1,0,0,.3), border = NA)
 
 
library(ggplot2)
v <- rbind(tg, ct)
v <- data.frame(v)
v$id <- c('trt', paste0('ctr', 1:nrow(ct)))
V <- melt(v, id = 'id')
V$dayt <- as.Date(V$variable, format = 'X%Y%m%d')
V$grp <- ifelse(grepl('trt', V$id), 'treated', 'control')

p1  <- ggplot( V, aes(x = dayt, y = value, group = id, col = grp) ) + geom_line() + scale_color_manual(values = c('#a9abaa66', 'red')) + theme_bw()+ theme(legend.position = 'none') + ylab('SATVI  * 1000') + xlab('')

p2 <- ggplot( ef, aes(x = dayt, y = CIpoint.effect, group = unid, color = unid) ) + geom_point(aes(y = CIpoint.effect, x = dayt),alpha = .2) +  geom_smooth(  size = 1.15, se = TRUE, fill = 'red', alpha = .2, span = .2) + theme_bw() + xlab('') + ylab('Treatment Effect\n (SATVI *1000)')  + theme(legend.position = 'none') + geom_vline( xintercept =  as.Date('2009-06-01'), linetype = 'dashed')+ coord_cartesian(ylim=c(-4000, 4000))


p3 <- ggplot( ef, aes(x = dayt, y = CIcum.effect, group = unid, color = unid) ) + geom_line(size = 1.15) +geom_ribbon( aes( ymin = CIcum.effect.lower, ymax = CIcum.effect.upper), fill = 'red', alpha = .2, color = NA)+ xlab('') + theme_bw() + ylab('Cumulative Treatment Effect\n (SATVI * 1000)')  + theme(legend.position = 'none') + geom_vline( xintercept =  as.Date('2009-06-01'), linetype = 'dashed')


##############################################################
pdf('figs/tsExample.pdf', height = 6, width = 12)
  gridExtra::grid.arrange(p1, p2, p3, ncol = 1, nrow = 3)
dev.off()
##############################################################





cr <- colorRampPalette( c('#fc1c03', '#fcb103', '#befc03', '#03fcf4'))

# finally -- make pixel-wise image 

ff <- list.files("C:\\temp\\sandbox\\run2\\out\\", pattern = 'RData', full = TRUE)
ff <- ff[which(!grepl('output', ff))]
ppx <- list()

for ( f in ff ){

  load(f)
  padpixels$uid <- paste0(padpoly$key, 1:nrow(padpixels))
  ppx[[f]] <- padpixels

}

j <- do.call(rbind, ppx)

load("C:\\temp\\sandbox\\run2\\out\\output.RData")
 
v <- X[ year(dayt) %in% 2010 & month(dayt) %in% c( 3:11),]
g <- v[, list(median = median(CIpoint.effect), mean = mean(CIpoint.effect)), by = list( pixelID, id, unid)]

j$delta2010 <- g$median[match(j$uid, g$unid)] 
#j$delta2010 <- g$mean[match(j$uid, g$unid)] 

r <- raster(j['delta2010'])
writeRaster(r, 'delta.tif')


j <- stack('shayPost.tif')

e <- new("Extent", xmin = -1175858.61620485, xmax = -1175127.57715831, 
    ymin = 1744869.24828071, ymax = 1745843.96700942)

jj <- crop(j, e)


######################################################
    pdf('figs/cloropleth.pdf')
    plotRGB(jj, maxpixels = 1e7)
    cc <-cr(10)
    cc <- paste0(cc, '99')
    plot(polygons, add = TRUE, border = 'yellow', lwd = 2)

    image(r, col = cc, add = TRUE)
    dev.off()
######################################################
   plot(r, col = cc)
r <- projectRaster(r, crs = projection(raster()))
#cc <- paste0(cc, '66')

KML(r, file = 'delta.kml', col = cc, overwrite = TRUE)



