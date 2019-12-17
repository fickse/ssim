
args <- commandArgs(TRUE)

d <- read.csv('orders.csv', stringsAsFactors = F)

# polygon id
chunk = as.numeric(args[1])
nchunk = as.numeric(args[2])

w <- which(as.numeric(cut(1:nrow(d), nchunk)) == chunk)

#####################################################

stime <-Sys.time()
setwd("C:\\temp\\sandbox\\run\\")

  # load('shay.RData')
  # px <- c('M_w' = 'WRI_1410', 'M_e' = 'WRI_1779', 'C' = 'WRI_1411', 'P' = 'WRI_1778', 'B' = "WRI_1776")
  # polygons <- shay[shay$id %in% px,]
  # polygons <- polygons[which(area(polygons) > 1),]
  # polygons$key <- names(px)[match(polygons$id, px)]
  # save(polygons, file = 'polys.RData')


source('dart_functions.R')
source('params.R')  # creates object 'dpar'
source('eval_functions.R')

######################################################
## Load packages
required.packages <- c("raster", "rgdal", "rgeos","stats","gower", 'data.table')

lapply(required.packages, require, character.only=T)
rm(required.packages)

######################################################
# Parameters


eval_dart <- function(fin, pix){

  load( fin )
  
  ## Evaluation
    zero_fill <- function(s) formatC(as.numeric(s), width = 2, flag = '0')

    yr <- as.Date( paste0(padpoly$yearEnd, zero_fill( padpoly$monthEnd ), zero_fill (padpoly$dayEnd)), format = '%Y%m%d')
    yr0 <- as.Date( '2009-06-01', format = '%Y-%m-%d')
            
        
     o <- longPanel(extraction, yr0, dpar$varname, TRUE)
     
     ids <- c( paste0('trt', pix), paste0('ctr', toposim$index[,pix]))

      oi <- o[which(o$id %in% ids),]
      oi <- na.omit(oi)
      oi$id <- ifelse(grepl('trt', oi$id), 'trt', oi$id)

      ev <- evaluate(list( D= data.table(oi)))

     ev$pixelID <- pix
     ev$id <- padpoly$key
     ev   
}

dat <- list()

for( i in w){

    pix <- d$pixel[i]
    f <- d$file[i]
    dat[[as.character(i)]] <- eval_dart(f, pix)
}

dat = do.call(rbind, dat)

save(dat, file = file.path(dirname(f), 'evals', paste0(chunk, '.RData')))

