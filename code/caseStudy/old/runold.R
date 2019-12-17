#####################################
## R implementation of DART 2.0
## polygon edition
####################################

args <- commandArgs(TRUE)

# polygon id
p = as.numeric(args[1])

randomize = FALSE

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

## 10/7/19 Now inherited completely from dpar

######################################################

#polygons <- shapefile(dpar$polygons)
load(dpar$polygons)
pids <- c(polygons@data[,dpar$polygon_id_column, with =F][[1]])

dir.create(dpar$outdir, recursive = TRUE)
dir.create(file.path(dpar$outdir,'metadata'), recursive = TRUE)
file.copy('dart_functions.R', file.path(dpar$outdir,'metadata'))
save(dpar, file = file.path(dpar$outdir, 'metadata','dpar.RData'))

######################################
# Initialize

#rasterOptions(maxmemory = 1e+09, chunksize = 1e+08)

 #RANDOMIZER
 if(randomize){
   set.seed(1000)
   p <- sample(unique(pids))[p]
 }
 cat(' running ID ', p, '\n')

 dart_fn(pids[1])
 dart_fn(pids[2])
 dart_fn(pids[3])
 dart_fn(pids[4])
 dart_fn(pids[5])

dir.create(file.path(dpar$outdir, 'evals'))

ff <- list.files(dpar$outdir, full = TRUE, pattern = 'RData')
ff <- ff[!grepl('eval',ff)]

strt <- Sys.time()
for(f in ff){
  eval_dart(f)
}
ed <- Sys.time()

N = list()
for( f in ff){
  load(f)
  d <- data.frame( file = f, pixel = 1:nrow(padpixels))
  N[[f]] <- d
}

N <- do.call(rbind, N)


###################################
###################################

rr <- list.files('out/evals', full = TRUE, pattern = 'RData')

d <- list()
for(r in rr){

  load(r)
  d[[r]] <- dat
}

X <- do.call(rbind, d)
X$unid <- paste0(X$id, X$pixelID)
X$id <- gsub('_.*', '', X$id)

library(ggplot2)



#######

# cool plots dude

v <- X[ year(dayt) == 2010 & month(dayt) %in% c( 3:10),]
v <- v[ , list( id, CausalImpact = CIpoint.effect, DiD = DDatt, gsynth = GSATT, bfast = bfast)]
v <- melt(v, id = 'id')
v <- v[, list(y = mean(value), se = sd(value)/sqrt(.N)), by = list(id, variable)]
ggplot( v, aes(x = id, y = y, fill = variable)) + geom_bar(stat = 'identity') + facet_grid(  . ~ variable , labeller=label_both)

####

v <- X[ year(dayt) > 2010 & month(dayt) %in% c( 3:10),]
v <- v[ , list( year = year(dayt),id, CausalImpact = CIpoint.effect, DiD = DDatt, gsynth = GSATT, bfast = bfast)]
v <- melt(v, id = c('id', 'year'))
v <- v[, list(y = mean(value), se = sd(value)/sqrt(.N)), by = list(id, variable, year)]
ggplot( v, aes(x = year, y = y, color = id, group = id)) + geom_line() + facet_grid(  variable ~ . , labeller=label_both)

####

Z <- X[time > - 10, list(lwr = mean(CIcum.effect.lower), upr = mean(CIcum.effect.upper), y = mean(CIcum.effect)), by = list(id, dayt, time)]
ggplot( Z[,], aes(x = dayt, y = y, group = id, color = id) ) + geom_line() + theme_bw() + geom_ribbon( aes(ymin = lwr, ymax = upr, fill = id), alpha = 0.2, color = NA)

ggplot( X[time > - 10,], aes(x = dayt, y = CIcum.effect, group = unid, color = id) ) + geom_line(alpha = .3) + 
  #geom_smooth(data = Z, aes(x = dayt, y = y, group = id, color = id), size = 1, linetype = 'dashed', se = FALSE) + theme_bw()
  geom_smooth( aes(group = id), size = 1.15, se = FALSE) + theme_bw() + xlab('Date') + ylab('Cumulative change in Satvi * 1000')


Z <- X[time > - 10, list(lwr = mean(CIpoint.effect.lower), upr = mean(CIpoint.effect.upper), y = mean(CIpoint.effect)), by = list(id, year(dayt), month(dayt))]
Z$dayt <- as.Date(paste0(Z$year, '-', Z$month, '-15'))

ggplot( Z[,], aes(x = dayt, y = y, group = id, color = id) ) + geom_point(aes(x = dayt, y =y, group = id)) +  geom_smooth(span = 0.5, aes(fill = id), se = FALSE) + theme_bw() + ylim(-7500, 5000) + ylab('Change in SATVI * 1000') + xlab('Date')

#######


Z <- X[time > - 10, list(y = mean(GSATT)), by = list(id, dayt, time)]
ggplot( Z, aes(x = dayt, y = y, group = id, color = id) ) + geom_point(alpha = .1) + 
  #geom_smooth(data = Z, aes(x = dayt, y = y, group = id, color = id), size = 1, linetype = 'dashed', se = FALSE) + theme_bw()
  geom_smooth( span = 0.5, aes(group = id), se = FALSE) + theme_bw() + xlab('Date')+ ylim(-7500, 5000) + ylab('Change in SATVI * 1000') + xlab('Date')


#######


Z <- X[time > - 10, list(y = mean(DDatt)), by = list(id, dayt, time)]
ggplot( Z, aes(x = dayt, y = y, group = id, color = id) ) + geom_point(alpha = .2) + 
  #geom_smooth(data = Z, aes(x = dayt, y = y, group = id, color = id), size = 1, linetype = 'dashed', se = FALSE) + theme_bw()
  geom_smooth( aes(group = id), size = 1.15, se = FALSE) + theme_bw() + xlab('Date')+ ylim(-7500, 5000) + ylab('Change in SATVI * 1000') + xlab('Date')


######

Z <- X[time > - 10, list(y = mean(bfast)), by = list(id, dayt, time)]
ggplot( Z, aes(x = dayt, y = y, group = id, color = id) ) + geom_point(alpha = .2) + 
  #geom_smooth(data = Z, aes(x = dayt, y = y, group = id, color = id), size = 1, linetype = 'dashed', se = FALSE) + theme_bw()
  geom_smooth( aes(group = id), size = 1.15, se = FALSE) + theme_bw() + xlab('Date')+ ylim(-7500, 5000) + ylab('Change in SATVI * 1000') + xlab('Date')

ggplot( X[time > -10,] , aes(x = dayt, y = bfast, group = unid, color = id) ) + geom_point(alpha = .2) + 
  #geom_smooth(data = Z, aes(x = dayt, y = y, group = id, color = id), size = 1, linetype = 'dashed', se = FALSE) + theme_bw()
  geom_smooth( aes(group = id), size = 1.15, se = FALSE) + theme_bw() + xlab('Date')+ ylim(-7500, 5000) + ylab('Change in SATVI * 1000') + xlab('Date')


