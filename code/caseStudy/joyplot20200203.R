#####################################
## R implementation of DART 2.0
## polygon edition
####################################

stime <-Sys.time()
setwd("C:\\temp\\sandbox\\run2\\")

   # load('shay.RData')
   # px <- c('M' = 'LTDL_28123', 'C' = 'WRI_1411', 'P' = 'WRI_1778', 'B' = "WRI_1776")
   # polygons <- shay[shay$id %in% px,]
   # w <- which(polygons$src == 'LTDL' & polygons$polyID != 5302)
   # if(length(w) > 0 ) polygons <- polygons[-w,]
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

# joyplot figure

 load("C:\\projects\\ssim\\code\\caseStudy\\out\\output.RData")


X$unid <- paste0(X$id, X$pixelID)
#X$id <- gsub('_.*', '', X$id)

library(ggplot2)

X[, DDcum := cumsum(DDatt), by = list(id, pixelID, time > 0)]
X[, GScum := cumsum(GSATT), by = list(id, pixelID, time > 0)]
X[, bfastcum := cumsum(bfast), by = list(id, pixelID, time > 0)]

#######

# cool plots dude
  

v <- X[ year(dayt) %in% 2010 & month(dayt) %in% c( 3:11),]
setnames(v, 'y', 'Raw SATVI')
v <- v[ , list( id, `Raw SATVI`, CausalImpact = point.effect, DiD = DDatt, gsynth = GSATT, bfast = bfast)]
v <- melt(v, id = 'id')

  #JOYPLOT
  library(ggridges)
  vv = c("#C4664A80", "#C4C24A80", "#4AC2C480", "#AF4AC480")
  vv = c("#C4664A", "#C4C24A", "#4AC2C4", "#AF4AC4")
  vv = c("#ff901f", "#ff2975", "#f222ff", "#8c1eff")
  linecol = grey(.9)
  bgcol = 'black'
  
#  vv = c( "#08f7fe","#fe53bb","#f5d300", "#09fbd3")
#  linecol = grey(.2)
  
  
jp <- function(v) {
  ggplot(v, aes(x = value, y = id, fill=factor(..quantile..)) )+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE, scale = 1.5, color = linecol, size = .35
  )  + 
  scale_fill_manual(values = vv) + 
  theme(legend.position = "none") + 
  ylab('') + xlab('') +
  ggtitle(unique(v$variable)) + 
  theme(#panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank()#,
        # panel.background = element_blank()
        panel.background = element_rect(fill = 'black', colour = 'black')
        )
}
 
 p1 <- jp( v[variable == 'CausalImpact',]) + xlim(-15000,15000)+ theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
 p2 <- jp( v[variable == 'gsynth',])+ xlim(-15000,15000) + theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
 p3 <- jp( v[variable == 'DiD',])+ xlim(-15000,15000) + theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
   p4 <- jp( v[variable == 'bfast',]) + xlim(-250,50) + theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
   p5 <- jp( v[variable == 'Raw SATVI',])  + theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
   
 
 
pdf('figs/joyResults.pdf', width = 8 , height = 6)
require(gridExtra)
grid.arrange(p5, p1, p2, p4,p3, ncol = 1, nrow = 5)
#grid.arrange(p5, p1, p2, p3, p4, layout_matrix = matrix( c( 1, 1, 1, 1,2,3,4,5), 2, 4))
dev.off()


v <- v[, list(y = mean(value), se = sd(value)/sqrt(.N)), by = list(id, variable)]
ggplot( v, aes(x = id, y = y, fill = variable)) + geom_bar(stat = 'identity') + facet_grid(  . ~ variable , labeller=label_both)
####

# only significant 
  

v <- X[ year(dayt) %in% 2010 & month(dayt) %in% c( 3:11),]
v <- v[, list( id, 
          CausalImpact = ifelse( CIpoint.effect.upper <0 | CIpoint.effect.lower > 0 , CIpoint.effect, NA),
          DiD = ifelse( DDupr < 0 | DDlwr > 0 , DDatt, NA),
          GS = ifelse( GSCI.upper < 0 | GSCI.lower > 0 , DDatt, NA)
         ) ] 
v <- melt(v, id = 'id')
v <- na.omit(v)

ggplot(v, aes(x = value, y = id, fill=factor(..quantile..)) )+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE, scale = 1.5, color = linecol, size = .35
  )  + facet_wrap(. ~variable) + 
  scale_fill_manual(values = vv) + 
  theme(legend.position = "none") + 
  ylab('') + xlab('') +

####

v <- X[ year(dayt) > 2010 & month(dayt) %in% c( 3:10),]
v <- v[ , list( year = year(dayt),id, CausalImpact = CIcum.effect, DiD = DDcum, gsynth = GScum, bfast = bfastcum)]
v <- melt(v, id = c('id', 'year'))
v <- v[, list(y = mean(value), se = sd(value)/sqrt(.N)), by = list(id, variable, year)]
ggplot( v, aes(x = year, y = y, color = id, group = id)) + geom_line() + facet_grid(  variable ~ . , labeller=label_both)

####
 # cumulative effects
 
 pdf('figs/cumulativeCI.pdf')
Z <- X[time > - 10, list(lwr = mean(CIcum.effect.lower), upr = mean(CIcum.effect.upper), y = mean(CIcum.effect)), by = list(id, dayt, time)]
ggplot( Z[,], aes(x = dayt, y = y, group = id, color = id) ) + geom_line() + theme_bw() + geom_ribbon( aes(ymin = lwr, ymax = upr, fill = id), alpha = 0.2, color = NA)
dev.off()

pdf(file.path("C:/projects/ssim/docs/figures/", paste0('cumulativeCIlines', format(Sys.time(), '%Y-%m-%d'), '.pdf')), height= 6, width = 8)
ggplot( X[time > - 10 ,], aes(x = dayt, y = CIcum.effect, group = unid, color = id) ) + geom_line(alpha = .5, size = .15) + 
  #geom_smooth(data = Z, aes(x = dayt, y = y, group = id, color = id), size = 1, linetype = 'dashed', se = FALSE) + theme_bw()
  geom_smooth( aes(group = id), size = 1.15, se = FALSE, span = .25) + theme_bw() + xlab('Date') + ylab('Cumulative change in SATVI * 1000') + theme(legend.position = c(.15, .15), legend.title = element_blank())
dev.off()


v <- melt( X[time > -10, list( dayt, CIcum.effect, unid, id, DDcum, GScum, bfastcum)], id.vars = c('id', 'unid', 'dayt'))

ggplot( v, aes(x = dayt, y = value, group = unid, color = id) ) + geom_line(alpha = .15, size = .15) + facet_wrap( . ~ variable) + 
  geom_smooth( aes(group = id), size = 1.15, se = FALSE, span = .25) + theme_bw() + xlab('Date') + ylab('Cumulative change in Satvi * 1000')







Z <- X[time > - 10 & time < 30, list(lwr = mean(CIpoint.effect.lower), upr = mean(CIpoint.effect.upper), y = mean(CIpoint.effect)), by = list(id, year(dayt), month(dayt))]
Z$dayt <- as.Date(paste0(Z$year, '-', Z$month, '-15'))

ggplot( Z[,], aes(x = dayt, y = y, group = id, color = id) ) + geom_point(aes(x = dayt, y =y, group = id)) +  geom_smooth(span = 0.5, aes(fill = id), se = FALSE) + theme_bw() + ylim(-7500, 5000) + ylab('Change in SATVI * 1000') + xlab('Date')

ggplot( X[time > -10 & time < 30,], aes(x = dayt, y = CIpoint.effect, group = unid, color = id) ) + geom_point(aes(x = dayt, y =CIpoint.effect)) +  geom_smooth(span = 0.25, aes(group = id,fill = id), se = FALSE) + theme_bw() + ylim(-7500, 5000) + ylab('Change in SATVI * 1000') + xlab('Date')

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


