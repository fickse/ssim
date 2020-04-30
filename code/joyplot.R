
# joyplot figure

  library(ggridges)
  vv = c("#C4664A80", "#C4C24A80", "#4AC2C480", "#AF4AC480")
  vv = c("#C4664A", "#C4C24A", "#4AC2C4", "#AF4AC4")
  vv = c("#ff901f", "#ff2975", "#f222ff", "#8c1eff")
  linecol = grey(.9)
  linecol2 = grey(.9)
  bgcol = 'white'
  
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
        panel.background = element_rect(fill = bgcol, colour = linecol),
        panel.grid.minor = element_line(size = (0.2), colour=linecol2),
        panel.grid.major = element_line(size = (0.7), colour=linecol2)
        )
}
 

 load("C:\\projects\\ssim\\code\\caseStudy\\out\\output.RData")



X$unid <- paste0(X$id, X$pixelID)
#X$id <- gsub('_.*', '', X$id)

library(ggplot2)

X[, DDcum := cumsum(DDatt), by = list(id, pixelID, time > 0)]
X[, GScum := cumsum(GSATT), by = list(id, pixelID, time > 0)]
X[, bfastcum := cumsum(bfast), by = list(id, pixelID, time > 0)]

v <- X[ year(dayt) %in% 2010 & month(dayt) %in% c( 3:10),]
setnames(v, 'y', 'Raw SATVI')
v <- v[ , list( id, pixelID, `Raw SATVI`, CausalImpact = point.effect, DiD = DDatt, gsynth = GSATT, bfast = bfast)]
v <- melt(v, id = c('id', 'pixelID'))

v <- v[, list(value = median(value)), by = list(id, pixelID, variable)]

 xl <- -7500
 xu <- 2500
 
 p1 <- jp( v[variable == 'CausalImpact',]) + xlim(xl,xu)+ theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
 p2 <- jp( v[variable == 'gsynth',])+ xlim(xl,xu) + theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
 p3 <- jp( v[variable == 'DiD',])+ xlim(xl,xu) + theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
   p4 <- jp( v[variable == 'bfast',]) + xlim(-250,85) + theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
 p5 <- jp( v[variable == 'Raw SATVI',])  + theme(axis.ticks.x=element_blank(), axis.ticks.y=element_blank())










dat <- fread('C:/projects/ssim/docs/KARL2014.csv', strings = FALSE)

dat$dwn <- dat$value - dat$se
dat$up <- dat$value + dat$se
 
key = c('N' = 'Bare Ground', 'L' = 'Litter', 'H' = 'Herbaceous', 'W' = 'Woody')

dat$variable = key[dat$var]
dat$trt <-factor(dat$trt, levels = c('C', 'M', 'P', 'B'))
setorder(dat, variable, trt)


dotchart(dat$value, labels = dat$trt, groups = factor(dat$variable), pch = 16, xlim = c(-.5, .5))
abline(v = 0, lty = 2)
y = 1:4 + rep( c(0, 6, 12, 18), each = 4)
y= c(  19, 20, 21, 22,      13, 14, 15, 16,   7, 8, 9, 10,     1, 2, 3, 4)
arrows(dat$dwn, y, dat$up, y, length = 0)

polygon( c(-.54, .75, .75, -.54), c(.5,.5, 4.5,4.5), col = rgb(1,0,0,.1), border = NA)
polygon( c(-.54, .75, .75, -.54), c(6.5,6.5, 10.5,10.5), col = rgb(0,1,0,.1), border = NA)
polygon( c(-.54, .75, .75, -.54), c(12.5,12.5, 16.5,16.5), col = rgb(0,0,1,.1), border = NA)
polygon( c(-.54, .75, .75, -.54), c(18.5,18.5, 22.5,22.5), col = rgb(0,0,0,.1), border = NA)
title('% Cover Change 2009-2010')

dat$treat <- factor(dat$trt, levels = rev(c('P','M', 'C', 'B')))

xn <- -50
xx <- 50

g1 <- ggplot(dat[ variable == 'Bare Ground',], aes( x = -1* 100* value, y = treat)) + geom_point(size = 3) + 
geom_errorbarh(aes(xmin=up*-100, xmax=dwn*-100), colour="black", height=0, size = 1) + theme(
        panel.background = element_rect(fill = bgcol, colour = linecol2),
        panel.grid.minor = element_line(size = (0.2), colour=linecol2),
        panel.grid.major = element_line(size = (0.7), colour=linecol2)
        ) + 
        xlab('') + xlim(xn, xx) + ylab('') + ggtitle("Change in Total Ground Cover (%)") 

g1

g2 <- ggplot(dat[ variable == 'Herbaceous',], aes( x = 100* value, y = treat)) + geom_point(size = 3) + 
geom_errorbarh(aes(xmin=up*100, xmax=dwn*100), colour="black", height=0, size = 1) + theme(
        panel.background = element_rect(fill = bgcol, colour = linecol2),
        panel.grid.minor = element_line(size = (0.2), colour=linecol2),
        panel.grid.major = element_line(size = (0.7), colour=linecol2)
        ) + 
        xlab('') + xlim(xn, xx) + ylab('') + ggtitle("Change in Herbaceous Cover (%)") 

g3 <- ggplot(dat[ variable == 'Litter',], aes( x = 100* value, y = treat)) + geom_point(size = 3) + 
geom_errorbarh(aes(xmin=up*100, xmax=dwn*100), colour="black", height=0, size = 1) + theme(
        panel.background = element_rect(fill = bgcol, colour = linecol2),
        panel.grid.minor = element_line(size = (0.2), colour=linecol2),
        panel.grid.major = element_line(size = (0.7), colour=linecol2)
        ) + 
        xlab('') + xlim(xn, xx) + ylab('') + ggtitle("Change in Litter Cover (%)") 


g4 <- ggplot(dat[ variable == 'Woody',], aes( x = 100* value, y = treat)) + geom_point(size = 3) + 
geom_errorbarh(aes(xmin=up*100, xmax=dwn*100), colour="black", height=0, size = 1) + theme(
        panel.background = element_rect(fill = bgcol, colour = linecol2),
        panel.grid.minor = element_line(size = (0.2), colour=linecol2),
        panel.grid.major = element_line(size = (0.7), colour=linecol2)
        ) + 
        xlab('') + xlim(xn, xx) + ylab('') + ggtitle("Change in Woody Cover (%)") 







require(gridExtra)
 
pdf(file.path("C:/projects/ssim/docs/figures/", paste0('joyResults', format(Sys.time(), '%Y-%m-%d'), '.pdf')), width = 8 , height = 11)
  
  # grid.arrange(p4,g1, p3,g2, p2,g3, p1,g4,  ncol = 2, nrow = 4)
  grid.arrange(p4, p3, p2,p1,g1,g3, g2,g4,  ncol = 2, nrow = 4)

dev.off()

