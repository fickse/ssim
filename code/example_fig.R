# demonstration plots

source('config.R')
source('simulation_functions.R')
source('eval_functions.R')
library(data.table); library(ggplot2)
R <- data.table(RUN)



#####################
#####################
 clrz <- c('gsynth' = '#ef476f','bfast' = '#06d6a0','DiD'='#118ab2','CausalImpact' = '#073b4c','other'='#ffd166')

 lwd0 = .25
 lwd1 = .25
#####################
#####################
 




x0 = list(sim = 117L, type = 'forest', sdNoise = 0.01, disturbance = 0.1,
    nControl = 20, misMatch = 0, climSD = 0.1, climCenter = 10,
    satLambda = 0.05, rwSD = 0.01, affinitySD = 0.25, timeVaryingAffinitySD = 0.05,
    randConstantSD = 0.05, overrideNoise = TRUE)
 
 
 x1 = list(sim = 117L, type = 'forest', sdNoise = 0.01, disturbance = 0.1,
    nControl = 20, misMatch = .5, climSD = 0.1, climCenter = 10,
    satLambda = 0.05, rwSD = 0.01, affinitySD = 0.25, timeVaryingAffinitySD = 0.05,
    randConstantSD = 0.05, overrideNoise = TRUE)
    
 x2 = list(sim = 117L, type = 'forest', sdNoise = 0.01, disturbance = 0.1,
    nControl = 20, misMatch = 1, climSD = 0.1, climCenter = 10,
    satLambda = 0.05, rwSD = 0.01, affinitySD = 0.25, timeVaryingAffinitySD = 0.05,
    randConstantSD = 0.05, overrideNoise = TRUE)
    
 v = simul_data(x0)
 D <- melt(v$truth, id.var = 'date')
  
  # get forest vs grassland
  v$D$kind <- ifelse(grepl('tr', v$D$id),'Treated', 'forest')
  v$D$kind <- as.factor(v$D$kind)
 #, levels = c('Treated', 'forest', 'grassland'))
 pdf('example.pdf')
 ggplot(D, aes(x = date, y = value)) + geom_line(aes(color = variable)) + 
         facet_grid(variable ~ ., scales = 'free_y') + theme(legend.position = "none")+ theme_bw() +
         theme(legend.position = "none")
 dev.off()
 
 
 ggl0  <- ggplot(v$D, aes(x = dayt, y = y, group = id, color = kind, linetype = kind)) + geom_line( size = lwd0 ) + 
 scale_color_manual(values = c( rgb(.3,.3, .3, .3), 'red'))+
 scale_linetype_manual(values=c("longdash",  "solid")) + theme_bw() + theme(legend.position = "none")+  ylab('NDVI') + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())


g <- evaluate.example(v)
x <- g[ , list(CIpoint.effect, DDatt, GSATT, bfast, date)]
names(x) <- c('CausalImpact', 'DiD', 'gsynth', 'bfast', 'date')
x <- melt(x, id = 'date')

x.u <- g[ , list(CIpoint.effect.upper, DDupr, GSCI.upper, bfast, date)]
names(x.u) <- c('CausalImpact', 'DiD', 'gsynth', 'bfast', 'date')
x.u <- melt(x.u, id = 'date')
names(x.u)[3] <- 'upper'

x.l <- g[ , list(CIpoint.effect.lower, DDlwr, GSCI.lower, bfast, date)]
names(x.l) <- c('CausalImpact', 'DiD', 'gsynth', 'bfast', 'date')
x.l <- melt(x.l, id = 'date')
names(x.l)[3] <- 'lower'

m <- merge(x, x.u, by = c('date', 'variable'))
m <- merge(m, x.l, by = c('date', 'variable'))


xy = v$truth[, list(treat, date)]
names(m)[1] <- 'dayt'

 ggci0 <-  ggplot(m[variable =='CausalImpact',], aes(x = dayt, y = value))   +   
          geom_line(size = lwd0, color =clrz['CausalImpact'] )  +  theme(legend.position = "none")+ 
          geom_ribbon(aes(ymin=lower, ymax=upper, linetype = NA, fill = variable), alpha = 0.2) +
          theme_bw()  + theme(legend.position = "none") + xlab('date') + ylab('CausalImpact') + 
          geom_vline(xintercept = ddate, linetype = 'dotted') + geom_line(data = xy, aes(x = date, y = treat), linetype = 'longdash', size = lwd1) + scale_fill_manual(values = clrz['CausalImpact']) + scale_color_manual(values = clrz['CausalImpact'])

 ggdd0 <-  ggplot(m[variable =='DiD',], aes(x = dayt, y = value))   +   
          geom_line(size = lwd0, color = clrz['DiD'])   +  theme(legend.position = "none")+ 
          geom_ribbon(aes(ymin=lower, ymax=upper, linetype = NA, fill = variable), alpha = 0.2) +
          theme_bw()  + theme(legend.position = "none") + ylab('DiD')+ theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+geom_vline(xintercept = ddate, linetype = 'dotted', size = lwd1) + geom_line(data = xy, aes(x = date, y = treat), linetype = 'longdash')+ scale_fill_manual(values = clrz['DiD'])

 gggs0 <-  ggplot(m[variable =='gsynth',], aes(x = dayt, y = value))   +   
          geom_line(size = lwd0, color = clrz['gsynth'])   +  theme(legend.position = "none")+ 
          geom_ribbon(aes(ymin=lower, ymax=upper, linetype = NA, fill = variable), alpha = 0.2) +
          theme_bw()  + theme(legend.position = "none") + ylab('gsynth')+ theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+geom_vline(xintercept = ddate, linetype = 'dotted') + geom_line(data = xy, aes(x = date, y = treat), linetype = 'longdash', size = lwd1)+ scale_fill_manual(values = clrz['gsynth'])

ggb0 <-  ggplot(m[variable =='bfast',], aes(x = dayt, y = value))   +   
          geom_line(size = lwd0, color = clrz['bfast'])   +  theme(legend.position = "none")+ 
          theme_bw()  + theme(legend.position = "none") + ylab('bfast')+ theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+geom_vline(xintercept = ddate, linetype = 'dotted') + geom_line(data = xy, aes(x = date, y = treat), linetype = 'longdash', size = lwd1)+ scale_fill_manual(values = clrz['bfast'])


library(grid)
grid.newpage()

r1 <- rbind(ggplotGrob(ggl), ggplotGrob(ggb), ggplotGrob(ggdd), ggplotGrob(gggs), ggplotGrob(ggci), size = "last")
pdf('example_noMismatch.pdf')
grid.draw(r1)
dev.off()

#######################
#######################
 v = simul_data(x1)
 D <- melt(v$truth, id.var = 'date')
 
  v$D[, kind := c('forest', 'grassland')[as.numeric(min(y) <.25) + 1], by = list(id)]
  v$D$kind <- ifelse(grepl('tr', v$D$id),'Treated', v$D$kind)
  v$D$kind <- as.factor(v$D$kind)

 ggl1  <- ggplot(v$D, aes(x = dayt, y = y, group = id, color = kind, linetype = kind)) + geom_line( size = lwd0 ) + 
 scale_color_manual(values = c( rgb(.3, .3,.3, .3), rgb(.3, .3, .3, .3), 'red'))+
 scale_linetype_manual(values=c("longdash", "longdash", "solid")) + theme_bw() + theme(legend.position = "none")+  ylab('') + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank()) 


g <- evaluate.example(v)
x <- g[ , list(CIpoint.effect, DDatt, GSATT, bfast, date)]
names(x) <- c('CausalImpact', 'DiD', 'gsynth', 'bfast', 'date')
x <- melt(x, id = 'date')

x.u <- g[ , list(CIpoint.effect.upper, DDupr, GSCI.upper, bfast, date)]
names(x.u) <- c('CausalImpact', 'DiD', 'gsynth', 'bfast', 'date')
x.u <- melt(x.u, id = 'date')
names(x.u)[3] <- 'upper'

x.l <- g[ , list(CIpoint.effect.lower, DDlwr, GSCI.lower, bfast, date)]
names(x.l) <- c('CausalImpact', 'DiD', 'gsynth', 'bfast', 'date')
x.l <- melt(x.l, id = 'date')
names(x.l)[3] <- 'lower'

m <- merge(x, x.u, by = c('date', 'variable'))
m <- merge(m, x.l, by = c('date', 'variable'))


xy = v$truth[, list(treat, date)]
names(m)[1] <- 'dayt'

 ggci1 <-  ggplot(m[variable =='CausalImpact',], aes(x = dayt, y = value))   +   
          geom_line(size = lwd0, color =clrz['CausalImpact'])  +  theme(legend.position = "none")+ ylab('') +
          geom_ribbon(aes(ymin=lower, ymax=upper, linetype = NA, fill = variable), alpha = 0.2) +
          theme_bw()  + theme(legend.position = "none") + xlab('date')  + 
          geom_vline(xintercept = ddate, linetype = 'dotted') + geom_line(data = xy, aes(x = date, y = treat), linetype = 'longdash', size = lwd1) + scale_fill_manual(values = clrz['CausalImpact']) + scale_color_manual(values = clrz['CausalImpact'])

 ggdd1 <-  ggplot(m[variable =='DiD',], aes(x = dayt, y = value))   +   
          geom_line(size = lwd0, color =clrz['DiD'])   +  theme(legend.position = "none")+ ylab('') +
          geom_ribbon(aes(ymin=lower, ymax=upper, linetype = NA, fill = variable), alpha = 0.2) +
          theme_bw()  + theme(legend.position = "none") + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+geom_vline(xintercept = ddate, linetype = 'dotted') + geom_line(data = xy, aes(x = date, y = treat), linetype = 'longdash', size = lwd1)+ scale_fill_manual(values = clrz['DiD'])

 gggs1 <-  ggplot(m[variable =='gsynth',], aes(x = dayt, y = value))   +   
          geom_line(size = lwd0, color =clrz['gsynth'])   +  theme(legend.position = "none")+ ylab('') +
          geom_ribbon(aes(ymin=lower, ymax=upper, linetype = NA, fill = variable), alpha = 0.2) +
          theme_bw()  + theme(legend.position = "none") + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+geom_vline(xintercept = ddate, linetype = 'dotted') + geom_line(data = xy, aes(x = date, y = treat), linetype = 'longdash', size = lwd1)+ scale_fill_manual(values = clrz['gsynth'])

ggb1 <-  ggplot(m[variable =='bfast',], aes(x = dayt, y = value))   +   
          geom_line(size = lwd0, color =clrz['bfast'])   +  theme(legend.position = "none")+ ylab('') +
          theme_bw()  + theme(legend.position = "none") + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+geom_vline(xintercept = ddate, linetype = 'dotted') + geom_line(data = xy, aes(x = date, y = treat), linetype = 'longdash', size = lwd1)+ scale_fill_manual(values = clrz['bfast'])

####################
####################

v = simul_data(x2)
 D <- melt(v$truth, id.var = 'date')
 
  v$D[, kind := c('forest', 'grassland')[as.numeric(min(y) <.25) + 1], by = list(id)]
  v$D$kind <- ifelse(grepl('tr', v$D$id),'Treated', v$D$kind)
  v$D$kind <- as.factor(v$D$kind)

 ggl2  <- ggplot(v$D, aes(x = dayt, y = y, group = id, color = kind, linetype = kind)) + geom_line( size = lwd0 ) + 
 scale_color_manual(values = c( rgb(.3, .3,.3, .3), 'red'))+
 scale_linetype_manual(values=c("longdash",  "solid")) + theme_bw() + theme(legend.position = "none")+  ylab('') + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())


g <- evaluate.example(v)
x <- g[ , list(CIpoint.effect, DDatt, GSATT, bfast, date)]
names(x) <- c('CausalImpact', 'DiD', 'gsynth', 'bfast', 'date')
x <- melt(x, id = 'date')

x.u <- g[ , list(CIpoint.effect.upper, DDupr, GSCI.upper, bfast, date)]
names(x.u) <- c('CausalImpact', 'DiD', 'gsynth', 'bfast', 'date')
x.u <- melt(x.u, id = 'date')
names(x.u)[3] <- 'upper'

x.l <- g[ , list(CIpoint.effect.lower, DDlwr, GSCI.lower, bfast, date)]
names(x.l) <- c('CausalImpact', 'DiD', 'gsynth', 'bfast', 'date')
x.l <- melt(x.l, id = 'date')
names(x.l)[3] <- 'lower'

m <- merge(x, x.u, by = c('date', 'variable'))
m <- merge(m, x.l, by = c('date', 'variable'))


xy = v$truth[, list(treat, date)]
names(m)[1] <- 'dayt'

 ggci2 <-  ggplot(m[variable =='CausalImpact',], aes(x = dayt, y = value))   +   
          geom_line(size = lwd0, color =clrz['CausalImpact'])  +  theme(legend.position = "none")+ 
          geom_ribbon(aes(ymin=lower, ymax=upper, linetype = NA, fill = variable), alpha = 0.2) +
          theme_bw()  + theme(legend.position = "none") + xlab('date')  +  ylab('') +
          geom_vline(xintercept = ddate, linetype = 'dotted') + geom_line(data = xy, aes(x = date, y = treat), linetype = 'longdash' , size = lwd1) + scale_fill_manual(values = clrz['CausalImpact']) + scale_color_manual(values = clrz['CausalImpact'])

 ggdd2 <-  ggplot(m[variable =='DiD',], aes(x = dayt, y = value))   +   
          geom_line(size = lwd0, color =clrz['DiD'])   +  theme(legend.position = "none")+ ylab('') +
          geom_ribbon(aes(ymin=lower, ymax=upper, linetype = NA, fill = variable), alpha = 0.2) +
          theme_bw()  + theme(legend.position = "none") + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+geom_vline(xintercept = ddate, linetype = 'dotted') + geom_line(data = xy, aes(x = date, y = treat), linetype = 'longdash', size = lwd1)+ scale_fill_manual(values = clrz['DiD'])

 gggs2 <-  ggplot(m[variable =='gsynth',], aes(x = dayt, y = value))   +   
          geom_line(size = lwd0, color =clrz['gsynth'])   +  theme(legend.position = "none")+ ylab('') +
          geom_ribbon(aes(ymin=lower, ymax=upper, linetype = NA, fill = variable), alpha = 0.2) +
          theme_bw()  + theme(legend.position = "none") + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+geom_vline(xintercept = ddate, linetype = 'dotted') + geom_line(data = xy, aes(x = date, y = treat), linetype = 'longdash', size = lwd1)+ scale_fill_manual(values = clrz['gsynth'])

ggb2 <-  ggplot(m[variable =='bfast',], aes(x = dayt, y = value))   +   
          geom_line(size = lwd0, color =clrz['bfast'])   +  theme(legend.position = "none")+ ylab('') +
          theme_bw()  + theme(legend.position = "none") + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+geom_vline(xintercept = ddate, linetype = 'dotted') + geom_line(data = xy, aes(x = date, y = treat), linetype = 'longdash', size = lwd1)+ scale_fill_manual(values = clrz['bfast'])


#####################################################

#####################################################

library(grid)
pdf('exampleAll.pdf', width= 8.5, height = 6)

grid.newpage()

pushViewport(viewport(layout = grid.layout(nrow = 5, ncol = 3)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 
# Arrange the plots
print(ggl0, vp = define_region(row = 1, col = 1))   # Span over two columns
print(ggb0, vp = define_region(row = 2, col = 1))
print(ggdd0, vp = define_region(row = 3, col = 1))
print(gggs0, vp = define_region(row = 4, col = 1))
print(ggci0, vp = define_region(row = 5, col = 1))


print(ggl1, vp = define_region(row = 1, col = 2))   # Span over two columns
print(ggb1, vp = define_region(row = 2, col = 2))
print(ggdd1, vp = define_region(row = 3, col = 2))
print(gggs1, vp = define_region(row = 4, col = 2))
print(ggci1, vp = define_region(row = 5, col = 2))


print(ggl2, vp = define_region(row = 1, col = 3))   # Span over two columns
print(ggb2, vp = define_region(row = 2, col = 3))
print(ggdd2, vp = define_region(row = 3, col = 3))
print(gggs2, vp = define_region(row = 4, col = 3))
print(ggci2, vp = define_region(row = 5, col = 3))

dev.off()
#####################################################

#####################################################

library(grid)
grid.newpage()
r2 <- rbind(ggplotGrob(ggl), ggplotGrob(ggb), ggplotGrob(ggdd), ggplotGrob(gggs), ggplotGrob(ggci), size = "last")


pdf('example_mismatch.pdf')
grid.draw(r2)
dev.off()




pdf('example2.pdf', width = 8.5, height = 3)
 ggl   
 dev.off()

pdf('exampleATT.pdf')
 ggc
dev.off()



