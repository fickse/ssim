
d <- read.csv('clipboard', header = F, strings = F)
d$var <- substr(1,1, d$V1)
d$var <- substr(d$V1, 1, )
d$var <- substr(d$V1, 1,1 )

d$trt <- substr(d$V1, 2,2 )
d$se <- substr(d$V1, 3,3 )
d$se <- ifelse(d$se == '', FALSE, TRUE)

D <- data.table(d)
D$V1 <- NULL
setnames(D, 'V2', 'value')

D[, list( value = value[which(!se)], se = value[which(se)] - value[which(!se)] ) , by = list(var, trt)]

dat <- D[, list( value = value[which(!se)], se = value[which(se)] - value[which(!se)] ) , by = list(var, trt)]

write.csv(dat, 'C:/projects/ssim/docs/KARL2014.csv')
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
