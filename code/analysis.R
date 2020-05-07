# Analysis V1

wd <- "/home/sfick/ssim/code/outputs/r1"
setwd(wd)

library(session)
restore.session('session.RData')

library(data.table)
library(RSQLite)
library(ggplot2)

outd <- file.path(dataDir,'..')
dbFile <- file.path(outd, paste0(basename(dataDir),'_a.sqlite'))
datFile <- file.path(outd, paste0(basename(dataDir), '_a.RData'))
reload_data <- FALSE

out <- file.path(wd, 'results')
dir.create(out)

################################################################################
################################################################################

# gather data

if( reload_data ) {
 
 CON <- dbConnect(RSQLite::SQLite(), dbFile)

 status <- "   
            SELECT COUNT(time)
             FROM
               response
             WHERE
               time > -100
             GROUP BY
                ID
        "
  stat <- data.table( dbGetQuery(CON, status))
 
 qry <- "   SELECT
               *
             FROM
               response
             WHERE
               time > -100
        "
  
 D<- data.table( dbGetQuery(CON, qry))

 #save 
 save(D, file = datFile)
 
} else {

   load( datFile)
   
}




################################################################################
################################################################################
 
 ## Error Plots
 
 clrz <- c('#ef476f','#06d6a0','#118ab2','#073b4c','#ffd166')
 setwd(out)



 wa <- function(x, n){
   # weighted average
   sum(x*n) / sum(n)

 }

R <- data.table(RUN)
R$ID <- 1:nrow(R)
#X <- merge(D, R[, list(ID, sdNoise, nControl, misMatch, type)], by = 'ID')

#X <- D
#X[, sigNoise := abs( treat / sdNoise) ]
 
# X <-  X[which(X$time > 0),]
 
# bins <- c(0,.001, .01, .05, .1, .5, 1, 5, 10, 50, 100) 
 #bins <- c(-1,.00001, seq(.01, 10, by = .01)) 
# X[, sigBin := cut(sigNoise, bins)]
 
 # cols_chosen <- c("ERR_bfast", "ERR_DD", "ERR_GS", "ERR_CI")
 # X[, winner := which.min(abs(.SD)), by = list(ID, date), .SDcols = cols_chosen]


 #X[, winner := which.min(c(abs(ERR_bfast),abs(ERR_DD),abs(ERR_GS),abs(ERR_CI), abs(ERR_ITS))), by = list(ID, date)]

 D[ , wa := wa(ERR.abs, N), by = list(ID,  method, sigBin)]
 D[, won := wa ==  min(wa), by = list(ID, sigBin)]


################################################################################
################################################################################
 
 ## check status
 R <- data.table(RUN)
 R$ID <- 1:nrow(R)

 D <- merge(D, R, by = 'ID')
 
 N = D[,.N, by = list(sdNoise, nControl, misMatch, type)]
 N0 = R[, .N, by = list(sdNoise, nControl, misMatch, type)]

 dcast(N,  nControl ~ sdNoise, value.var = c('N'), fun = sum)
 dcast(N0,  nControl ~ sdNoise, value.var = c('N'), fun = sum)


 dcast(N,  type ~ sdNoise, value.var = c('N'), fun = sum)
 dcast(N0,  type ~ sdNoise, value.var = c('N'), fun = sum)


 dcast(N,  type ~ misMatch, value.var = c('N'), fun = sum)
 dcast(N0,  type ~ misMatch, value.var = c('N'), fun = sum)


  # which ids are missing?
  s <- setdiff(R$ID, D$ID)

################################################################################
################################################################################




 normal <- which(D$auto_range > 0 )
 X <- D[normal,]
 



 ################################################################################
 
 # Error by signal/noise ratio x method
 
" 
 V <- X[ , list( gsynth = mean(abs( ERR_GS)),
              bfast = mean(abs(ERR_bfast)), 
              DiD = mean(abs(ERR_DD)), 
              CausalImpact = mean(abs(ERR_CI)),
              pGS = mean( InCI_GS),
              pDD = mean( InCI_DD),
              pCI = mean( InCI_CI)
            ) , by = list(sigBin) ]

  
  V <- melt(V, id.vars = c('sigBin'))
"

  V <- X[,
         list( value = wa(ERR.abs, N), ave = wa(ERR.ave, N)),
         by = list( sigBin, variable =method)]

                               
  vars <- c('gsynth', 'bfast', 'DiD', 'CausalImpact')
  vars <- unique(V$variable)

  pdf('fig_err_x_sigNoise_all.pdf')
 # png('fig_err_x_sigNoise_all.png')
  
    ggplot( V[variable %in% vars, ] , aes (x = sigBin, y = value, group = variable) ) + 
        geom_line(aes(color = variable))
    
  dev.off()


################################################################################
################################################################################
 
 
 # Error by signal/noise ratio x method Across nControl
 " 
 V <- X[ , list( gsynth = mean(abs( ERR_GS)),
              bfast = mean(abs(ERR_bfast)), 
              DiD = mean(abs(ERR_DD)), 
              CausalImpact = mean(abs(ERR_CI)),
              pGS = mean( InCI_GS),
              pDD = mean( InCI_DD),
              pCI = mean( InCI_CI)
            ) , by = list(sigBin, nControl) ]

  
  V <- melt(V, id.vars = c('sigBin', 'nControl'))
  vars <- c('gsynth', 'bfast', 'DiD', 'CausalImpact')

"

  V <- X[,
         list( value = wa(abs, N), ave = wa(ERR.ave, N)),
         by = list( sigBin,nControl, variable =method)]



  pdf('fig_err_x_sigNoise_nControl.pdf')
  
    ggplot( V[variable %in% vars, ] , aes (x = sigBin, y = value, group = variable) ) + 
        geom_line(aes(color = variable))+ facet_wrap(~ nControl)
    
  dev.off()
  
################################################################################
################################################################################
 

 # Error by signal/noise ratio x method Across misMatch. Ncontrol = 50
"  
 V <- X[nControl %in% c(1,5,50, 100)  , list( gsynth = mean(abs( ERR_GS)),
              bfast = mean(abs(ERR_bfast)), 
              DiD = mean(abs(ERR_DD)), 
              CausalImpact = mean(abs(ERR_CI)),
              pGS = mean( InCI_GS),
              pDD = mean( InCI_DD),
              pCI = mean( InCI_CI)
            ) , by = list(sigBin, misMatch, nControl) ]

  
  V <- melt(V, id.vars = c('sigBin', 'misMatch', 'nControl'))
  vars <- c('gsynth', 'bfast', 'DiD', 'CausalImpact')
 "
  V <- X[ which(X$nControl %in% c(1,5,50, 100)),
         list( value = wa(ERR.abs, N), ave = wa(ERR.ave, N)),
         by = list( sigBin,misMatch,nControl, variable =method)]


  pdf('fig_err_x_sigNoise_mismatch_nControl.pdf')
  
    ggplot( V[variable %in% vars, ] , aes (x = sigBin, y = value, group = variable) ) + 
        geom_line(aes(color = variable, linetype= variable))+ theme_bw() +
        facet_grid(  misMatch ~ nControl, labeller=label_both)+
        theme(legend.position = c(.12,.88)) + 
        theme(legend.title = element_blank()) + 
        theme(legend.box.background = element_rect(colour = "black"))+
        theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5))+
        xlab('Signal / noise ratio') + ylab ('Absolute Error') + 
        scale_linetype_manual(values=c("longdash", "solid","twodash", "dotted"))+
        scale_color_manual(values=clrz)+ 
        theme(strip.background = element_rect(colour="black", fill="white",linetype="solid")) 
  dev.off()
  
  
################################################################################
################################################################################
 

 # Chosen by signal/noise ratio x method Across misMatch. Ncontrol = 50
"  
   V <- X[ , list( 
              gsynth = sum(length(which( winner == 3)))/.N*100,
              bfast = sum(length(which( winner == 1)))/.N*100,
              DiD = sum(length(which( winner == 2)))/.N*100,
              CausalImpact = sum(length(which( winner == 4)))/.N*100)
             , by = list(sigBin, nControl, misMatch) ]

   V <- melt(V, id.vars = c('sigBin', 'misMatch', 'nControl'))



   V$variable = factor(V$variable, levels = rev(c('bfast', 'DiD', 'gsynth', 'CausalImpact')))
"
 
    
    V <- X[,
         list( value = sum(won)/.N*100),
         by = list( sigBin,nControl, variable =method)]



  pdf('fig_stacked_sigNoise_mismatch_nControl.pdf')
  
    ggplot( V , aes (x = as.numeric(sigBin), y = value, fill = variable) ) + 
        geom_area()+ theme_bw() +
        facet_grid(  misMatch ~ nControl, labeller=label_both)+
        #theme(legend.position = c(.12,.88)) + 
        theme(legend.title = element_blank()) + 
        theme(legend.box.background = element_rect(colour = "black"))+
        theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5))+
        xlab('Signal / noise ratio') + ylab ('% lowest error') + 
        #scale_linetype_manual(values=c("longdash", "solid","twodash", "dotted"))+
        scale_fill_manual(values=clrz[c(4,1,3,2)])+ 
        theme(strip.background = element_rect(colour="black", fill="white",linetype="solid"))+ 
        scale_x_continuous(breaks = c(2,4,6,8), labels=bins[c(3,5,7,9)])
  dev.off()

  
################################################################################
################################################################################p


  # Overlaps Zero
#  V <- X[ , list( 
#             gsynth = 100-sum(CIgt0_GS)/.N*100,
#             DiD = 100 - sum(CIgt0_DD)/.N*100,
#             CausalImpact = 100 - sum(CIgt0_CI)/.N*100)
#            , by = list(sigBin, nControl, misMatch) ]
#  V <- melt(V, id.vars = c('sigBin', 'misMatch', 'nControl'))

  V <- X[,
         list( value = wa(CIgt0.ave, N)*100),
         by = list( sigBin,misMatch,nControl, variable =method)]



    pdf('fig_sens_x_sigNoise_mismatch_nControl.pdf')
  
    ggplot( V, aes (x = sigBin, y = value, group = variable) ) + 
        geom_line(aes(color = variable, linetype= variable))+ theme_bw() +
        facet_grid(  misMatch ~ nControl, labeller=label_both)+
        theme(legend.position = c(.12,.88)) + 
        theme(legend.title = element_blank()) + 
        theme(legend.box.background = element_rect(colour = "black"))+
        theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5))+
        xlab('Signal / noise ratio') + ylab ('Absolute Error') + 
        scale_linetype_manual(values=c("longdash","twodash", "dotted"))+
        scale_color_manual(values=clrz)+ 
        theme(strip.background = element_rect(colour="black", fill="white",linetype="solid")) 
  dev.off()
 ###########################

  # Overlaps True
"   V <- X[ , list( 
              gsynth = 100-sum(InCI_GS)/.N*100,
              DiD = 100 - sum(InCI_DD)/.N*100,
              CausalImpact = 100 - sum(InCI_CI)/.N*100)
             , by = list(sigBin, nControl, misMatch) ]
   V <- melt(V, id.vars = c('sigBin', 'misMatch', 'nControl'))
 "

  V <- X[,
         list( value = wa(InCI.ave, N)*100),
         by = list( sigBin,misMatch,nControl, variable =method)]


    pdf('fig_ballpark_x_sigNoise_mismatch_nControl.pdf')
  
    ggplot( V, aes (x = sigBin, y = value, group = variable) ) + 
        geom_line(aes(color = variable, linetype= variable))+ theme_bw() +
        facet_grid(  misMatch ~ nControl, labeller=label_both)+
        theme(legend.position = c(.12,.88)) + 
        theme(legend.title = element_blank()) + 
        theme(legend.box.background = element_rect(colour = "black"))+
        theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5))+
        xlab('Signal / noise ratio') + ylab ('Absolute Error') + 
        scale_linetype_manual(values=c("longdash","twodash", "dotted"))+
        scale_color_manual(values=clrz)+ 
        theme(strip.background = element_rect(colour="black", fill="white",linetype="solid")) 
  dev.off()
 
 
 ###########################
    # Overlaps True
"
V <- X[ , list( 
              gsynth = 100-sum(InCI_GS * CIgt0_GS)/.N*100,
              DiD = 100 - sum(InCI_DD * CIgt0_DD)/.N*100,
              CausalImpact = 100 - sum(InCI_CI *CIgt0_CI)/.N*100)
             , by = list(sigBin, nControl, misMatch) ]
   V <- melt(V, id.vars = c('sigBin', 'misMatch', 'nControl'))
"
V <- X[,
         list( value = wa(InCI.ave, N)*100),
         by = list( sigBin,misMatch,nControl, variable =method)]

    pdf('fig_specificity_x_sigNoise_mismatch_nControl.pdf')
  
    ggplot( V, aes (x = sigBin, y = value, group = variable) ) + 
        geom_line(aes(color = variable, linetype= variable))+ theme_bw() +
        facet_grid(  misMatch ~ nControl, labeller=label_both)+
        theme(legend.position = c(.12,.88)) + 
        theme(legend.title = element_blank()) + 
        theme(legend.box.background = element_rect(colour = "black"))+
        theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5))+
        xlab('Signal / noise ratio') + ylab ('Absolute Error') + 
        scale_linetype_manual(values=c("longdash","twodash", "dotted"))+
        scale_color_manual(values=clrz)+ 
        theme(strip.background = element_rect(colour="black", fill="white",linetype="solid")) 
  dev.off()
 
 ##################################
" V <- X[ nControl > 5 , list( 
              gsynth = 100-sum(CIgt0_GS, na.rm= TRUE)/.N*100,
              DiD = 100 - sum(CIgt0_DD)/.N*100,
              CausalImpact = 100 - sum(CIgt0_CI)/.N*100)
             , by = list(sigBin,  misMatch, nControl) ]
   V <- melt(V, id.vars = c('sigBin', 'misMatch', 'nControl'))
"
   V <- X[ nControl > 5,
         list( value = wa(CIgt0.ave, N)*100),
         by = list( sigBin,misMatch,nControl, variable =method)]
  
    pdf('fig_specificity_x_nControl_mismatch.pdf')
  
    ggplot( V, aes (x = sigBin, y = value, group = variable) ) + 
        geom_line(aes(color = variable, linetype= variable))+ theme_bw() +
        facet_grid(  misMatch ~ nControl , labeller=label_both)+
        theme(legend.position = 'top') + 
        theme(legend.title = element_blank()) + 
        theme(legend.box.background = element_rect(colour = "black"))+
        theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5))+
        xlab('Signal / noise ratio') + ylab ('Proportion of intervals excluding zero') + 
        scale_linetype_manual(values=c("longdash","twodash", "dotted"))+
        scale_color_manual(values=clrz)+ 
        theme(strip.background = element_rect(colour="black", fill="white",linetype="solid")) 
  dev.off()
 
 ###########################
 # error as a function of confounding
 "
 X$confounder <- abs( X$season + X$climate + X$drift + X$satellite) 
 
 cclasses <- c( -1, .001, 0.01, 0.1, 0.5, 1.5)
 X[, conBin := cut(confounder, cclasses)]


 
 V <- X[ , list( gsynth = mean(abs( ERR_GS)),
              bfast = mean(abs(ERR_bfast)), 
              DiD = mean(abs(ERR_DD)), 
              CausalImpact = mean(abs(ERR_CI)),
              pGS = mean( InCI_GS),
              pDD = mean( InCI_DD),
              pCI = mean( InCI_CI)
            ) , by = list(conBin) ]


  
  V <- melt(V, id.vars = c('conBin'))
  vars <- c('gsynth', 'bfast', 'DiD', 'CausalImpact')
"


  V <- X[,
         list( value = wa(ERR.abs, N)),
         by = list( conBin,variable =method)]

  pdf('fig_err_x_confounder_all.pdf')
 # png('fig_err_x_sigNoise_all.png')
  
    ggplot( V[variable %in% vars, ] , aes (x = conBin, y = value, group = variable) ) + 
        geom_line(aes(color = variable))
    
  dev.off()

 
 #######
 # by noise and mismatch
 "
 V <- X[ nControl > 1, list( gsynth = mean(abs( ERR_GS)),
              bfast = mean(abs(ERR_bfast)), 
              DiD = mean(abs(ERR_DD)), 
              CausalImpact = mean(abs(ERR_CI)),
              pGS = mean( InCI_GS),
              pDD = mean( InCI_DD),
              pCI = mean( InCI_CI)
            ) , by = list(conBin, sdNoise, misMatch ) ]

  
  V <- melt(V, id.vars = c('conBin', 'sdNoise', 'misMatch'))
  vars <- c('gsynth', 'bfast', 'DiD', 'CausalImpact')

"
  V <- X[,
         list( value = wa(ERR.abs, N)),
         by = list( conBin,misMatch,sdNoise, variable =method)]

pdf('fig_err_x_Confounder_mismatch_sdNoise.pdf')
  
    ggplot( V[variable %in% vars, ] , aes (x = conBin, y = value, group = variable) ) + 
        geom_line(aes(color = variable, linetype= variable))+ theme_bw() +
        facet_grid(  misMatch ~ sdNoise, labeller=label_both)+
        theme(legend.position = c(.12,.88)) + 
        theme(legend.title = element_blank()) + 
        theme(legend.box.background = element_rect(colour = "black"))+
        theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5))+
        xlab('confounder intensity') + ylab ('Absolute Error') + 
        scale_linetype_manual(values=c("longdash", "solid","twodash", "dotted"))+
        scale_color_manual(values=clrz)+ 
        theme(strip.background = element_rect(colour="black", fill="white",linetype="solid")) 
  dev.off()
  
   
 #######
 # CI by confouder and sigBin
 "
 V <- X[ nControl > 5 , list(
              gsynth = 100-sum(CIgt0_GS, na.rm= TRUE)/.N*100,
              DiD = 100 - sum(CIgt0_DD)/.N*100,
              CausalImpact = 100 - sum(CIgt0_CI)/.N*100)
             , by = list(sigBin, conBin, misMatch) ]

  
  V <- melt(V, id.vars = c('conBin', 'sigBin', 'misMatch'))
  vars <- c('gsynth', 'bfast', 'DiD', 'CausalImpact')

"

  V <- X[,
         list( value = wa(CIgt0, N)*100),
         by = list( conBin,misMatch,sigBin, variable =method)]

  pdf('fig_CI_x_Confounder_sigNoise.pdf')
  
    ggplot( V[variable %in% vars, ] , aes (x = sigBin, y = value, group = variable) ) + 
        geom_line(aes(color = variable, linetype= variable))+ theme_bw() +
        facet_grid(  misMatch ~ conBin, labeller=label_both)+
        theme(legend.position = c(.12,.88)) + 
        theme(legend.title = element_blank()) + 
        theme(legend.box.background = element_rect(colour = "black"))+
        theme(axis.text.x = element_text(angle = 45, hjust = .5, vjust = .5))+
        xlab('confounder intensity') + ylab ('Absolute Error') + 
        scale_linetype_manual(values=c("longdash", "solid","twodash", "dotted"))+
        scale_color_manual(values=clrz)+ 
        theme(strip.background = element_rect(colour="black", fill="white",linetype="solid")) 
  dev.off()
  
  

 
