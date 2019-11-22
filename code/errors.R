
# errors v2

cargs <- commandArgs(TRUE)

# input directory
h <- cargs[1]
#h <- '~/projects/NRCS/CODE/synth_control/outputs/run_stupid_20190614225749/data'
#h <- '~/projects/NRCS/CODE/synth_control/outputs/run_hires_20190615222214/data'
setwd(h)
library(session)
restore.session('session.RData')

library(data.table)

# iter info
#load('../permutations.RData')
R <- data.table(RUN)
R$id <- 1:nrow(R)

#loop through and compile
cat('listing files\n');flush.console()
ff <- list.files(dataDir, pattern = 'RData', full = TRUE)
dat <- list()
counter = 0
batch = 1
dir.create('outputs')

#ff <- ff[1:1000]

for( f in ff){
#      cat(f,'\n')
      counter = counter + 1
      id <- gsub('[.].*', '', basename(f))
      cat('                 ',id, '\r'); flush.console()
      load(f)
      ev$id <- as.numeric(id)
      #ev <- ev[which(ev$time >=0),]
      ev <- data.table(ev)
      m <- melt(ev, id.vars = c('time', 'trt', 'ctr', 'effect', 'id'))
      m$delta <- m$value - m$effect
      m$error <- abs(m$delta)

      dat[[basename(f)]]<- m

      if( counter %% 1e4 == 0 | counter == length(ff)){
          # stash result
          cat('stashing batch ', batch, '\n'); flush.console()
          if(!file.exists(paste0('outputs/errors', batch, '.RData'))){
             D <- do.call(rbind, dat)
             M <- merge(R,D, by = 'id')
             save(M, file = paste0('outputs/errors', batch, '.RData'))
          }
          batch = batch + 1
          dat <- list()
          gc()
      }

}

ff <- list.files('outputs', full = TRUE)
K <- list()
for( i in 1:length(ff)){

  load(ff[i])

=======
  M <- M[which(M$time > -1),]
  K[[i]] <- M

}

M <- do.call(rbind, K)

save(M, 'errors.RData')
