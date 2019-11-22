
# errors v2

cargs <- commandArgs(TRUE)

# input directory
h <- cargs[1]
#h <- '~/projects/NRCS/CODE/synth_control/outputs/run_stupid_20190614225749/data'
#h <- '~/projects/NRCS/CODE/synth_control/outputs/run_hires_20190615222214/data'
#setwd(h)
library(session)
restore.session('session.RData')

library(data.table)

#loop through and compile
cat('listing files\n');flush.console()
ff <- list.files(dataDir, pattern = 'RData', full = TRUE)
dat <- list()
outd <- file.path(dataDir,'..')
dir.create(outd)

#ff <- ff[1:1000]

for( f in ff){

      cat(f,'\n');flush.console()
      load(f)
      #ev <- ev[which(ev$time >=0),]
      M <- M[time > -1,]
      dat[[basename(f)]] <- M
      gc()

}

cat('Rbinding\n');flush.console()
D <- do.call(rbind, dat)
save(D, file = file.path(outd, paste0( basename(dataDir), '.RData')))
