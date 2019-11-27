library(session)
restore.session('session.RData')

library(data.table)

#loop through and compile
cat('listing files\n');flush.console()
ff <- list.files(dataDir, pattern = 'RData', full = TRUE)

dat <- list()
outd <- file.path(dataDir,'..')
dir.create(outd)

for( f in ff){

      cat(f,'\n');flush.console()
      load(f)
      #ev <- ev[which(ev$time >=0),]
      M$time <- M$date - ddate
      
      # Error = predicted - actual
      # in CI = actual in CI
      
      M$ERR_bfast <- as.numeric(M$bfast) - M$treat
      M$ERR_DD <- M$DDatt - M$treat
      M$ERR_CI <- M$CIpoint.effect - M$treat
      M$ERR_GS <- M$GSATT - M$treat

      # In CI
      M$InCI_DD <-  M$treat > M$DDupr & M$treat < M$DDlwr
      M$InCI_GS <-  M$treat < M$GSCI.upper & M$treat > M$GSCI.lower
      M$InCI_CI <-  M$treat < M$CIpoint.effect.upper & M$treat > M$CIpoint.effect.lower
      
      
      dat[[basename(f)]] <- M
      gc()

}

cat('Rbinding\n');flush.console()
D <- do.call(rbind, dat)
save(D, file = file.path(outd, paste0( basename(dataDir), '.RData')))

