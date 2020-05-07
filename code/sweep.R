library(session)
restore.session('session.RData')

library(data.table)
library(RSQLite)

#loop through and compile
cat('listing files\n');flush.console()
ff <- list.files(dataDir, pattern = 'RData', full = TRUE)

dat <- list()
outd <- file.path(dataDir,'..')
dir.create(outd)

outf <- file.path(outd, paste0( basename(dataDir), '_a.RData'))
dbFile <- gsub('[.]RData', '.sqlite', outf)

# connect to db
CON <- dbConnect(RSQLite::SQLite(), dbFile)
reload <- TRUE
if(reload){

d <- list()
counter = 0
for( f in ff ){
      counter <- counter + 1
      cat(f,'\n');flush.console()
      load(f)
      #ev <- ev[which(ev$time >=0),]
      M$time <- M$date - ddate
      M[, sigNoise := abs( treat / sdNoise) ]
      M$confounder <- abs( M$season + M$climate + M$drift + M$satellite) 

     ## transform to id, time, method, var, value
     cols = c('time','date','sigNoise','confounder', 'ID',grep('ERR', names(M), v=T),grep('InCI', names(M), v = T), grep('CIgt0', names(M), v = T))

     m <- M[ time > 0, ..cols]


     m <- melt(m, id = c('date', 'time', 'ID', 'sigNoise', 'confounder'))
      
     m$method <- substr(gsub('.*?_', '', m$variable),1,2)
     m$variable <- gsub('_.*', '', m$variable)
     m$value <- round(m$value, 5)
     m$time <- as.numeric(m$time)
     bins <- c(0,.001, .01, .05, .1, .5, 1, 5, 10, 50, 100, Inf) 

     m[, sigBin := cut(sigNoise, bins)]

     cclasses <- c( -1, .001, 0.01, 0.1, 0.5, 1, 1.5, Inf)
     m[, conBin := cut(confounder, cclasses)]

     m <- m[, list(abs = mean(abs(value)), ave = mean(value), .N ), by = list(ID, variable, method, sigBin, conBin)]

     m <- melt(m, id = c('ID', 'variable', 'method', 'sigBin', 'conBin', 'N'))
     m$variable <- paste0(m$variable, '.', m$variable.1)
     m <-  dcast(m, ID  + method + sigBin + conBin + N ~ variable, value.var = "value")

     m$CIgt0.abs <- m$InCI.abs <- NULL

     #m$vv <- paste0(m$variable, '_', m$method)
     #m <- dcast(m, ID + date + time ~ vv, value.var = 'value')      
      # Error = predicted - actual
      # in CI = actual in CI
      
#      M$ERR_bfast <- as.numeric(M$bfast) - M$treat
#      M$ERR_DD <- M$DDatt - M$treat
#      M$ERR_CI <- M$CIpoint.effect - M$treat
#      M$ERR_GS <- M$GSATT - M$treat
#
#      # In CI
#      M$InCI_DD <-  M$treat > M$DDupr & M$treat < M$DDlwr
#      M$InCI_GS <-  M$treat < M$GSCI.upper & M$treat > M$GSCI.lower
#      M$InCI_CI <-  M$treat < M$CIpoint.effect.upper & M$treat > M$CIpoint.effect.lower
#
      
#      M <- M[time > -50,]
      d[[f]] <- m

 #      dat[[basename(f)]] <- M
      if( counter %% 500 == 0 ){
        cat('  rbinding\n')
        dx <- do.call(rbind, d)
        res <- dbWriteTable(CON, "dat" , value = dx, append = TRUE)
        d <- list()
        gc()
      }
}


    dx <- do.call(rbind, d)
    res <- dbWriteTable(CON, "dat" , value = dx, append = TRUE)
}
    cat('extracting\n')

    qry <- paste0( " SELECT * FROM dat;")
    D <- data.table( dbGetQuery( CON, qry))


save(D, file = outf)

