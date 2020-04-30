#chunkSize = 1000

string = paste0( "

datadir = '", dataDir, "'

## Pass in row to run
argv = commandArgs(TRUE)

## Check if outfile exists
i <- as.numeric( argv[1])
outfile = file.path(datadir, paste0(i, '.RData'))
if(file.exists(outfile)) quit('no')

## load session
library(session)
restore.session('session.RData')
library(data.table)
library(RSQLite)

R <- data.table(RUN)
R$ID <- 1:nrow(R)

outs <- list()

## timing stuff
i <- as.numeric( argv[1])
job <- jobs[jobs$job== i,]
cat('job:\n')
job


## writing outputs
outd <- file.path(dataDir,'..')
dbFile <- file.path(outd, paste0(basename(dataDir),'.sqlite'))


for( i in job$start:job$end){

    t0 <- Sys.time()
    print(i)
    print(RUN[i,])

    ## the business
    s <- simul_data( RUN[i,] )
    tr <- try({  ev <- evaluate(s)})
    if(class(tr) == 'try-error'){ cat('\n\n   FAILED \n\n'); next()}
    cat('\n SUCESS!\n')
    
    ## process
    ev$ID <- as.numeric(i)
    M <- merge(R, ev, by = 'ID')
    outs[[as.character(i)]] <- M
    
    cat('\n\n', (Sys.time() - t0), '\n--------------\n')
    
}

    ## combine
    M <- do.call(rbind, outs)
    
      ## process
      M$time <- M$date - ddate
      
      # Error = predicted - actual
      M$ERR_bfast <- as.numeric(M$bfast) - M$treat
      M$ERR_DD <- M$DDatt - M$treat
      M$ERR_CI <- M$CIpoint.effect - M$treat
      M$ERR_GS <- M$GSATT - M$treat

      # treatment value In CI
      
      M$InCI_DD <-  M$treat > M$DDupr & M$treat < M$DDlwr
      M$InCI_GS <-  M$treat < M$GSCI.upper & M$treat > M$GSCI.lower
      M$InCI_CI <-  M$treat < M$CIpoint.effect.upper & M$treat > M$CIpoint.effect.lower
      
      M$CIgt0_DD <- 0 > M$DDupr & 0 < M$DDlwr
      M$CIgt0_GS <-  0 < M$GSCI.upper & 0 > M$GSCI.lower
      M$CIgt0_CI <-  0 < M$CIpoint.effect.upper & 0 > M$CIpoint.effect.lower


      keep <- c('ID', 'sim', 'date', 'time', 'season',
                'climate', 'treat', 'satellite', 'drift', 
                 'ERR_bfast', 'ERR_DD', 'ERR_CI', 'ERR_GS', 
                 'InCI_DD','InCI_GS', 'InCI_CI', 'CIgt0_DD',
                 'CIgt0_GS', 'CIgt0_CI')
      
      m <- M[, keep, with = F]
    
      # connect to db
      CON <- dbConnect(RSQLite::SQLite(), dbFile)
    
      # write to tables
      cat('\nwriting to table response in ', dbFile, '\n')

      retry <- TRUE
      while( retry ){
      
            res <- try({ dbWriteTable(CON, 'response', value = m, append = TRUE) } )
            
            if( class(res) != 'try-error' ) { cat('\n     success!\n'); retry <- FALSE 
            
            }else {
              cat('\n    write try failed. Sleeping 5 secs\n')
            }
            Sys.sleep(5)
      }



    tx <- Sys.time() - t0
    save(M, tx, file = outfile)


")

cat(string, file = file.path(outdir, 'loadBalance.R'))
