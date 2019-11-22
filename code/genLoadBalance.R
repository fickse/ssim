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

R <- data.table(RUN)
R$id <- 1:nrow(R)

outs <- list()

## timing stuff
i <- as.numeric( argv[1])

for( i in i:(i + ", chunkSize, ")){

    t0 <- Sys.time()
    print(i)
    print(RUN[i,])

    ## the business
    s <- simul_data( RUN[i,] )
    tr <- try({  ev <- evaluate2(s, se = FALSE)})
    if(class(tr) == 'try-error') next()
    
    
    ## process
    ev$id <- as.numeric(i)
    D <- data.table(ev)
    m <- melt(D, id.vars = c('time', 'trt', 'ctr', 'effect', 'id'))
    m$delta <- m$value - m$effect
    m$error <- abs(m$delta)
    M <- merge(R, m, by = 'id')
    outs[[as.character(i)]] <- M
    
}

    ## combine
    M <- do.call(rbind, outs)
    
    ## save
    tx <- Sys.time() - t0
    save(M, tx, file = outfile)


")

cat(string, file = file.path(outdir, 'loadBalance.R'))
