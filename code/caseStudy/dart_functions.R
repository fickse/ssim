

###########################################################
## DART function for parallel processing



dart_fn <- function(id) { # wids will be list for list apply and w the iterator
   
   polygons$id <- polygons@data[[dpar$polygon_id_column]]
   padpoly <- polygons[ which(polygons$id == id),]
  
   filename <- file.path( dpar$outdir, paste0(padpoly$key, '.RData'))
   if(file.exists(filename)){ return() }

   #internal timer
   tstart <- Sys.time()
   cat(Sys.time(), ' ', id, '\n', file = '~/snowlog.txt', append = TRUE)

   # get candidates
   tries <- dpar$tries
   while( tries > 0){
       t1 <- try({ get_candidates(dpar, id) })
       if(class(t1) == 'try-error'){
           if(grepl('(ERROR1|ERROR3)', t1)){
               cat('initial search for candidates failed. Doubling search radius\n');
               dpar$rad <- dpar$rad *2
               tries <- tries - 1
           } else {
               stop(t1)
           }
       } else if( nrow(t1$padbufsoilpixels) < nrow(t1$padpixels) & nrow(t1$padbufsoilpixels) < 1000){
               cat(Sys.getpid(), ' ', id, ' not enouqh candidates, increasing search radius');
               dpar$rad <- dpar$rad*2
               tries <- tries -1
       } else {
           break
       }
   }
       
   if(class(t1) == 'try-error') stop ('no candidates in search radius. Radius = ', dpar$rad)     


  # pull out result of candidate selection as variables in this scope
  cat('unpacking results of candidate search\n')
  for (v in 1:length(t1)) assign(names(t1)[v], t1[[v]])
  cat('current workspace:', ls(), sep = '\n\t')

    
  # Identify factors for classification
  padbufsoilpixels@data$LFELEMS <- as.character(padbufsoilpixels@data$LFELEMS)
  padpixels@data$LFELEMS <- as.character(padpixels@data$LFELEMS)
    
  ## Set up topographic similarity routine and select ART pixels
   
   # select comparison vars in pad
    dat1 <- padpixels@data[,c(toponames)]
    
    # select buffered candidate references
    dat2 <- padbufsoilpixels@data[,c(toponames)]
    
    # get similarity matrix for top 100 ref pixels for each target pixel
    cat('getting similarity index \n')
    #toposim <- gower_topn(x=dat1, y=dat2, n=100, nthread = 1)
    toposim <- getGowerByGroup(uni, padpixels, padbufsoilpixels)
    #aa <- get_art(padbufsoilpixels, padpixels, toposim, toponames)
    
    
    
    #artpixels <- aa$artpixels
    #avedist <- aa$avedist 
    allpixels <- get_chosen(toposim, padbufsoilpixels) 
    #extracted <- get_TS(dpar$respVars)


    ## Extraction
    if (length(dpar$respVars) > 0){

        extraction <- extract_TS(dpar$respVars, padpixels, allpixels, toposim)

    } else {

        extraction <- 'NO Extraction performed because no response included in parameters object'

    }
    
      
    timeElapsed <- Sys.time() - tstart
    cat('saving data to ', filename, '\n')
    save( padbufrast,padpolyi,padpoly,padstk, padpixels,padbufsoilpixels,padbufpixels, allpixels,  toposim, timeElapsed, dpar, extraction, file = filename) 
    
     gc()
}


eval_dart <- function(fin){

  load( fin )
  
  ## Evaluation
    zero_fill <- function(s) formatC(as.numeric(s), width = 2, flag = '0')

    yr <- as.Date( paste0(padpoly$yearEnd, zero_fill( padpoly$monthEnd ), zero_fill (padpoly$dayEnd)), format = '%Y%m%d')
    yr0 <- as.Date( '2009-06-01', format = '%Y%m%d')
            
        
     o <- longPanel(extraction, yr, dpar$varname, FALSE)
     dat <- list()
     
     for( r in 1:nrow(padpixels) ){
          cat(r, ' out of ', nrow(padpixels),'\n')
          # select right control and pad pixels
          ids <- c( paste0('trt', r), paste0('ctr', toposim$index[,r]))

          oi <- o[which(o$id %in% ids),]
          oi <- na.omit(oi)
          oi$id <- ifelse(grepl('trt', oi$id), 'trt', oi$id)

          ev <- evaluate(list( D= data.table(oi)))

           ev$pixelID <- r
          dat[[r]] <- ev
        
        }
    dat <- do.call(rbind, dat)
    dat$id <- padpoly$key
    save(dat, file = extension(fin, '_eval.RData'))
}


getGowerByGroup <- function(uni, padpixels, padbufsoilpixels){

   
   #Loop through unique combos of soilps and EC and get Gower output
   ts.out <- list( index = matrix(NA, 100, nrow(padpixels)), distance =  matrix(NA, 100, nrow(padpixels)) )
   
   for(i in 1:nrow(uni)){
      
      cat('    running gower similarity for group ', i , ' of ', nrow(uni), '\n')
      
      j <- which( padpixels@data$uni == i)
      
      k <- which( padbufsoilpixels@data$soilps == uni$soilps[i] & 
                  padbufsoilpixels@data$soilec > (uni$soilec[i] - dpar$ecBuf) & 
                  padbufsoilpixels@data$soilec < (uni$soilec[i] + dpar$ecBuf) )
      
      # select comparison vars in pad
      dat1 <- padpixels@data[j, c(toponames)]
      
      # select buffered candidate references
      dat2 <- padbufsoilpixels@data[k,c(toponames)]
    
      # get similarity matrix for top 100 ref pixels for each target pixel
      cat('getting similarity index \n')
      toposim <- gower_topn(x=dat1, y=dat2, n=100, nthread = 1)
      ts.out$index[,j] <- k[toposim$index]
      ts.out$distance[,j] <- toposim$distance
      

   }
   ts.out
}



get_candidates <- function(dpar, id){

   # get data

   cat('loading polygons\n')
   #   polygons <- shapefile(dpar$polygons)
   load(dpar$polygons)
   polygons$id <- polygons@data[[dpar$polygon_id_column]]
   padpoly <- polygons[ which(polygons$id == id),]
   
   toponames <- gsub(".tif","", names(dpar$topoVars))
   
   zone <- stack('zone.grd')
   # get_fun <- function(x) { 
       
        # s <- lapply( names(x), function(y) { cat('getting ', y, '\n') ; r <- brick(x[[y]]) ; names(r) <- y; r})
        # stack(s)
   
   # }
   
   # masks <- get_fun(dpar$maskVars)
   # topovars <- get_fun(dpar$topoVars)
   # filtervars <- get_fun(dpar$filterVars)

   # b <- stack(masks, topovars, filtervars)
   
   # rast.proj <- projection(masks[['refrast']])

   ## Prepare neighborhood
   padpolybuf <- buffer(padpoly, width = dpar$rad)
   padpolybuf$rastval <- 1

   ## crop
#   cat('cropping predictors\n')
 #  zone <- crop(b, padpolybuf)

   padbufrast <- rasterize(padpolybuf, zone[[1]], field=padpolybuf$rastval, datatype='INT1U')


  ## Screen out unwanted disturbances in buffer zone
  f_mask <- function(a,b) a*b

  nlcd_fn <- function(nlcd,padbufrast) {
        ind <- ifelse(nlcd!=21&nlcd!=22&nlcd!=24&nlcd!=81&nlcd!=82&nlcd!=11&nlcd!=12&padbufrast==1,1,NA)
        return(ind)
  }


  for( msk in names(dpar$maskVars)){
  
    if( msk == 'refrast') next()
    
    cat('masking ', msk,'\n')
    
    if( msk == 'nlcd'){
          padbufrast <- overlay( zone[[msk]], padbufrast , fun=nlcd_fn)
    } else {
        padbufrast <- overlay(padbufrast, zone[[msk]], fun = f_mask)
    }
    
  }

  # mask everything
  cat('masking buffer\n')
  padbufrast[which(padbufrast[] == 0)] <- NA
  padbufstk <- overlay(padbufrast, zone, fun = f_mask)
    
  # make donut
  cat('making donut\n')
  padpolyb <- buffer(padpoly, width = dpar$buffer)
  padbufstk <- mask(padbufstk, padpolyb, inverse = TRUE)
    
  # propagate names
  names(padbufstk) <-  names(zone)  

  # checks
  cat('checking number of candidates in buffer\n')
  if(all(is.na(padbufrast[]))) stop( 'ERROR1: no non-na candidate pixels in buffer (every pixel masked)')    

  cat('converting buffer to Spatial Pixels\n')
  padbufpixels <- as(padbufstk, "SpatialPixelsDataFrame")
  padbufpixels@data$ec <- padbufpixels@data$soilec / 100 # Rescale back to actual ec units from scaled integer used to store raster


################################################################

   # prepare treatment area   
   cat('preparing treatment area by inner buffer of ', dpar$innerRad, '\n')
   padpolyi <- buffer(padpoly, width = dpar$innerRad)  
   if(is.null(padpolyi)){
         cat('removing inner buffer due to insufficient size of polygon\n')
	 padpolyi <- polygons[ which(polygons$id == id),]
   }

  # for rasterization

  padpolyi$rastval <- 1
  
  
  ## Create rasters 
  cat('rasterizing treatment polygon\n')
  padrast <- rasterize(padpolyi, crop(zone[[1]], padpolyi),field=padpolyi$rastval, datatype='INT1U')

  ## if area too small, remove buffer -- should be redundant
  if(all(is.na(values(padrast)))){
	padpolyi <- polygons[ which(polygons$id == id),]
	padpolyi$rastval <- 1
	padrast <- rasterize(padpolyi, crop(zone[[1]], padpolyi),field=padpolyi$rastval, datatype='INT1U')
  }


  # make donut hole
  cat('cropping and masking treatment raster stack\n')  
  padstk <- crop(zone, padpolyi)
  
  # mask out unwanted  pixels in treated area

  padMask  <- overlay( padstk[['roadrast']], padstk[[1]], fun = f_mask)
  padMask  <- overlay( padstk[['oilgas']], padMask, fun = f_mask)
  padMask  <- overlay( padstk[['oilgas4corners']], padMask, fun = f_mask)
  padMask  <- overlay( padstk[['nlcd']], padMask, fun = nlcd_fn)
  padMask  <- overlay( padrast, padMask, fun = f_mask)
  
  padMask[padMask[] == 0] <- NA
  
  
  padstk <- overlay(padMask, padstk, fun = f_mask)
  
  names(padstk) <-  names(zone)  

  
  # check that there are enough padpixels

  cat('checking for enough non-na pixels in treatment area\n')
  if(all(is.na(padrast[]))) stop( 'ERROR2: not enough pixels in treatment polygon; area = ', area(padpoly)/900, ' pixels')  


    if(length(which(!is.na(padrast[]))) == 1){

        # do something to avoid error

    }
    

  cat('
  Convert treatment raster to Spatial Pixels DFs 
  \n')
  
  padpixels <- as(padstk, "SpatialPixelsDataFrame")
  padpixels@data$ec <- padpixels@data$soilec / 100 # Rescale back to actual ec units from scaled integer used to store raster


###############################################################
  
  cat('
  ## Summarizing pad pixels
  \n')
  
  padpscclasses <- as.numeric(names(summary(as.factor(padpixels@data$soilps))))
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  } # Fn to calculate mode if that is desirable
  
  padpscmode <- getmode(padpixels@data$psc)
  ecave <- mean(padpixels@data$soilec, na.rm=TRUE)
  
  # find unique combinations of soil ps and ec
  uni  = unique(padpixels@data[,c('soilps', 'soilec')])
  uni$uni <- paste0(uni$soilps, '-', uni$soilec)
  padpixels$uni <- match(paste0(padpixels@data$soilps, '-', padpixels@data$soilec), uni$uni)
  
  # check to see that there are at least 100 candidates per combo (using a +/- thresh value for ec)
  threshFunction <- function(x) length( which( padbufpixels@data$soilps == x[1] & 
                                               padbufpixels@data$soilec > (x[2] - dpar$ecBuf) & 
                                               padbufpixels@data$soilec < (x[2] + dpar$ecBuf) ))

  avail  = apply(uni[,1:2], 1, threshFunction)
  
  if( any(avail < 100 ) ) {
    cat( 'not enough pixels within threshold for some treated, \n'); 
    uni[which (avail < 100),] ; 
    stop('\nnot enough quality pixels\n')
  }

  
  #extend EC range by .05
  ecmin <- min(padpixels@data$soilec, na.rm=TRUE) - min(padpixels@data$soilec, na.rm=TRUE) -dpar$ecBuf
  ecmax <- max(padpixels@data$soilec, na.rm=TRUE) + max(padpixels@data$soilec, na.rm=TRUE) +dpar$ecBuf
  
  ## Select pad buffer pixels with same PSC and EC
  padbufsoilpixels <- subset(padbufpixels, padbufpixels@data$soilps %in% padpscclasses &    padbufpixels@data$soilec < ecmax & padbufpixels@data$soilec > ecmin)
  
  if(nrow(padbufsoilpixels) == 0){

     stop( 'ERROR3: not enough candidates w/correct EC/PSD in buffer')

  }

  return(list(padbufsoilpixels = padbufsoilpixels,padbufpixels=padbufpixels, padpixels = padpixels, padpoly= padpoly, padpolyi = padpolyi, padrast = padrast, padbufrast = padbufrast, padstk = padstk, padbufstk = padbufstk, ecave = ecave, ecmin = ecmin, ecmax = ecmax, zone = zone, toponames = toponames, uni = uni))

}



# select all pixels chosen in top 100 gower pixels
get_chosen <- function(toposim, padbufsoilpixels){

    a <- vv <- toposim$index
    aindex <- unique(c(a))
    allpixels <- padbufsoilpixels[aindex,]
    return(allpixels)

}

extract_TS <- function(responseVars, padpixels, allpixels, toposim){

    # extract response data from dart output
    # bfile = path to brick of things to extract
    # f = filename
    # rename = replace original file with 'renamed' file

    cat('loading response variables\n');flush.console()
    BB <- lapply(responseVars, function(x) { s <- brick(x); s } )
    extractedNames <- sapply(BB, names)

    
    st <- Sys.time()

    cat('extracting padvals\n') ; flush.console()
    padvals <- lapply(BB, function(x) {extract(x, padpixels, progress = 'text')})
    cat('extracting reference vals\n') ; flush.console()
    allvals <- lapply(BB, function(x) {extract(x, allpixels, progress = 'text')})

    get_stats <- function(x) { c( 'avg' = mean(x, na.rm= TRUE), 'med' = median(x, na.rm = TRUE), 'sd' = sd(x, na.rm = TRUE),
                                 'N' = sum(!is.na(x))) }


    ##
    # quantiles for each pixel relative to reference, for each variable
    ##

    a <- toposim$index
    aindex <- unique(c(a)) # unique reference pixels

    cat('getting reference pixel quantiles\n')
    # qqTsRef <- qqTsArt <- list()
    # for(i in 1:nrow(padvals)){

       # for a given pad pixel get candidates from gower
       # j <-  a[,i]
       # ref <- allvals[match(j, aindex),]
       # obs <- padvals[i,]

       # q_ref <- as.numeric(sapply(1:length(obs), function(i) try(ecdf(ref[,i])(obs[i]))))

       # qqTsRef[[i]] <- q_ref

    # }

    # q_ref <- do.call(rbind, qqTsRef)

    # q_ref_sum <- apply(q_ref, 2, get_stats)

    timeElapsed <- Sys.time() - st

  return ( list( refIndex = aindex, extractedTarget =padvals, extractedReference=allvals, ExtractedTimeElapsed = timeElapsed, extractedNames = extractedNames) )

}



 # check status of dart process
dartStatus <- function(dir){

    require(raster)
    ff <- list.files(dir, full = TRUE, pattern = 'RData')
    load(ff[1])
    s <- shapefile(dpar$polygons)
    pids <- s@data[ , dpar$polygon_id_column]
    has <- gsub('[^0-9]', '', ff)
    d <- setdiff(pids, has)
    cat('missing ', length(d), ' out of ', length(pids),' polygons\n')
    d

}

# split polygons by grid
split <- function(p, cellsz = 1000){

	x <- st_as_sf(p)
	grid <- st_make_grid(p, cellsize = cellsz)
	int <- st_intersection(x,grid)
        as(int, 'Spatial')
}




evalRun <- function(dir){
    
setwd(dir)

  load('results.RData')
  source('dart_functions.R')

  ds <- dartStatus('..')
  k <- grep('Error', result)

  error <- data.frame(id = ds, error = as.character(result[k]))

  error$err <- gsub('area.*','', error$error)


  print(table(error$err))
  save(error, file = 'errors.RData')
}



# Visualization

basicPlot <- function(datafile){

    load(datafile)
    require(raster)
    ## allpixels : SPDF of candidate pixel population
    ## artpixels : SPDF of art pixels
    ## avedist : average distance between dart pixels and target
    ## dpar : Parameter object for the extraction run
    ## padbufrast : raster of the extent of the pad buffer
    ## padpixels :  SPDF of treatment area
    ## padpoly : SPolygonsDF of treatment area
    ## padpolyi : SPolygonsDF of treatment area - buffer (the business region)
    ## padstk : raster stack of treatment area covariates
    ## timeElapsed : time for dart run
    ## toposim : distance matrix where rows = top 100 candidates, columns = target pixels
    ##     index = index in allpixels of candidate
    ##     distance = distance of candidate pixel from column id target
    ##     aindex <- unique(c(toposim$index)) # unique list of candidate reference pixels

    ## Basic plot of pad + buffers + candidates
    layout(matrix(1:2, 1,2))
    opar <- par()
    par(mar = c(5.1,4.1, 4.1,5.7))
    plot(padbufrast, legend = FALSE, col = 'light grey', main = 'Frequency')
    plot(padpolyi, add = TRUE)

    aindex <- unique(c(toposim$index))
    tb <- table(toposim$index[])
    allpixels$freq <- NA
    allpixels$freq[ match(as.numeric( names(tb) ) , aindex) ] <- tb
    plot( raster(allpixels['freq']), add = TRUE, col = rev(topo.colors(10)))

    ## average distance of each candidate pixel to reference
    distances = c(toposim$distance[])
    ids = c(toposim$index[])
    avedists <- aggregate( distances, list(id = ids), FUN = mean)
    allpixels$avedist <- NA
    allpixels$avedist[ match(avedists$id, aindex) ] <- avedists$x
    
    
    plot(padbufrast, legend = FALSE, col = 'light grey', main = '[Environmental] Distance')
    plot(padpolyi, add = TRUE)
    plot( raster( allpixels['avedist'] ), add = TRUE, col = rev(topo.colors(10)))
    par(opar) 
}


# plot timeseries
tsPlot <- function(datafile, xnames = 1984:2017){

    load(datafile)
    require(raster)
    
    target <- extraction$extractedTarget
    controls <- extraction$extractedReference

    yl = range(c(target[], controls[]), na.rm = TRUE)
    x <- 1:ncol(target)
    
    #png(fname, height = 7.5, width = 14, units = 'in', res = 300)
    layout(1)
    plot(x, x, ylim = yl, type = 'n', ylab = '', xaxt = 'n')
    axis(1,at = x, labels = xnames)
   
    apply(controls, 1, lines, col = rgb(0,0,0,.1))
    apply(target, 1, lines, col = rgb(1,0,0, .1))
    legend('topleft', col = c('red', 'black'), lty = 1, legend = c('treated', 'control'), bty = 'n')
    #dev.off()
    
}


# Format extracted data to long panel conventions
# 

longPanel <- function(extraction, yr, varname, covs=FALSE){

   # produce `long` data.frame with the following columns
   # id = name of pixel
   # y = response value
   # D = treatment occurred
   # time = timestep

   trt <- extraction$extractedTarget[[varname]]
   
   if(class(trt) == 'numeric') trt <- t(matrix(trt))
   
   ctr <- extraction$extractedReference[[varname]]
   
   # SCAN FOR BAD TIME POINTS
   W <- apply(trt, 2, function(x) sum(is.na(x))) > 0
   K <- apply(ctr, 2, function(x) sum(is.na(x))) > 0
   keep <- setdiff(1:ncol(trt), unique(c(which(W), which(K))))
   trt <- trt[,keep]
   ctr <- ctr[,keep]
   
   
   tn <- paste0('trt', 1:nrow(trt))
   
   if( any ( apply( trt, 1, function(x) length(which(x == 0)) > 2))) stop('too many zeros')
   
   # todo -- add extracted year names somehow in DART process
   #tm <- (1984:2018)[1:ncol(trt)]
   tm <- as.Date( gsub('X', '', colnames(trt)), format = '%Y%m%d')[keep]
   tD <- rep(0, length(tm))
   tD[which(yr < tm)[1]:length(tm)] <- 1
   tx <- 1:length(tm) - which(yr < tm)[1]
   
   g1 <- data.frame(id = rep(tn, each = ncol(trt)), y = c(t(trt)), D = rep(tD, nrow(trt)), time = rep(tx, nrow(trt)), dayt = rep(tm, nrow(trt)), stringsAsFactors = FALSE)
   
   if(covs){
   
      toAdd <- setdiff(names(extraction$extractedTarget),varname)
      for( v in toAdd){
        tv <- extraction$extractedTarget[[v]][,keep]
        g1[[v]] <- c(tv)
      }
   }
   
   cn <- paste0('ctr', extraction$refIndex) # use spatialPixels index
   
   g2 <- data.frame(id = rep(cn, each = ncol(ctr)), y = c(t(ctr)), D = 0, time = rep(tx, nrow(ctr)),dayt = rep(tm, nrow(ctr)), stringsAsFactors = FALSE)
   
   if(covs){
   
      toAdd <- setdiff(names(extraction$extractedReference),varname)
      for( v in toAdd){
        tv <- extraction$extractedReference[[v]][,keep]
        g2[[v]] <- c(tv)
      }
   }
   
   
   rbind(g1, g2)


}



get_art <- function( padbufsoilpixels, padpixels, toposim , toponames){

  # if there are enough pixels ...
  if(length(padbufsoilpixels) > 100){
     
    # get list of top choices per target pixel
    toposimf1 <- as.data.frame(factor(toposim$index[1,], levels = unique(toposim$index[1,])))
    colnames(toposimf1) <- "rows"
    
    # tabulate frequency
    toposimf1 <- count(toposimf1, "rows")

    #sort
    toposimf1$rows<-factor(toposimf1$rows, levels=toposimf1$rows[order(toposimf1$freq, decreasing=TRUE)])
    
    # create pool of candidates to draw from
    toposimrows <- as.character(toposimf1$rows)
    toposimrows <- unique(toposimrows)
    toposimrowstogo <- 100-length(toposimrows)
    
    # make list of top 25 candidates from each pixel
    toposimf25 <- as.data.frame(as.factor(toposim$index[2:25,]))
    colnames(toposimf25) <- "rows"
    toposimf25 <- count(toposimf25, "rows")
    toposimf25$rows<-factor(toposimf25$rows, levels=toposimf25$rows[order(toposimf25$freq, decreasing=TRUE)])

    # if still need more references
    if(toposimrowstogo > 0){
	    if(length(toposimf25$rows)<toposimrowstogo){
	      # add next 25 rows to selected rows
	      toposimrows <- append(toposimrows, as.character(toposimf25$rows))
	      toposimrows <- unique(toposimrows)
	      toposimrowstogo <- 100-length(toposimrows)
	    } else {
	      #prepare candidates
	      toposimrows <- append(toposimrows, names(summary(toposimf25$rows)[1:toposimrowstogo]))
	      toposimrows <- unique(toposimrows)
	      toposimrowstogo <- 100-length(toposimrows)
	    }
	    
	    if (length(toposimrowstogo)>0){
	      #look among top 100 for each pixel
	      toposimf100 <- as.data.frame(as.factor(toposim$index[26:100,]))
	      colnames(toposimf100) <- "rows"
	      toposimf100 <- count(toposimf100, "rows")
	      toposimf100$rows<-factor(toposimf100$rows, levels=toposimf100$rows[order(toposimf100$freq, decreasing=TRUE)])
	      toposimrows <- append(toposimrows, names(summary(toposimf100$rows)[1:toposimrowstogo]))
	      toposimrows <- unique(toposimrows)
	      toposimrowstogo <- 100-length(toposimrows)
	      while(toposimrowstogo > 0){
		toposimf100 <- subset(toposimf100,!(as.character(rows) %in% toposimrows))
		colnames(toposimf100) <- "rows"
		toposimf100 <- count(toposimf100, "rows")
		toposimf100$rows<-factor(toposimf100$rows, levels=toposimf100$rows[order(toposimf100$freq, decreasing=TRUE)])
		toposimrows <- append(toposimrows, names(summary(toposimf100$rows)[1:toposimrowstogo]))
		toposimrows <- unique(toposimrows)
		toposimrowstogo <- 100-length(toposimrows)
	      }
             }
    } 
    artpixels <- padbufsoilpixels[as.numeric(toposimrows),][1:100,]
    rowindex <- which(toposim$index %in% toposimrows, arr.ind = T)
    distances <- toposim$distance[rowindex]
    avedist <- mean(distances)
  } else {
    artpixels <- padbufsoilpixels
    dat1 <- padpixels@data[,c(toponames)]
    dat2 <- artpixels@data[,c(toponames)]
    toposim <- gower_topn(x=dat1, y=dat2, n=length(padbufsoilpixels), nthread = 1)
    avedist <- mean(toposim$distance)
  }
  
    return(list(artpixels = artpixels, avedist = avedist))
  
}




