
#library(data.table)

# number of simulations to run per combination of inputs
nsim = 1000
chunkSize = 250

# run name
  now <- Sys.time()
  
  # daily
  # dname <- paste0( 'run_', strftime(now, format = "%Y%m%d"))
   
  # sub-daily outputs
  # dname <- paste0( 'run_',tag, strftime(now, format = '%Y%m%d%H%M%S'))
  
  dname <- 'r1'


# where output files will be stored
dataDir = file.path('/lustre/projects/ecosystems/sbsc/ucrb/sims', dname)
dir.create(dataDir)

###
# generate expand.grid of test conditions
###

  ddate <- as.Date('2006-02-15')
  tt <- seq( as.Date('1999-01-01'), as.Date('2010-01-01'), by = 16)

  # variables
  trtType = c('forest', 'grassland')
  sdNoise = c(0.001, 0.005, seq(.01, .07, .01))
  disturbance = c(.1,0)
  nControl = c(1, 5, 10, 50, 100)
  misMatch = c(0, .5, 1)
  overrideNoise = c(TRUE, FALSE)  # use sdNoise instead of defaults
  auto_range = c(0, 10)
  auto_type = c('hadj', 'climAnom;sat;rw', 'nse','response')


  # set params
  climSD = .5
  climCenter = 10
  satLambda = .05
  rwSD = .1
  affinitySD = .25
  timeVaryingAffinitySD = .05
  randConstantSD = .05
  
  #
  RUN <- expand.grid(sim = 1:nsim, type = trtType,  sdNoise= sdNoise, disturbance = disturbance,  nControl = nControl, misMatch = misMatch, climSD = climSD, climCenter = climCenter, satLambda = satLambda, rwSD = rwSD, affinitySD = affinitySD, timeVaryingAffinitySD = timeVaryingAffinitySD, randConstantSD = randConstantSD, overrideNoise = overrideNoise, auto_range = auto_range, auto_type = auto_type, stringsAsFactors = FALSE)
  
  # thin Runs here

  # Run autocorr on specific sets of other params

  # only keep sdnoise = 0.05 & grassland & n = 100 & mismatch = .5
  w <- which(RUN$auto_range > 0 & RUN$sdNoise != .05 & RUN$type == 'forest' & RUN$nControl != 100 & RUN$misMatch != .5 )
  if(length(w) > 0) RUN <- RUN[-w,]

  # remove autocor permutations for auto_range = 0)
  w <- which(RUN$auto_range == 0 & RUN$auto_type != 'nse')
  if(length(w) > 0) RUN <- RUN[-w,]

  # dont override noise for autocor simulations
  w <- which(RUN$auto_range > 0 & !RUN$overrideNoise)
  if(length(w) > 0 ) RUN <- RUN[-w,]

  # Remove all reps of sdNoise where sdNoise is over-ridden
  i <- which(RUN$sdNoise != .01 & RUN$overrideNoise)
  if(length(i) > 0) RUN <- RUN[-i,]

  i <- which(RUN$scNoise < 0.01 & RUN$nControl < 10)
  if (length(i) > 0) RUN <- RUN[-i,]


# thin out combos to only have one confounder active at a time

cat( nrow(RUN)/nsim, ' combinations\n')

cat(nsim,  'sims per combo\n')

cat(nrow(RUN), ' sims total\n')

# create job csv
 jobs <- RUN
 jobs$i <- 1:nrow(jobs)
 jobs$job <- as.numeric( cut( jobs$i, seq(0, nrow(jobs), by = chunkSize)))
  
 j <- aggregate(jobs$i, FUN = min, by = list(job = jobs$job))
 k <- aggregate(jobs$i, FUN = max, by = list(job = jobs$job))
 j$end <-k$x
 jobs <- j
 names(jobs) <- c('job', 'start', 'end')

# SLURM parameters
nodes <- 1
cores <- 24
timeLimit <- "01:00:00"
partition <- 'normal'
#qos <- 'normal'
output <- 'log/job-%A_%a.out'
acct <- 'swbsc'
maxArray <- 100





