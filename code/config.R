
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
  
  dname <- 'run_01'


# where output files will be stored
dataDir = file.path('/lustre/projects/ecosystems/sbsc/ucrb/sims', dname)
dir.create(dataDir)

###
# generate expand.grid of test conditions
###

  ddate <- as.Date('2006-02-15')
  tt <- seq( as.Date('2001-01-01'), as.Date('2010-01-01'), by = 16)

  # variables
  trtType = c('forest', 'grassland')
  sdNoise = seq(.01, .07, .01)
  disturbance = c(.1)
  nControl = c(1, 5, 10, 50, 100)
  misMatch = c(0, .5, 1)
  overrideNoise = c(TRUE, FALSE)  # use sdNoise instead of defaults

  # set params
  climSD = .1
  climCenter = 10
  satLambda = .05
  rwSD = .01
  affinitySD = .25
  timeVaryingAffinitySD = .05
  randConstantSD = .05
  
  #
  RUN <- expand.grid(sim = 1:nsim, type = trtType,  sdNoise= sdNoise, disturbance = disturbance,  nControl = nControl, misMatch = misMatch, climSD = climSD, climCenter = climCenter, satLambda = satLambda, rwSD = rwSD, affinitySD = affinitySD, timeVaryingAffinitySD = timeVaryingAffinitySD, randConstantSD = randConstantSD, overrideNoise = overrideNoise)
  
  # thin Runs here
  
  # Remove all reps of sdNoise where sdNoise is over-ridden
  i <- which(RUN$sdNoise != .01 & RUN$overrideNoise)
  if(length(i) > 0) RUN <- RUN[-i,]


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
timeLimit <- "01:30:00"
partition <- 'normal'
#qos <- 'normal'
output <- 'log/job-%A_%a.out'
acct <- 'swbsc'
maxArray <- 100





