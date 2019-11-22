
# reframe everything in terms of raw noise = 1

# number of simulations to run per combination of inputs
nsim = 1000
chunkSize = 1000

# where output files will be stored
dataDir = file.path('/scratch/summit/sfick@xsede.org/sims', dname)
dir.create(dataDir)

###
# generate expand.grid of test conditions
###
  # variables
  trtType = c('forest', 'grassland')
  sdNoise = seq(.01, .07, .01)
  disturbance = c(.1)
  nControl = c(1, 5, 10, 50, 100)
  misMatch = c(0, .5, 1)
  
  # set params
  climSD = .1
  climCenter = 10
  satLambda = .05
  rwSD = .01
  affinitySD = .25
  timeVaryingAffinitySD = .05
  randConstantSD = .05
  
  #
  RUN <- expand.grid(sim = 1:nsim, type = trtType,  sdNoise= sdNoise, disturbance = disturbance,  nControl = nControl, misMatch = misMatch, climSD = climSD, climCenter = climCenter, satLambda = satLambda, rwSD = rwSD, affinitySD = affinitySD, timeVaryingAffinitySD = timeVaryingAffinitySD, randConstantSD = randConstantSD)


# thin out combos to only have one confounder active at a time

cat( nrow(RUN)/nsim, ' combinations\n')

cat(nsim,  'sims per combo\n')

cat(nrow(RUN), ' sims total\n')


# SLURM parameters
nodes <- 1
cores <- 24
timeLimit <- "01:00:00"
partition <- 'normal'
#qos <- 'normal'
output <- 'log/job-%j.out'







