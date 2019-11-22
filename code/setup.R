# RS Simulation for summit
#
# Project Author (s): Steve Fick
#
# Project Description: Simulate various treatment effects with added noise. See how well different techniques approximate 'truth'

args = commandArgs(TRUE)

if(length(args) > 0){
    if(args[1] == '&'){
        tag=''
    } else {
        tag= paste0(args[1],'_')
    }
}else{
    tag = ''
}

source('simulation_functions.R')
source('eval_functions.R')


# outputs directory
# For each invocation of run_all.R, create a new directory and store all results there
now <- Sys.time()

# output directory name
    
	# daily
#  dname <- paste0( 'run_', strftime(now, format = "%Y%m%d"))
    
	# sub-daily outputs
    dname <- paste0( 'run_',tag, strftime(now, format = '%Y%m%d%H%M%S'))

outdir <- file.path( 'outputs', dname )
dir.create(outdir, recursive =TRUE)

# create copy of code used in the run_

########################################################
########################################################

# load config
source('config.R')

# stash outputs
library(session)
save.session(file.path(outdir, 'session.RData'))

# move serial function to work directory
source('genLoadBalance.R')

file.copy('config.R', outdir)
file.copy('sweep.R', outdir)
file.copy('sweep.sh', outdir)
    
# generate list of commands
ii <- seq(1, nrow(RUN), chunkSize)
ii <- sample(ii)
cat( paste0('Rscript loadBalance.R ', ii), sep = '\n', file = file.path(outdir, 'workList') )

# generate sbatch file
source('sbatch.R')

# execute simulations
setwd(outdir)
# system('sbatch loadBalance.sh')

# e.g source('code/02_analysis_1.R')
