
# nodes <- 1
# cores <- 20
# timeLimit <- "01:00:00"
# partition <- 'shas'
# qos <- 'long'
# output <- 'job-%j.out'

string <- c(

'#!/bin/bash',
'',
#paste0('#SBATCH --nodes=', nodes),
paste0('#SBATCH --ntasks=', cores),
paste0('#SBATCH --time=', timeLimit),
paste0('#SBATCH --partition=', partition),
#paste0('#SBATCH --qos=', qos),
paste0('#SBATCH --output=', output),
'
module purge

module load intel
module load impi
module load loadbalance
module load R/3.5.0

echo $(date)
time mpirun lb workList
echo $(date)


echo "== End of Job =="
')

cat(string, sep = '\n', file = file.path(outdir, 'loadBalance.sh'))




cat(
'#!/bin/bash
ls -l ', dataDir, ' | wc -l
echo "out of"
echo "', nrow(RUN), '"', file = file.path(outdir, 'check.sh'))