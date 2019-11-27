
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
#paste0('#SBATCH --ntasks=', cores),
paste0('#SBATCH --time=', timeLimit),
paste0('#SBATCH --partition=', partition),
#paste0('#SBATCH --qos=', qos),
paste0('#SBATCH --output=', output),
paste0('#SBATCH --account=', acct),
paste0('#SBATCH --array=1-', nrow(jobs),'%',maxArray),
'

echo "SLURM_JOBID: " $SLURM_JOBID
echo "SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
echo "SLURM_ARRAY_JOB_ID: " $SLURM_ARRAY_JOB_ID
echo "Scratch: " $GLOBAL_SCRATCH

module load R/3.6.1-gcc7.1.0
module load gdal/2.2.2-gcc proj/5.0.1-gcc-7.1.0 gcc/7.1.0
module load gis/geos-3.5.0


echo $(date)
Rscript loadBalance.R $SLURM_ARRAY_TASK_ID
echo $(date)


echo "== End of Job =="
')

cat(string, sep = '\n', file = file.path(outdir, 'loadBalance.sh'))




cat(
'#!/bin/bash
ls -l ', dataDir, ' | wc -l
echo "out of"
echo "', nrow(RUN), '"', file = file.path(outdir, 'check.sh'))