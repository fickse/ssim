#!/bin/bash

#SBATCH --job-name=sweep
#SBATCH --partition=normal,UV
#SBATCH --account=swbsc
#SBATCH --time=01:59:00
#SBATCH --output=log/job%A_%a.out
#SBATCH --mem=16G
#SBATCH --array=1

echo "SLURM_JOBID: " $SLURM_JOBID
echo "SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
echo "SLURM_ARRAY_JOB_ID: " $SLURM_ARRAY_JOB_ID
echo "Scratch: " $GLOBAL_SCRATCH

module load R/3.6.1-gcc7.1.0
module load gdal/2.2.2-gcc proj/5.0.1-gcc-7.1.0 gcc/7.1.0
module load gis/geos-3.5.0

echo $(date)
time Rscript sweep.R
echo $(date)


echo "== End of Job =="
