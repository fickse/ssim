#!/bin/bash

#SBATCH --ntasks=1
#SBATCH --time=00:30:00
#SBATCH --partition=shas
#SBATCH --qos=normal
#SBATCH --output=sweep-%j.out

module purge

module load R/3.5.0

echo $(date)
time Rscript sweep.R
echo $(date)


echo "== End of Job =="
