#!/bin/bash

#SBATCH --ntasks=1
#SBATCH --time=00:40:00
#SBATCH --partition=shas
#SBATCH --qos=normal
#SBATCH --output=job-%j.out

module purge

module load intel
module load impi
module load loadbalance
module load R/3.5.0

echo $(date)
time Rscript errors.R outputs/run_a_20190619205943
echo $(date)


echo "== End of Job =="

