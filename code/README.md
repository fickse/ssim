|filename|description|
|---|---|
| analysis.R | generate figures and outputs |
| config.R | Set parameters for simulations |
| error.sh | SLURM batch script for errors.R | 
| errors.R | Compile the results of simulation run | 
| eval_functions.R | functions for generating effect estimates |
| example_fig.R | Generate figures showing simulation methodology |
| genLoadBalance.R | Generate an batch-ready R script based on parameters from `config.R`|
| sbatch.R | generate SLURM batch file based on parameters from `config.R` | 
| setup.R | Generate files and folders needed for simulation on HPC | 
| simulation_functions.R | Functions for generating random NDVI timeseries |
| sweep.R | compile results |
| sweep.sh | Slurm batch script for `sweep.R`|

