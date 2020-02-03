|filename|description|
|---|---|
| analysis.R | generate figures and outputs |
| config.R | Set parameters for simulations |
| error.sh | SLURM batch script for errors.R | 
| errors.R | Compile the results of simulation run | 
| eval_functions.R | functions for generating effect estimates |
| example_fig.R | Generate figures showing simulation methodology |
| genLoadBalance.R | Generate an batch-ready R script `loadBalance.R` based on parameters from `config.R`|
| sbatch.R | generate SLURM batch file `loadBalance.sh` based on parameters from `config.R` | 
| setup.R | Generate files and folders needed for simulation on HPC | 
| simulation_functions.R | Functions for generating random NDVI timeseries |
| sweep.R | compile results |
| sweep.sh | Slurm batch script for `sweep.R`|


---

### the process:

1. edit config.R for file system and run parameters
2. execute setup.R, which generates `loadBalance.sh` and `loadBalance.R` in an 'outputs' directory specified in `config.R`. In this directory is also a saved R session with preloaded environmental variables. 
3. execute `loadBalance.sh` script to run the simulations
4. execute `sweep.R` to combine results
5. `analysis.R` generates figures for paper. 
