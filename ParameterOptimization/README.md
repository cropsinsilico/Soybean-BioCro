# Parameter optimization

To reproduce the fitting of the partitioning and senescence parameters source the `multiyear_partitioning_senescence_coefs_optimization.R` script. This script calls the `multiyear_BioCro_optim()` function which calculates the weighted predicted RMSEs described in Matthews et al.

Note: This optimization can take several hours to run.

Required R packages:
- BioCro (the version included as a submodule in this repository)
- DEoptim (tested on version 2.2-5)
