# Soybean-BioCro

The code in this repository reproduces the results in Matthews et al. Soybean-BioCro: A semi-mechanistic model of soybean growth.

The version of BioCro associated with this manuscript is included as a submodule in this repository. To clone this repository with the submodule using the following github command:

`git clone --recurse-submodules https://github.com/cropsinsilico/Soybean-BioCro.git`

See the BioCro README to install BioCro.

To run an example run of Soybean-BioCro use the following command after installing the BioCro package:

`result <- Gro_solver(soybean_initial_state, soybean_parameters, soybean_weather2002, soybean_steadystate_modules, soybean_derivative_modules, soybean_solver_params)`


The code in this repository was developed and tested on Mac OS version 10.14.6, with R version 3.6.1 and Rstudio version 1.2.1335.


