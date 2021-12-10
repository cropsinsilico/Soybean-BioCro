# Soybean-BioCro

The code in this repository reproduces the results in Matthews et al. Soybean-BioCro: a semi-mechanistic model of soybean growth. <i>in silico</i> Plants. https://doi.org/10.1093/insilicoplants/diab032

The version of BioCro associated with this manuscript is included as a submodule in this repository. To clone this repository with the submodule using the following github command:

`git clone --recurse-submodules https://github.com/cropsinsilico/Soybean-BioCro.git`

See the [BioCro README](https://github.com/ebimodeling/biocro/tree/e999dd1a69ca83dc48ac918a17174b6156bc4a49#readme) to install BioCro.

To run an example run of Soybean-BioCro use the following command after installing the BioCro package:

`result <- run_biocro(soybean_initial_values, soybean_parameters, soybean_weather2002, soybean_direct_modules, soybean_differential_modules, soybean_ode_solver)`


The code in this repository was developed and tested on Mac OS version 10.14.6, with R version 3.6.1 and Rstudio version 1.2.1335.


