library(BioCro)
library(GenSA)
library(tictoc)
library(DEoptim)

setwd('~/Research/BioCro/SoybeanBioCro/Parameter_Optimization')

# Cost function
source('multiyear_BioCro_optim.R')

# soybean_initial_state
source('~/Research/BioCro/SoybeanBioCro/Parameters/soybean_initial_state.R')
soybean_initial_state$soil_evaporation_rate<-NULL

# soybean_parameters
source('~/Research/BioCro/SoybeanBioCro/Parameters/soybean_parameters.R')
soybean_parameters$atmospheric_pressure<-101325
soybean_parameters$atmospheric_transmittance<-0.85
soybean_parameters$atmospheric_scattering<-0.3
soybean_parameters$par_energy_fraction_of_sunlight<-0.5
soybean_parameters$par_energy_content<-0.235

soybean_parameters$tpu_rate_max<-23
soybean_parameters$absorptivity_par<-0.8
# soybean_parameters$absorptivity_nir<-0.2

soybean_parameters$remobilization_fraction <- 0.6
# soybean_parameters$Rd<-0.6 # value used in yu's leaf model

# steady_state_modules
# steady_state_modules <- c('soil_type_selector', 'stomata_water_stress_linear', 'leaf_water_stress_exponential', 'parameter_calculator', 'c3_canopy', 'soybean_development_rate_calculator', 'partitioning_coefficient_logistic', 'no_leaf_resp_partitioning_growth_calculator','senescence_coefficient_logistic')
steady_state_modules <- c('soil_type_selector', 'stomata_water_stress_linear',
                'leaf_water_stress_exponential', 'parameter_calculator',
                'soybean_development_rate_calculator','partitioning_coefficient_logistic',
                'soil_evaporation','solar_zenith_angle','shortwave_atmospheric_scattering',
                'incident_shortwave_from_ground_par','ten_layer_canopy_properties',
                'ten_layer_c3_canopy','ten_layer_canopy_integrator',
                'no_leaf_resp_partitioning_growth_calculator_negative_assimilation_partitioned',
                'senescence_coefficient_logistic')

# derivative_modules
derivative_modules <- c('thermal_time_senescence_logistic', 'partitioning_growth_negative_assimilation_partitioned', 'two_layer_soil_profile', 'development_index','thermal_time_linear')

# Solver parameters
solver_params <- list(
  type = 'Gro_rsnbrk',#'Gro_rkck54',
  output_step_size = 1.0,
  adaptive_rel_error_tol = 1e-4,
  adaptive_abs_error_tol = 1e-4,
  adaptive_max_steps = 200)

# Names of optimized parameters
arg_names <- c('alphaLeaf','alphaRoot','alphaStem','betaLeaf','betaRoot','betaStem',
               'rateSeneLeaf','rateSeneStem','alphaSeneLeaf','betaSeneLeaf',
               'alphaSeneStem','betaSeneStem')

verbose <- FALSE

# weather
# year <- c('2002', '2004', '2005')
# sow.date <- c(152, 149, 148)
# harv.date <- c(288, 280, 270)

year <- c('2002', '2005')
sow.date <- c(152, 148)
harv.date <- c(288, 270)

soybean_optsolver <- list()
ExpBiomass <- list()
ExpBiomass.std <- list()
RootVals <- list()
weights <- list()
numrows <- vector()

for (i in 1:length(year)) {
  yr <- year[i]
  weather <- read.csv(file = paste0('~/Research/BioCro/SoybeanBioCro/Weather_Data/', yr, '/', yr,'_Bondville_IL_daylength.csv'))
  sd.ind <- which(weather$doy == sow.date[i])[1]
  hd.ind <- which(weather$doy == harv.date[i])[24]
  
  weather.growingseason <- weather[sd.ind:hd.ind,]
  
  soybean_optsolver[[i]] <- partial_gro_solver(soybean_initial_state, soybean_parameters, weather.growingseason,
                                               steady_state_modules, derivative_modules, 
                                               arg_names, solver_params, verbose)
  
  ExpBiomass[[i]] <- read.csv(file=paste0('~/Research/Soy Bean/SoyFace/2001-2007_Biomass/',yr,'_ambient_biomass.csv'))
  colnames(ExpBiomass[[i]])<-c("DOY","Leaf","Stem","Rep","Seed","Litter","CumLitter")
  
  ExpBiomass.std[[i]] <- read.csv(file=paste0('~/Research/Soy Bean/SoyFace/2001-2007_Biomass/',yr,'_ambient_biomass_std.csv'))
  colnames(ExpBiomass.std[[i]])<-c("DOY","Leaf","Stem","Rep","Seed","Litter","CumLitter")
  
  RootVals[[i]] <- data.frame("DOY"=ExpBiomass[[i]]$DOY[5], "Root"=0.17*sum(ExpBiomass[[i]][5,2:4])) # See Ordonez et al. 2020, doi: 10.1016/j.eja.2020.126130
  
  numrows[i] <- nrow(weather.growingseason)
  invwts <- ExpBiomass.std[[i]]
  weights[[i]] <- log(1/(invwts[,2:ncol(invwts)]+1e-5))
  # weights[[i]] <- log(1/(ExpBiomass.std[[i]][,2:ncol(ExpBiomass.std[[i]])]+1e-5))
  
}

wts2 <- data.frame("Stem" = 1, "Leaf" = 1, "Rep" = 1, "Root" = 0.125)

ul<-50
ll<--50
upperlim<-c(ul,ul,ul,
            0,0,0,
            0.0125,.005,
            ul,0,ul,0)
lowerlim<-c(0,0,0,
            ll,ll,ll,
            0,0,
            0,ll,0,ll)


cost_func <- function(x){
  multiyear_BioCro_optim(x, soybean_optsolver, ExpBiomass, numrows, weights,wts2,RootVals)
}


rng.seed <- 12345
set.seed(rng.seed)
opt_pars <- runif(length(arg_names), min = lowerlim, max = upperlim)
# opt_pars <- c(29.171164778, 43.065053235, 26.638414815, -21.327746505, -40.098306546,
#               -19.151087039, 0.011804004, 0.000399121, 47.675890507, -24.447502898,
#               34.219878212, -28.226981250)

# opt_pars <- c(5.953971e+01, 6.620136e+01, 5.710041e+01, -4.554979e+01, -5.395100e+01,
#               -4.356632e+01, 1.250000e-02, 6.948098e-04, 4.350725e+01, -2.332875e+01,
#               5.264437e+01, -2.879990e+01)

# opt_pars <- c(21.3820759314431, 25.0353104070888, 24.435066579769, -15.8760615382224,
              # -20.3459150811421, -19.1595370042528, 0.0124453707343277, 0.00159250048406981,
              # 46.9734272893614, -24.8135535342014, 21.6725290955469, -10.3362305852489)

# max.call <- 1.55e5 #5e4 #5.2e4# 7e4 #1.55e5
max.iter <- 1000

# weights <- data.frame("Stem" = 10, "Leaf" = 10, "Rep" = 15, "kRoot" = 0)
parVars <- c('multiyear_BioCro_optim','soybean_optsolver','ExpBiomass','numrows','weights','wts2','RootVals')
tic()
# optim_result<-GenSA(par=opt_pars, fn=function(x) multiyear_BioCro_optim(x, soybean_optsolver, ExpBiomass, numrows, weights,wts2,RootVals), lower=lowerlim, upper = upperlim, control=list(nb.stop.improvement=50, max.call = max.call))
optim_result<-DEoptim(fn=cost_func, lower=lowerlim, upper = upperlim, control=list(itermax=max.iter,parallelType=1,packages=c('BioCro'),parVar=parVars))

toc()

note<-'210317 run: this was run after fix the c3photoc function in biocro to include temperature impact on jmax'
save.image(file='210317_multiyear_optimization_2002_2005amb_allwtedsame_negassimpartitioned_Root_DEoptim.RData')

