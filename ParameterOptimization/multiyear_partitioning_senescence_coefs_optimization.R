library(BioCro)
library(DEoptim)

# Clear workspace
rm(list=ls())

# Set working directory to location of this file
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Cost function
source('multiyear_BioCro_optim.R')

# Names of parameters being fit
arg_names <- c('alphaLeaf','alphaRoot','alphaStem','betaLeaf','betaRoot','betaStem',
               'rateSeneLeaf','rateSeneStem','alphaSeneLeaf','betaSeneLeaf',
               'alphaSeneStem','betaSeneStem')


# years, sowing dates, and harvesting dates of growing seasons being fit to
year <- c('2002', '2005')
sow.date <- c(152, 148)
harv.date <- c(288, 270)

## Initialize variables for the cost function
soybean_optsolver <- list()
ExpBiomass <- list()
ExpBiomass.std <- list()
RootVals <- list()
weights <- list()
numrows <- vector()

for (i in 1:length(year)) {
  yr <- year[i]
  weather <- read.csv(file = paste0('../Data/Weather_data/', yr,'_Bondville_IL_daylength.csv'))
  sd.ind <- which(weather$doy == sow.date[i])[1]
  hd.ind <- which(weather$doy == harv.date[i])[24]
  
  weather.growingseason <- weather[sd.ind:hd.ind,]
  
  soybean_optsolver[[i]] <- partial_gro_solver(soybean_initial_state, soybean_parameters, weather.growingseason,
                                               soybean_steadystate_modules, soybean_derivative_modules, 
                                               arg_names, soybean_solver_params)
  
  ExpBiomass[[i]] <- read.csv(file=paste0('../Data/SoyFACE_data/',yr,'_ambient_biomass.csv'))
  colnames(ExpBiomass[[i]])<-c("DOY","Leaf","Stem","Pod")
  
  ExpBiomass.std[[i]] <- read.csv(file=paste0('../Data/SoyFACE_data/',yr,'_ambient_biomass_std.csv'))
  colnames(ExpBiomass.std[[i]])<-c("DOY","Leaf","Stem","Pod")
  
  RootVals[[i]] <- data.frame("DOY"=ExpBiomass[[i]]$DOY[5], "Root"=0.17*sum(ExpBiomass[[i]][5,2:4])) # See Ordonez et al. 2020, https://doi.org/10.1016/j.eja.2020.126130
  
  numrows[i] <- nrow(weather.growingseason)
  invwts <- ExpBiomass.std[[i]]
  weights[[i]] <- log(1/(invwts[,2:ncol(invwts)]+1e-5))

}

wts2 <- data.frame("Stem" = 1, "Leaf" = 1, "Rep" = 1, "Root" = 0.125)

## Optimization settings
ul<-50 
ll<--50

# parameter upper limit
upperlim<-c(ul,ul,ul,
            0,0,0,
            0.0125,.005,
            ul,0,ul,0)

# parameter lower limit
lowerlim<-c(0,0,0,
            ll,ll,ll,
            0,0,
            0,ll,0,ll)


# cost function
cost_func <- function(x){
  multiyear_BioCro_optim(x, soybean_optsolver, ExpBiomass, numrows, weights, wts2, RootVals)
}


rng.seed <- 12345 # seed for random number generator
set.seed(rng.seed)

# initialize parameters being fitted as randome values from a uniform distribution
opt_pars <- runif(length(arg_names), min = lowerlim, max = upperlim)

# maximum number of iterations
max.iter <- 1000

# Call DEoptim function to run optimization
parVars <- c('multiyear_BioCro_optim','soybean_optsolver','ExpBiomass','numrows','weights','wts2','RootVals')
optim_result<-DEoptim(fn=cost_func, lower=lowerlim, upper = upperlim, control=list(itermax=max.iter,parallelType=1,packages=c('BioCro'),parVar=parVars))
