library(BioCroSoyBean)
# Clear workspace
rm(list=ls())

# Set working directory to location of this file
#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)

# turn off warnings
options(warn=-1)

# if needed, create ./figs/ directory
path.figs <- './figs/'
dir.create(path = path.figs, showWarnings = FALSE)

years <- c('2002','2004','2005','2006')

# sowing and harvest DOYs for each growing season
dates <- data.frame("year" = c(2002, 2004:2006),"sow" = c(152,149,148,148), "harvest" = c(288, 289, 270, 270))

# initialize variables
# results <- list()
# results.elevCO2 <- list()
weather.growingseason <- list()

# for partial_gro_solver
arg_names <- c('Catm') # atmospheric CO2 parameter
CO2s <- 550#c(380,550,800)
solar_threshold = 10 #to get the daytime
no_layers = 10
doys = c(180,220,260) #specfic doy to investigate
#varying vmax & jmax from -50% to +50%
v_scaler = seq(0.5,1.5,by=0.1) 
j_scaler = seq(0.5,1.5,by=0.1) 

soybean_para0 = soybean_parameters
soybean_para  = soybean_parameters
#init outputs
podmass     = array(NaN,c(length(v_scaler),length(j_scaler),length(years),length(doys)))    
shootmass   = array(NaN,c(length(v_scaler),length(j_scaler),length(years),length(doys)))    
total_assim = array(NaN,c(length(v_scaler),length(j_scaler),length(years),length(doys)))    

total_assim_dmax = array(NaN,c(length(v_scaler),length(j_scaler),length(years))) #mean of daily max    
layer_assim_sunlit = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) 
layer_assim_shaded = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) 
Ci_sunlit          = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) #mean 
Ci_shaded          = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) 

for (CO2 in CO2s){
t0 = Sys.time()
for (i in 1:length(years)) {
  yr <- years[i]
  print(paste("processing year",yr)) 

  weather <- read.csv(file = paste0('../Data/Weather_data/', yr, '_Bondville_IL_daylength.csv'))

  sowdate <- dates$sow[which(dates$year == yr)]
  harvestdate <- dates$harvest[which(dates$year == yr)]
  sd.ind <- which(weather$doy == sowdate)[1] # start of sowing day
  hd.ind <- which(weather$doy == harvestdate)[24] # end of harvest day
  
  weather.growingseason[[i]] <- weather[sd.ind:hd.ind,]
  solar = weather.growingseason[[i]]$solar 
  daytime_ind = which(solar>solar_threshold) #look at daytime only

  for (j in 1:length(j_scaler)){
  for (k in 1:length(v_scaler)){
  	soybean_para$jmax =  soybean_para0$jmax   *  j_scaler[j] 
  	soybean_para$vmax1 = soybean_para0$vmax1  *  v_scaler[k]
  	soybean_solver <- partial_gro_solver(soybean_initial_state, soybean_para, weather.growingseason[[i]],
                                       soybean_steadystate_modules, soybean_derivative_modules,
                                       arg_names, soybean_solver_params)
        results         <- soybean_solver(CO2)
        canopy_assim = results[,"canopy_assimilation_rate"]
        cname = colnames(results)
        sunlit_index = grep("^sunlit_Assim.*", cname) #matching all sunlit assim 
        shaded_index = grep("^shaded_Assim.*", cname) 
        if(length(sunlit_index) != no_layers || length(shaded_index) !=no_layers){
           stop("results issues!?")
        }
#        layer_assim_sunlit[k,j,i,] = apply(results[,sunlit_index],c(2),mean,na.rm=TRUE) #apply mean for each layer
#        layer_assim_shaded[k,j,i,] = apply(results[,shaded_index],c(2),mean,na.rm=TRUE) #apply mean for each layer
        assim_sunlit = results[,sunlit_index]
        assim_shaded = results[,shaded_index]

        sunlit_index_Ci = grep("^sunlit_Ci_layer.*", cname)
        shaded_index_Ci = grep("^shaded_Ci_layer.*", cname)
        if(length(sunlit_index_Ci) != no_layers || length(shaded_index_Ci) !=no_layers){
           stop("results issues!?")
        }
        tmp = results[,sunlit_index_Ci]
        if(dim(tmp)[1] != length(solar)) stop("what?")
        tmp = tmp[daytime_ind,] 
        Ci_sunlit[k,j,i,] = apply(tmp,c(2),mean,na.rm=TRUE) #apply mean for each layer
        tmp = results[,shaded_index_Ci]
        tmp = tmp[daytime_ind,] 
        Ci_shaded[k,j,i,] = apply(tmp,c(2),mean,na.rm=TRUE) #apply mean for each layer

        DOY = results[,"doy"]
        doy_unique = unique(DOY)
#for assimilation, we calculate the MEAN of DAILY MAX! 
#use a simple loop for now. Using matrix operation can speed this up
        canopy_assim_dmax = c()
        assim_sunlit_dmax = c()
        assim_shaded_dmax = c()
        for (ss in 1:length(doy_unique)){
            d_ss = doy_unique[ss]
            doy_ind = which(DOY==d_ss)
            canopy_assim_dmax = c(canopy_assim_dmax,max(canopy_assim[doy_ind],na.rm=TRUE))
            tmp = apply(assim_sunlit[doy_ind,],2,max,na.rm=TRUE) #get the daily max of each layer
            if(length(tmp) != no_layers) stop("bug1")
            assim_sunlit_dmax = cbind(assim_sunlit_dmax,tmp)
            tmp = apply(assim_shaded[doy_ind,],2,max,na.rm=TRUE) #get the daily max of each layer
            assim_shaded_dmax = cbind(assim_shaded_dmax,tmp)
        }
        total_assim_dmax[k,j,i] = mean(canopy_assim_dmax,na.rm=TRUE)
        layer_assim_sunlit[k,j,i,] = rowMeans(assim_sunlit_dmax,na.rm=TRUE)
        layer_assim_shaded[k,j,i,] = rowMeans(assim_shaded_dmax,na.rm=TRUE)
        if(min(layer_assim_shaded,na.rm=TRUE) < (-100)){
           print(dim(assim_shaded_dmax))
           print(assim_shaded_dmax[1:5,1:10])
           print(which(layer_assim_shaded < (-100),arr.ind=TRUE))
           stop()
        }

        if(any(is.na(layer_assim_shaded[k,j,i,]))){
              print(c(k,j,i))
              saveRDS(results,"results_nan.rds")
              print(layer_assim_shaded[k,j,i,])
              print(c(soybean_para0$jmax, soybean_para0$vmax1))
              print(c(soybean_para$jmax, soybean_para$vmax1))
              print(c(j_scaler[j], v_scaler[k]))
              stop()
        }
#for biomass and canoy_assim we save the values on those specfic doys
        for (dd in 1:length(doys)){
             doy_ind = which(DOY==doys[dd])
             grain = results[,"Grain"]
             podmass[k,j,i,dd] = max(grain[doy_ind])
             aboveground_mass = results[,"Grain"]+results[,"Leaf"]+results[,"Stem"]
             shootmass[k,j,i,dd] = max(aboveground_mass[doy_ind]) 
             total_assim[k,j,i,dd] = max(canopy_assim[doy_ind])
        }

  #      print(which(cname=="sunlit_Assim_layer_0"))
  #      print(which(cname=="shaded_Assim_layer_0"))
  #      saveRDS(results,"results_example.rds")
        rm(soybean_solver)  #make sure the solver is properly cleaned
  } 
  } 
}

t1 = Sys.time()
print(t1-t0)
#save the output for fast plotting
X1 = list(layer_assim_sunlit,layer_assim_shaded,total_assim,podmass,shootmass,Ci_sunlit,Ci_shaded,total_assim_dmax)
saveRDS(X1,file=paste("results_rds/results_CO2_",CO2,".rds",sep=""))
}

