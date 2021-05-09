library(BioCroSoyBean)
library(ggplot2) # To ggplot functions
library(reshape2) # For melt function
library(data.table) # For first and last functions


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
ExpBiomass <- list()
ExpBiomass.std <- list()
ExpBiomass.elevCO2 <- list()
ExpBiomass.std.elevCO2 <- list()

# for partial_gro_solver
arg_names <- c('Catm') # atmospheric CO2 parameter
#params.ambient <- c(372) # ambient atmospheric CO2
#params.elevCO2 <- c(550) # elevated atmospheric CO2
CO2s <- c(380,550,800)
solar_threshold = 10
no_layers = 10
doys = c(180,220,260)
v_scaler = seq(0.5,1.5,by=0.1) 
j_scaler = seq(0.5,1.5,by=0.1) 

soybean_para0 = soybean_parameters
soybean_para  = soybean_parameters
podmass     = array(NaN,c(length(v_scaler),length(j_scaler),length(years),length(doys)))    
shootmass   = array(NaN,c(length(v_scaler),length(j_scaler),length(years),length(doys)))    
total_assim = array(NaN,c(length(v_scaler),length(j_scaler),length(years),length(doys)))    
layer_assim_sunlit = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) #mean?
layer_assim_shaded = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) #mean?
Ci_sunlit          = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) #mean?
Ci_shaded          = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) #mean?

#podmass_elevCO2     = array(NaN,c(length(v_scaler),length(j_scaler),length(years),length(doys)))    
#shootmass_elevCO2   = array(NaN,c(length(v_scaler),length(j_scaler),length(years),length(doys)))    
#total_assim_elevCO2 = array(NaN,c(length(v_scaler),length(j_scaler),length(years),length(doys)))    
#layer_assim_sunlit_elevCO2 = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) #mean?
#layer_assim_shaded_elevCO2 = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) #mean?
#Ci_sunlit_elevCO2          = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) #mean?
#Ci_shaded_elevCO2          = array(NaN,c(length(v_scaler),length(j_scaler),length(years),no_layers)) #mean?

for (CO2 in CO2s){
for (i in 1:length(years)) {
  
  yr <- years[i]
  
  weather <- read.csv(file = paste0('../Data/Weather_data/', yr, '_Bondville_IL_daylength.csv'))

  sowdate <- dates$sow[which(dates$year == yr)]
  harvestdate <- dates$harvest[which(dates$year == yr)]
  sd.ind <- which(weather$doy == sowdate)[1] # start of sowing day
  hd.ind <- which(weather$doy == harvestdate)[24] # end of harvest day
  
  weather.growingseason[[i]] <- weather[sd.ind:hd.ind,]
  solar = weather.growingseason[[i]]$solar 
  daytime_ind = which(solar>solar_threshold)

  for (j in 1:length(j_scaler)){
  for (k in 1:length(v_scaler)){
  	soybean_para$jmax =  soybean_para0$jmax   *  j_scaler[j] 
  	soybean_para$vmax1 = soybean_para0$vmax1  *  v_scaler[k]
  	soybean_solver <- partial_gro_solver(soybean_initial_state, soybean_para, weather.growingseason[[i]],
                                       soybean_steadystate_modules, soybean_derivative_modules,
                                       arg_names, soybean_solver_params)
        results         <- soybean_solver(params.ambient)
        results_elevCO2 <- soybean_solver(params.elevCO2)
        cname = colnames(results)
        sunlit_index = grep("^sunlit_Assim.*", cname) 
        shaded_index = grep("^shaded_Assim.*", cname) 
        if(length(sunlit_index) != no_layers || length(shaded_index) !=no_layers){
           stop("results issues!?")
        }
        layer_assim_sunlit[k,j,i,] = apply(results[,sunlit_index],c(2),mean,na.rm=TRUE) #apply mean for each layer
        layer_assim_shaded[k,j,i,] = apply(results[,shaded_index],c(2),mean,na.rm=TRUE) #apply mean for each layer
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

        if(any(is.na(layer_assim_shaded[k,j,i,]))){
              print(c(k,j,i))
              saveRDS(results,"results_nan.rds")
              print(layer_assim_shaded[k,j,i,])
              print(c(soybean_para0$jmax, soybean_para0$vmax1))
              print(c(soybean_para$jmax, soybean_para$vmax1))
              print(c(j_scaler[j], v_scaler[k]))
              stop()
        }
        total_assim[k,j,i,1] = mean(results[,"canopy_assimilation_rate"],na.rm=TRUE)
        total_assim[k,j,i,2] = max(results[,"canopy_assimilation_rate"],na.rm=TRUE)
        DOY = results[,"doy"]
        for (dd in 1:length(doys)){
             doy_ind = which(DOY==doys[dd])
             grain = results[,"Grain"]
             podmass[k,j,i,dd] = max(grain[doy_ind])
             aboveground_mass = results[,"Grain"]+results[,"Leaf"]+results[,"Stem"]
             shootmass[k,j,i,dd] = max(aboveground_mass[doy_ind]) 
             canopy_assim = results[,"canopy_assimilation_rate"]
             total_assim[k,j,i,dd] = max(canopy_assim[doy_ind])
        }

        cname = colnames(results_elevCO2)
        sunlit_index = grep("^sunlit_Assim.*", cname) 
        shaded_index = grep("^shaded_Assim.*", cname) 
        layer_assim_sunlit_elevCO2[k,j,i,] = apply(results_elevCO2[,sunlit_index],c(2),mean,na.rm=TRUE)
        layer_assim_shaded_elevCO2[k,j,i,] = apply(results_elevCO2[,shaded_index],c(2),mean,na.rm=TRUE)
        sunlit_index_Ci = grep("^sunlit_Ci_layer.*", cname)
        shaded_index_Ci = grep("^shaded_Ci_layer.*", cname)
        if(length(sunlit_index_Ci) != no_layers || length(shaded_index_Ci) !=no_layers){
           stop("results issues!?")
        }
#        Ci_sunlit_elevCO2[k,j,i,] = apply(results_elevCO2[,sunlit_index_Ci],c(2),mean) #apply mean for each layer
#        Ci_shaded_elevCO2[k,j,i,] = apply(results_elevCO2[,shaded_index_Ci],c(2),mean) #apply mean for each layer
        tmp = results_elevCO2[,sunlit_index_Ci]
        tmp = tmp[daytime_ind,] 
        Ci_sunlit_elevCO2[k,j,i,] = apply(tmp,c(2),mean,na.rm=TRUE) #apply mean for each layer
        tmp = results_elevCO2[,shaded_index_Ci]
        tmp = tmp[daytime_ind,] 
        Ci_shaded_elevCO2[k,j,i,] = apply(tmp,c(2),mean,na.rm=TRUE) #apply mean for each layer
        DOY = results_elevCO2[,"doy"]
        for (dd in 1:length(doys)){
             doy_ind = which(DOY==doys[dd])
             grain = results_elevCO2[,"Grain"]
             podmass_elevCO2[k,j,i,dd] = max(grain[doy_ind])
             aboveground_mass = results_elevCO2[,"Grain"]+results_elevCO2[,"Leaf"]+results_elevCO2[,"Stem"]
             shootmass_elevCO2[k,j,i,dd] = max(aboveground_mass[doy_ind]) 
             canopy_assim = results_elevCO2[,"canopy_assimilation_rate"]
             total_assim_elevCO2[k,j,i,dd] = max(canopy_assim[doy_ind])
        }
  #      print(which(cname=="sunlit_Assim_layer_0"))
  #      print(which(cname=="shaded_Assim_layer_0"))
  #      saveRDS(results,"results_example.rds")
        rm(soybean_solver)
  } 
  } 
}
}

X1 = list(layer_assim_sunlit,layer_assim_shaded,total_assim,podmass,shootmass,Ci_sunlit,Ci_shaded)
saveRDS(X1,file="results_AmbCO2.rds")
X1 = list(layer_assim_sunlit_elevCO2,layer_assim_shaded_elevCO2,total_assim_elevCO2,podmass_elevCO2,shootmass_elevCO2,Ci_sunlit_elevCO2,Ci_shaded_elevCO2)
saveRDS(X1,file="results_EleCO2.rds")

stop("ALLO1")



calc_diff<-function(res1,res2,varname,method){
    x1 = res1[,varname]
    x2 = res2[,varname]
    xdiff = x2-x1
    xdiff_percentage = (x2-x1)/x1*100
    if(method=="last"){
       return(xdiff_percentage[length(xdiff)])
    }else if(method=="max"){
       return(max(xdiff_percentage))
    }else if(method=="mean"){
       return(mean(xdiff_percentage))
    }else if(method=="min"){
       return(min(xdiff_percentage))
    }
}

difference = array(NaN,c(4,length(years)))
plot_list = list()
varname = "Grain"
method  = "last"
for (i in 1:length(years)){
    year_i = years[i]
    Fig_AmbCO2 <- plot_all_tissues(results[[i]],results[[i+length(years)]], year_i, ExpBiomass[[1]], ExpBiomass.std[[1]],"AmbCO2")
    Fig_EleCO2 <- plot_all_tissues(results.elevCO2[[i]],results.elevCO2[[i+length(years)]], year_i, ExpBiomass[[1]], ExpBiomass.std[[1]],"EleCO2")
    tmp1 = calc_diff(results[[i]],results[[i+length(years)]],varname,method)
    tmp2 = calc_diff(results.elevCO2[[i]],results.elevCO2[[i+length(years)]],varname,method)
    difference[,i] = c(tmp1,tmp2) 
    plot_list[[i]] = Fig_AmbCO2 
    plot_list[[length(years)+i]] = Fig_EleCO2 
}
write.csv(difference,"diff.csv")
pdf("Fig_sensitivity.pdf",width=16)
grid.arrange(grobs = plot_list,nrow=2,ncol=length(years))
dev.off()
