library(gridExtra)
library(lattice)
library(reshape2)
library(ggplot2) # To ggplot functions
library(metR)
library(akima)

calc_difference<-function(x1,x0){

    x0[abs(x0) < 0.1] = NaN #remove x0 that are too small to calculate percentages 

    y=(x1-x0)/x0*100

    return(y)
}

source("../plot_functions.R")
CO2s <- c(380,550,800,1000)
years <- c('2002','2004','2005','2006')
doys = c(180,220,260)
no_layers = 10
v_scaler = seq(0.5,1.5,by=0.1) 
j_scaler = seq(0.5,1.5,by=0.1) 
doy_to_plot = 260
doy_ind = which(doys==doy_to_plot)

barplot_output=c()
for (CO2 in CO2s){
   barplot_output = rbind(barplot_output,data.frame(year = years,CO2 = CO2))
} 
num_vars = 5 #pod, shoot, A_dmax, A_dmean, A_sum
array_init = array(NaN,c(dim(barplot_output)[1],num_vars))
colnames(array_init)=c("pod", "shoot", "A_dmax", "A_dmean", "A_sum")
barplot_output = cbind(barplot_output,array_init)

for (ii in 1:length(CO2s)){
CO2_case = CO2s[ii] 
X1 = readRDS(paste("../results_rds/results_CO2_",CO2_case,".rds",sep=""))
total_assim_all = X1[[3]] #these are at specfic days
podmass_all     = X1[[4]] #these are at specfic days
shootmass_all   = X1[[5]] #these are at specfic days
total_assim_dmax = X1[[8]]  #array(NaN,c(length(v_scaler),length(j_scaler),length(years),3))

layer_assim_sunlit = X1[[1]]  #array(NaN,c(length(v_scaler),length(j_scaler),length(years),10))
layer_assim_shaded = X1[[2]]  #array(NaN,c(length(v_scaler),length(j_scaler),length(years),10))
Ci_sunlit = X1[[6]]  #array(NaN,c(length(v_scaler),length(j_scaler),length(years),10))
Ci_shaded = X1[[7]]  #array(NaN,c(length(v_scaler),length(j_scaler),length(years),10))
#print(c(min(Ci_sunlit),max(Ci_sunlit)))
#print(c(min(Ci_shaded),max(Ci_shaded)))
#print(c(min(Ci_shaded[,,,1]),max(Ci_shaded[,,,1])))
#print(c(min(Ci_shaded[,,,10]),max(Ci_shaded[,,,10])))
#print(c(min(layer_assim_shaded[,,2,2]),max(layer_assim_shaded[,,2,2])))
#print(layer_assim_shaded[,,2,2])
#stop()
podmass = podmass_all[,,,doy_ind]
podmass_diff = podmass
shootmass = shootmass_all[,,,doy_ind]
shootmass_diff = shootmass
total_assim = total_assim_all[,,,doy_ind]  
total_assim_diff = total_assim
total_assim_dmax_diff = total_assim_dmax
for (j in 1:length(j_scaler)){
for (k in 1:length(v_scaler)){
   x0 = podmass[6,6,]
   podmass_diff[k,j,] = (podmass[k,j,] - x0)/x0*100  
   x0 = shootmass[6,6,]
   shootmass_diff[k,j,] = (shootmass[k,j,] - x0)/x0*100  
   x0 = total_assim[6,6,]
   total_assim_diff[k,j,] = (total_assim[k,j,] - x0)/x0*100  
   x0 = total_assim_dmax[6,6,,]

   total_assim_dmax_diff[k,j,,] = (total_assim_dmax[k,j,,] - x0)/x0*100  
}
}
#print(total_assim_dmax[6,6,,])
#print(total_assim_dmax[8,8,,])
#print(total_assim_dmax_diff[8,8,,])
#print(c(min(total_assim_diff),max(total_assim_diff)))
#stop()
#list_diff = list(podmass_diff,shootmass_diff,total_assim_diff,total_assim_dmax_diff)
list_diff = list(podmass_diff,shootmass_diff,total_assim_diff,total_assim_dmax_diff[,,,1],total_assim_dmax_diff[,,,2],total_assim_dmax_diff[,,,3])

layer_assim_sunlit_diff = layer_assim_sunlit
layer_assim_shaded_diff = layer_assim_shaded
Ci_sunlit_diff = Ci_sunlit
Ci_shaded_diff = Ci_shaded
for (l in 1:no_layers){
for (j in 1:length(j_scaler)){
for (k in 1:length(v_scaler)){
   x0 = layer_assim_sunlit[6,6,,l]
   layer_assim_sunlit_diff[k,j,,l] = calc_difference(layer_assim_sunlit[k,j,,l],x0) 
   x0 = layer_assim_shaded[6,6,,l]
   layer_assim_shaded_diff[k,j,,l] = calc_difference(layer_assim_shaded[k,j,,l],x0) 
   x0 = Ci_sunlit[6,6,,l]
   Ci_sunlit_diff[k,j,,l] = calc_difference(Ci_sunlit[k,j,,l],x0) 
   x0 = Ci_shaded[6,6,,l]
   Ci_shaded_diff[k,j,,l] = calc_difference(Ci_shaded[k,j,,l],x0) 
}
}
}
#print(layer_assim_shaded[6,6,2,])
#print(layer_assim_shaded[,,2,2])
#print(layer_assim_shaded[,,1,5])
#print(layer_assim_shaded[,,2,5])
#print(c(min(Ci_sunlit_diff),max(Ci_sunlit_diff)))
#print(c(min(Ci_shaded_diff),max(Ci_shaded_diff)))
#print(c(min(layer_assim_sunlit_diff),max(layer_assim_sunlit_diff)))
#print(c(min(layer_assim_shaded_diff),max(layer_assim_shaded_diff)))
#print(c(min(layer_assim_sunlit),max(layer_assim_sunlit)))
print(c(min(total_assim_dmax_diff),max(total_assim_dmax_diff)))
#print(layer_assim_shaded[6,6,3,])
#print(layer_assim_shaded[11,11,3,])
#print(which(layer_assim_shaded<(-100),arr.ind=TRUE))
#print(which(layer_assim_sunlit<(-100),arr.ind=TRUE))
#stop()

cbar_limit = c(-30,10)
#var2plot = c("POD","SHOOT","ASSIM","A_DMAX")
var2plot = c("POD","SHOOT","A_DMAX","A_DMEAN","A_SUM")
plot_list = list()
for (j in 1:length(var2plot)){
        varname = var2plot[j]
        var_diff = list_diff[[j]]
#        if(j==3) cbar_limit = c(-40,40)
#        if(j==4) cbar_limit = c(-20,20)
	for (i in 1:length(years)){
	    year_i = years[i]
	    fig_i = plot_contour(v_scaler,j_scaler,var_diff[,,i],varname,cbar_limit)
#Adding the max gradient line
            xy_trace = gradient_desc(v_scaler,j_scaler,var_diff[,,i],1) #1: descent; 2: ascent
            new_df = as.data.frame(xy_trace)
            fig_i <- fig_i+ geom_point(data = new_df,aes(x=V1,y=V2),inherit.aes = FALSE)
            fig_order = i+ (j-1) * length(years)
	    plot_list[[fig_order]] = fig_i 
	}
}
v_use = 1.2
j_use = 1.2
v_index = which(abs(v_scaler-v_use)<1e-10)
j_index = which(abs(j_scaler-j_use)<1e-10)

pdf(paste("Fig_CO2_",CO2_case,"_doy",doy_to_plot,"_asc.pdf",sep=""),height = 24, width=24)
grid.arrange(grobs = plot_list,nrow=length(var2plot),ncol=length(years))
dev.off()

tmp0 = cbind(podmass_diff[v_index,j_index,],shootmass_diff[v_index,j_index,],total_assim_dmax_diff[v_index,j_index,,])
barplot_output[barplot_output$CO2 == CO2_case,-c(1,2)] = tmp0 
#write.csv(csv_output,paste("diff_",CO2_case,".csv",sep=""))

#if(ii==1) z_gd = total_assim_dmax_diff[,,1,2] 

}#end for CO2s

#bar plot
#saveRDS(barplot_output,"barplot_output.rds")
pdfname = "barplot1.pdf"
#barplot(barplot_output,pdfname)


#gradient descent
#cbar_limit = c(-30,10) 
#var_name = "A_DMEAN" 
#xy_trace = gradient_desc(v_scaler,j_scaler,z_gd,2)
#new_df = as.data.frame(xy_trace)
##saveRDS(xy_trace,"xy_trace.rds")
#p = plot_contour(v_scaler,j_scaler,z_gd,var_name,cbar_limit)
#p <- p + geom_point(data = new_df,aes(x=V1,y=V2),inherit.aes = FALSE)
#pdf(paste("Fig_CO2_",CO2_case,"_gd.pdf",sep=""),height = 12, width=12)
#print(p)
##grid.arrange(grobs = plot_list,nrow=length(var2plot),ncol=length(years))
#dev.off()
stop()

#Look at the diagonal or horizontal,averaged all years
pod_diag = c()
shoot_diag = c()
Admax_diag = c()
for (i in 1:length(v_scaler)){
   tmp = mean(podmass_diff[i,6,],na.rm=TRUE)
   pod_diag = c(pod_diag,tmp) 
   tmp = mean(shootmass_diff[i,6,],na.rm=TRUE)
   shoot_diag = c(shoot_diag,tmp) 
   tmp = mean(total_assim_dmax_diff[i,6,],na.rm=TRUE)
   Admax_diag= c(Admax_diag,tmp) 
}
csv_output = cbind(pod_diag,shoot_diag,Admax_diag)
write.csv(csv_output,paste("vmax_",CO2_case,".csv",sep=""))

########################## plot layers' assimilation 

#sunlit assim, mean of daily max, layered
cbar_limit = c(-20,20)
pdfname = paste("Fig_",CO2_case,"_layers_sunlit.pdf",sep="")
var2plot = layer_assim_sunlit_diff  
#layers_contour(v_scaler,j_scaler,var2plot,cbar_limit,pdfname,no_layers,years)

#shaded assim, mean of daily max, layered
cbar_limit = c(-20,20)
pdfname = paste("Fig_",CO2_case,"_layers_shaded.pdf",sep="")
var2plot = layer_assim_shaded_diff  
#layers_contour(v_scaler,j_scaler,var2plot,cbar_limit,pdfname,no_layers,years)

########################## plot layers' Ci
#cbar_limit = c(300,450)
#cbar_limit = c(300,330)
cbar_limit = c(-30,30)
pdfname = paste("Fig_",CO2_case,"_Ci_sunlit_diff.pdf",sep="")
var2plot = Ci_sunlit_diff  
#layers_contour(v_scaler,j_scaler,var2plot,cbar_limit,pdfname,no_layers,years)
cbar_limit = c(-10,2)
pdfname = paste("Fig_",CO2_case,"_Ci_shaded_diff.pdf",sep="")
var2plot = Ci_shaded_diff  
#layers_contour(v_scaler,j_scaler,var2plot,cbar_limit,pdfname,no_layers,years)


#vertical profile

v_use = c(1,1.2)
j_use = c(1,1.2)
var2plot1 = Ci_shaded_diff  
var2plot2 = Ci_sunlit_diff #shaded first, then sunlit 
pdfname = paste("Fig_",CO2_case,"Ci_profile_sunlit.pdf",sep="")
#profile_plot1(var2plot1,var2plot2,no_layers,years,pdfname,v_use,j_use,v_scaler,j_scaler) #include both sunlit and shaded
#profile_plot2(var2plot2,no_layers,years,pdfname,v_use,j_use,v_scaler,j_scaler) #looking at sunlit or shaded once at a time

# below is to plot biomass time series, This requires the raw results output from BioCro
stop()

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
pdf("Fig_timeseries_sensitivity.pdf",width=16)
grid.arrange(grobs = plot_list,nrow=2,ncol=length(years))
dev.off()
