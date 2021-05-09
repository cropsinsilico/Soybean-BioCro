plot_contour<-function(x,y,data,var_name,cbar_limit){
    s = expand.grid(x,y) 
    data[data<=cbar_limit[1]] = cbar_limit[1]  #make sure min values are also plotted!
    data[data>=cbar_limit[2]] = cbar_limit[2]
    df = data.frame(s,as.vector(data))
    colnames(df) = c("vmax","jmax","z")
    v<-ggplot(df,aes(x=vmax,y=jmax,z=z,fill=z))+geom_contour_filled()+ 
       geom_tile()+
       scale_fill_distiller(name=var_name,palette = "Spectral",limits = cbar_limit)
#       scale_fill_distiller(name=var_name,palette = "RdBu",limits = cbar_limit)

}

layers_contour<-function(x,y,var2plot,cbar_limit,pdf_name,no_layers,years){
	var2plot_layers = paste(rep("layer",no_layers),1:no_layers,sep="") 
	plot_list = list()
	for (j in 1:length(var2plot_layers)){
	        varname = var2plot_layers[j]
	        var_diff = var2plot[,,,j] 
		for (i in 1:length(years)){
		    year_i = years[i]
		    fig_i = plot_contour(x,y,var_diff[,,i],varname,cbar_limit)
	            fig_order = i+ (j-1) * length(years)
		    plot_list[[fig_order]] = fig_i 
		}
	}
	pdf(pdf_name,height = 48, width=24)
	grid.arrange(grobs = plot_list,nrow=length(var2plot_layers),ncol=length(years))
	dev.off()
}


profile_plot1<-function(y1,y2,no_layers,years,pdfname,v_use,j_use,v_scaler,j_scaler){
	plot_list = list()
        fig_order = 1
         layers = 1:no_layers
 for (i in 1:length(v_use)){
 for (j in 1:length(j_use)){
                xlabel = paste("Ci(vmax*",v_use[i],",jmax*",j_use[j],")",sep="")
		for (k in 1:length(years)){
                    v_index = which(abs(v_scaler-v_use[i])<1e-10)
                    j_index = which(abs(j_scaler-j_use[j])<1e-10)
                    if(length(v_index)!=1 | length(j_index)!=1) stop("indexing error")
                    y11 = y1[v_index,j_index,k,]
                    y22 = y2[v_index,j_index,k,]
 			df = data.frame(shaded=y11,sunlit=y22,layers)
                        df_molten=melt(df,id.vars="layers")
                     plot_list[[fig_order]] =   ggplot(df_molten,aes(x=value,y=layers,color=variable)) + 
                      geom_line()+
                      geom_point()+
		      xlab(xlabel)
 		     fig_order= fig_order+1
                }
 }
 }
        no_rows = length(v_use)*length(j_use)
	pdf(pdfname,height = 48, width=24)
	grid.arrange(grobs = plot_list,nrow=no_rows,ncol=length(years))
	dev.off()
}

profile_plot2<-function(y,no_layers,years,pdfname,v_use,j_use,v_scaler,j_scaler){

	plot_list = list()
        fig_order = 1
        layers = 1:no_layers
	for (k in 1:length(years)){
        y_sub = (1:no_layers)*NaN  #create a empty column for cbind
        col_names = c()
 	for (i in 1:length(v_use)){
 	for (j in 1:length(j_use)){
             v_index = which(abs(v_scaler-v_use[i])<1e-10)
             j_index = which(abs(j_scaler-j_use[j])<1e-10)
            if(length(v_index)!=1 | length(j_index)!=1) stop("indexing error")
             y_sub = cbind(y_sub,y[v_index,j_index,k,])
             col_names = c(col_names,paste("vmax",v_use[i],"jmax",j_use[j],sep=""))
	}      
	}      
        #after cbind, remove the first column
        y_sub = y_sub[,-1]
        colnames(y_sub) = col_names
 	     df = data.frame(y_sub,layers)
             df_molten=melt(df,id.vars="layers")
            plot_list[[fig_order]] =   ggplot(df_molten,aes(x=value,y=layers,color=variable,shape=variable)) + 
            geom_line()+
            geom_point(alpha=0.5)+
	    xlab("Ci(ppm)")+
           scale_y_continuous(breaks=c(1:no_layers))+
            theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14),
                  axis.text.y = element_text(face="bold", color="#993333", 
                           size=14),
                  axis.title=element_text(size=14),
                  legend.position = c(0.8, 0.2),
                  legend.text=element_text(size=14))
 	    fig_order= fig_order+1
	}
	pdf(pdfname,height = 12, width=24)
	grid.arrange(grobs = plot_list,nrow=1,ncol=length(years))
	dev.off()
}
# Define functions to create plots
plot_all_tissues <- function(res, year, biomass, biomass.std) {
  
  r <- reshape2::melt(res[, c("time","Root","Leaf","Stem","Grain")], id.vars="time")
  r.exp <- reshape2::melt(biomass[, c("DOY", "Leaf", "Stem", "Pod")], id.vars = "DOY")
  r.exp.std <- reshape2::melt(biomass.std[, c("DOY", "Leaf", "Stem", "Pod")], id.vars = "DOY")
  r.exp.std$ymin<-r.exp$value-r.exp.std$value
  r.exp.std$ymax<-r.exp$value+r.exp.std$value
  
  # Colorblind friendly color palette (https://personal.sron.nl/~pault/)
  col.palette.muted <- c("#332288", "#117733", "#999933", "#882255")
  
  size.title <- 12
  size.axislabel <-10
  size.axis <- 10
  size.legend <- 12
  
  f <- ggplot() + theme_classic()
  f <- f + geom_point(data=r, aes(x=time,y=value, colour=variable), show.legend = TRUE, size=0.25)
  f <- f + geom_errorbar(data=r.exp.std, aes(x=DOY, ymin=ymin, ymax=ymax), width=3.5, size=.25, show.legend = FALSE)
  f <- f + geom_point(data=r.exp, aes(x=DOY, y=value, fill=variable), shape=22, size=2, show.legend = FALSE, stroke=.5)
  f <- f + labs(title=element_blank(), x=paste0('Day of Year (',year,')'),y='Biomass (Mg / ha)')
  f <- f + coord_cartesian(ylim = c(0,10)) + scale_y_continuous(breaks = seq(0,10,2)) + scale_x_continuous(breaks = seq(150,275,30))
  f <- f + theme(plot.title=element_text(size=size.title, hjust=0.5),
                 axis.text=element_text(size=size.axis),
                 axis.title=element_text(size=size.axislabel),
                 legend.position = c(.15,.85), legend.title = element_blank(),
                 legend.text=element_text(size=size.legend),
                 legend.background = element_rect(fill = "transparent",colour = NA),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(), panel.background = element_rect(fill = "transparent",colour = NA),
                 plot.background = element_rect(fill = "transparent", colour = NA))
  f <- f + guides(colour = guide_legend(override.aes = list(size=2)))
  f <- f + scale_fill_manual(values = col.palette.muted[2:4], guide = FALSE)
  f <- f + scale_colour_manual(values = col.palette.muted, labels=c('Root','Leaf','Stem','Pod'))
  
  return(f)
}



