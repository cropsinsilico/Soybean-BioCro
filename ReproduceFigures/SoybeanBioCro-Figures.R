library(BioCro)
library(ggplot2) # To ggplot functions
library(reshape2) # For melt function


# Clear workspace
rm(list=ls())

# Set working directory to location of this file
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# turn off warnings
options(warn=-1)

# if needed, create ./figs/ directory
path.figs <- './figs/'
dir.create(path = path.figs, showWarnings = FALSE)

years <- c('2002','2004','2005','2006')

# sowing and harvest DOYs for each growing season
dates <- data.frame("year" = c(2002, 2004:2006),"sow" = c(152,149,148,148), "harvest" = c(288, 289, 270, 270))

# initialize variables
results <- list()
results.elevCO2 <- list()
weather.growingseason <- list()
ExpBiomass <- list()
ExpBiomass.std <- list()
ExpBiomass.elevCO2 <- list()
ExpBiomass.std.elevCO2 <- list()

# for partial_gro_solver
arg_names <- c('Catm') # atmospheric CO2 parameter
params.ambient <- c(372) # ambient atmospheric CO2
params.elevCO2 <- c(550) # elevated atmospheric CO2

for (i in 1:length(years)) {
  
  yr <- years[i]
  
  weather <- read.csv(file = paste0('../Data/Weather_data/', yr, '_Bondville_IL_daylength.csv'))

  sowdate <- dates$sow[which(dates$year == yr)]
  harvestdate <- dates$harvest[which(dates$year == yr)]
  sd.ind <- which(weather$doy == sowdate)[1] # start of sowing day
  hd.ind <- which(weather$doy == harvestdate)[24] # end of harvest day
  
  weather.growingseason[[i]] <- weather[sd.ind:hd.ind,]

  soybean_solver <- partial_gro_solver(soybean_initial_state, soybean_parameters, weather.growingseason[[i]],
                                       soybean_steadystate_modules, soybean_derivative_modules,
                                       arg_names, soybean_solver_params)
  
  results[[i]] <- soybean_solver(params.ambient)
  results.elevCO2[[i]] <- soybean_solver(params.elevCO2)
  
  # Load SoyFACE ambient and elevated CO2 biomass means and standard deviations
  ExpBiomass[[i]]<-read.csv(file=paste0('../Data/SoyFACE_data/', yr,'_ambient_biomass.csv'))
  ExpBiomass.std[[i]]<-read.csv(file=paste0('../Data/SoyFACE_data/', yr,'_ambient_biomass_std.csv'))
  colnames(ExpBiomass[[i]])<-c("DOY","Leaf","Stem","Pod")
  colnames(ExpBiomass.std[[i]])<-c("DOY","Leaf","Stem","Pod")
  
  ExpBiomass.elevCO2[[i]]<-read.csv(file = paste0('../Data/SoyFACE_data/',yr,'_co2_biomass.csv'))
  ExpBiomass.std.elevCO2[[i]]<-read.csv(file = paste0('../Data/SoyFACE_data/',yr,'_co2_biomass_std.csv'))
  colnames(ExpBiomass.elevCO2[[i]])<-c("DOY","Leaf","Stem","Pod")
  colnames(ExpBiomass.std.elevCO2[[i]])<-c("DOY","Leaf","Stem","Pod")
  
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

plot_partitioning <- function(res, year) {
  
  # Colorblind friendly color palette (https://personal.sron.nl/~pault/)
  col.palette.muted <- c("#332288", "#117733", "#999933", "#882255")
  
  size.title <- 12
  size.axislabel <-10
  size.axis <- 10
  size.legend <- 12
  
  r.partcoeffs.doy <- reshape2::melt(res[,c("time","kRoot","kLeaf","kStem","kGrain")],id.vars="time")
  r.partcoeffs.doy$value<-100*r.partcoeffs.doy$value
  f <- ggplot() + theme_classic()
  f <- f + geom_point(data=r.partcoeffs.doy, aes(x=time, y=value, colour=variable),show.legend = TRUE,size=0.25)
  f <- f + labs(title=element_blank(), x=paste0('Day of Year (',year,')'),y='% Allocated')
  f <- f + theme(plot.title=element_text(size=size.title, hjust=0.5),
                 axis.text=element_text(size=size.axis),
                 axis.title=element_text(size=size.axislabel),
                 legend.position = c(.85,.45), legend.title = element_blank(),
                 legend.text=element_text(size=size.legend),
                 legend.background = element_rect(fill = "transparent",colour = NA),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(), panel.background = element_rect(fill = "transparent",colour = NA),
                 plot.background = element_rect(fill = "transparent", colour = NA))
  f <- f + scale_x_continuous(breaks = seq(150,275,30))
  f <- f + guides(colour = guide_legend(override.aes = list(size=2)))
  f <- f + scale_colour_manual(values = col.palette.muted,labels=c("Root","Leaf","Stem","Pod"))

  return(f)
}

plot_amb_elev_leaf <- function(res, elev_res, year, biomass, biomass.std, elev_biomass, elev_biomass.std) {
  
  # Colorblind friendly color palette (https://personal.sron.nl/~pault/)
  col.palette.muted <- c("#332288", "#117733", "#999933", "#882255")
  
  size.title <- 12
  size.axislabel <-10
  size.axis <- 10
  size.legend <- 12
  
  s.leaf <- cbind(res[,c("time","Leaf")],elev_res[,"Leaf"])
  colnames(s.leaf) <- c("time","Amb","Elev")
  r.leaf <- reshape2::melt(s.leaf, id.vars = "time")

  s.exp.leaf <- cbind(biomass[,c("DOY","Leaf")],elev_biomass[,"Leaf"])
  colnames(s.exp.leaf) <- c("DOY","Amb","Elev")
  r.exp.leaf <- reshape2::melt(s.exp.leaf, id.vars = "DOY")

  s.exp.std.leaf <- cbind(biomass.std[,c("DOY","Leaf")],elev_biomass.std[,"Leaf"])
  colnames(s.exp.std.leaf) <- c("DOY","Amb","Elev")
  r.exp.std.leaf <- reshape2::melt(s.exp.std.leaf, id.vars = "DOY")
  r.exp.std.leaf$ymin <- r.exp.leaf$value - r.exp.std.leaf$value
  r.exp.std.leaf$ymax <- r.exp.leaf$value + r.exp.std.leaf$value

  f <- ggplot() + theme_classic()
  f <- f + geom_point(data=r.leaf, aes(x=time, y=value, colour=variable),show.legend = TRUE,size=0.25)
  f <- f + geom_errorbar(data=r.exp.std.leaf, aes(x=DOY, ymin=ymin, ymax=ymax), width=3.5, size=0.25, show.legend = FALSE)
  f <- f + geom_point(data=r.exp.leaf, aes(x=DOY, y=value, fill=variable), shape=22, size=2, show.legend = FALSE, stroke=.5)
  f <- f + coord_cartesian(ylim = c(0,5)) + scale_x_continuous(breaks = seq(150,275,30))
  f <- f + labs(x=paste0('Day of Year (',year,')'),y='Leaf Biomass (Mg / ha)')
  f <- f + theme(plot.title=element_text(size=size.title, hjust=0.5),
                 axis.text=element_text(size=size.axis),
                 axis.title=element_text(size=size.axislabel),
                 legend.position = c(.25,.85), legend.title = element_blank(),
                 legend.text=element_text(size=size.legend),
                 legend.background = element_rect(fill = "transparent",colour = NA),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(), panel.background = element_rect(fill = "transparent",colour = NA),
                 plot.background = element_rect(fill = "transparent", colour = NA))
  f <- f + guides(colour = guide_legend(override.aes = list(size=2)))
  f <- f + scale_fill_manual(values = col.palette.muted[2:3], guide = FALSE)
  f <- f + scale_colour_manual(values = col.palette.muted[2:3],labels=c('Ambient',bquote(Elevated~CO[2])))

  return(f)
}

plot_amb_elev_stem <- function(res, elev_res, year, biomass, biomass.std, elev_biomass, elev_biomass.std) {
  
  # Colorblind friendly color palette (https://personal.sron.nl/~pault/)
  col.palette.muted <- c("#332288", "#117733", "#999933", "#882255")
  
  size.title <- 12
  size.axislabel <-10
  size.axis <- 10
  size.legend <- 12
  
  s.stem <- cbind(res[,c("time","Stem")],elev_res[,"Stem"])
  colnames(s.stem) <- c("time","Amb","Elev")
  r.stem <- reshape2::melt(s.stem, id.vars = "time")
  
  s.exp.stem <- cbind(biomass[,c("DOY","Stem")],elev_biomass[,"Stem"])
  colnames(s.exp.stem) <- c("DOY","Amb","Elev")
  r.exp.stem <- reshape2::melt(s.exp.stem, id.vars = "DOY")
  
  s.exp.std.stem <- cbind(biomass.std[,c("DOY","Stem")],elev_biomass.std[,"Stem"])
  colnames(s.exp.std.stem) <- c("DOY","Amb","Elev")
  r.exp.std.stem <- reshape2::melt(s.exp.std.stem, id.vars = "DOY")
  r.exp.std.stem$ymin <- r.exp.stem$value - r.exp.std.stem$value
  r.exp.std.stem$ymax <- r.exp.stem$value + r.exp.std.stem$value
  
  f <- ggplot() + theme_classic()
  f <- f + geom_point(data=r.stem, aes(x=time, y=value, colour=variable),show.legend = TRUE,size=0.25)
  f <- f + geom_errorbar(data=r.exp.std.stem, aes(x=DOY, ymin=ymin, ymax=ymax), width=3.5, size=0.25, show.legend = FALSE)
  f <- f + geom_point(data=r.exp.stem, aes(x=DOY, y=value, fill=variable), shape=22, size=2, show.legend = FALSE, stroke=.5)
  f <- f + coord_cartesian(ylim = c(0,10)) + scale_x_continuous(breaks = seq(150,275,30))
  f <- f + labs(x=paste0('Day of Year (',year,')'),y='Stem Biomass (Mg / ha)')
  f <- f + theme(plot.title=element_text(size=size.title, hjust=0.5),
                 axis.text=element_text(size=size.axis),
                 axis.title=element_text(size=size.axislabel),
                 legend.position = c(.25,.85), legend.title = element_blank(),
                 legend.text=element_text(size=size.legend),
                 legend.background = element_rect(fill = "transparent",colour = NA),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(), panel.background = element_rect(fill = "transparent",colour = NA),
                 plot.background = element_rect(fill = "transparent", colour = NA))
  f <- f + guides(colour = guide_legend(override.aes = list(size=2)))
  f <- f + scale_fill_manual(values = col.palette.muted[2:3], guide = FALSE)
  f <- f + scale_colour_manual(values = col.palette.muted[2:3],labels=c('Ambient',bquote(Elevated~CO[2])))
  
  return(f)
  
}

plot_amb_elev_pod <- function(res, elev_res, year, biomass, biomass.std, elev_biomass, elev_biomass.std, dvi.flag) {
  
  # Colorblind friendly color palette (https://personal.sron.nl/~pault/)
  col.palette.muted <- c("#332288", "#117733", "#999933", "#882255")
  
  size.title <- 12
  size.axislabel <-10
  size.axis <- 10
  size.legend <- 12
  
  s.pod <- cbind(res[,c("time","Grain")],elev_res[,"Grain"])
  colnames(s.pod) <- c("time","Amb","Elev")
  r.pod <- reshape2::melt(s.pod, id.vars = "time")
  
  s.exp.pod <- cbind(biomass[,c("DOY","Pod")],elev_biomass[,"Pod"])
  colnames(s.exp.pod) <- c("DOY","Amb","Elev")
  r.exp.pod <- reshape2::melt(s.exp.pod, id.vars = "DOY")
  
  s.exp.std.pod <- cbind(biomass.std[,c("DOY","Pod")],elev_biomass.std[,"Pod"])
  colnames(s.exp.std.pod) <- c("DOY","Amb","Elev")
  r.exp.std.pod <- reshape2::melt(s.exp.std.pod, id.vars = "DOY")
  r.exp.std.pod$ymin <- r.exp.pod$value - r.exp.std.pod$value
  r.exp.std.pod$ymax <- r.exp.pod$value + r.exp.std.pod$value
  
  dvi.1 <- first(which(res$DVI >= 1 & res$DVI < 1.5))
  dvi.1.5 <- last(which(res$DVI >= 1 & res$DVI < 1.5))
  
  f <- ggplot() + theme_classic()
  if(dvi.flag) {
  f <- f + geom_rect(aes(xmin=res$time[dvi.1],ymin=-5,xmax=res$time[dvi.1.5],ymax=40),fill='#FFF7BC')
  }
  f <- f + geom_point(data=r.pod, aes(x=time, y=value, colour=variable),show.legend = TRUE,size=0.25)
  f <- f + geom_errorbar(data=r.exp.std.pod, aes(x=DOY, ymin=ymin, ymax=ymax), width=3.5, size=0.25, show.legend = FALSE)
  f <- f + geom_point(data=r.exp.pod, aes(x=DOY, y=value, fill=variable), shape=22, size=2, show.legend = FALSE, stroke=.5)
  f <- f + coord_cartesian(ylim = c(0,10)) + scale_x_continuous(breaks = seq(150,275,30))
  f <- f + labs(x=paste0('Day of Year (',year,')'),y='Pod Biomass (Mg / ha)')
  f <- f + theme(plot.title=element_text(size=size.title, hjust=0.5),
                 axis.text=element_text(size=size.axis),
                 axis.title=element_text(size=size.axislabel),
                 legend.position = c(.25,.85), legend.title = element_blank(),
                 legend.text=element_text(size=size.legend),
                 legend.background = element_rect(fill = "transparent",colour = NA),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(), panel.background = element_rect(fill = "transparent",colour = NA),
                 plot.background = element_rect(fill = "transparent", colour = NA))
  f <- f + guides(colour = guide_legend(override.aes = list(size=2)))
  f <- f + scale_fill_manual(values = col.palette.muted[2:3], guide = FALSE)
  f <- f + scale_colour_manual(values = col.palette.muted[2:3],labels=c('Ambient',bquote(Elevated~CO[2])))
 
  return(f) 
}

plot_temps <- function(res, year, pod.flag) {
  
  temp <- res[, c("time","temp")]
  
  temp$Fifteen <- matrix(data=NA, nrow=nrow(res), ncol=1)
  temp$Ten <- matrix(data=NA, nrow=nrow(res), ncol=1)
  
  temp$Fifteen[which((temp$temp<15)&temp$temp>=10)] <- temp$temp[which((temp$temp<15)&temp$temp>=10)]
  temp$Ten[which(temp$temp<10)] <- temp$temp[which(temp$temp<10)]
  
  temp.melt <- reshape2::melt(temp, id.vars = 'time')
  
  dvi.1 <- first(which(res$DVI >= 1 & res$DVI < 1.5))
  dvi.1.5 <- last(which(res$DVI >= 1 & res$DVI < 1.5))
  
  size.title <- 12
  size.axislabel <-10
  size.axis <- 10

  f <- ggplot() + theme_classic()
  f <- f + geom_rect(aes(xmin=res$time[dvi.1],ymin=-5,xmax=res$time[dvi.1.5],ymax=40),fill='#FFF7BC')
  f <- f + geom_point(data=temp.melt, aes(x=time, y=value, color=variable), na.rm=TRUE, show.legend = FALSE)
  f <- f + labs(title=element_blank(), x=paste0('Day of Year (',year, ')'), y='Temperature (deg C)')
  f <- f + theme(plot.title=element_text(size=size.title, hjust=0.5),axis.text=element_text(size=size.axis), axis.title=element_text(size=size.axislabel))
  f <- f + coord_cartesian(ylim=c(0,35))
  f <- f + scale_y_continuous(breaks = seq(0,35,5)) + scale_x_continuous(breaks =  seq(150,275,30))
  f <- f + scale_color_manual(values = c("#A9A9A9","#000000","#0000FF"))
  
  return(f)

}

# Reproduce Fig 1
Fig1A <- plot_all_tissues(results[[1]], '2002', ExpBiomass[[1]], ExpBiomass.std[[1]])
ggsave(filename = paste0("./figs/Fig1A.pdf"),width=3.5,height=2.8,units="in",bg='transparent')

Fig1B <- plot_all_tissues(results[[3]], '2005', ExpBiomass[[3]], ExpBiomass.std[[3]])
ggsave(filename = paste0("./figs/Fig1B.pdf"),width=3.5,height=2.8,units="in",bg='transparent')

Fig1C <- plot_partitioning(results[[1]], '2002')
ggsave(filename = paste0("./figs/Fig1C.pdf"),width=3.5,height=2.8,units="in",bg='transparent')


# Reproduce Fig 2
i<-1 # 2002
Fig2A <- plot_amb_elev_leaf(results[[i]], results.elevCO2[[i]], years[i], ExpBiomass[[i]], ExpBiomass.std[[i]], ExpBiomass.elevCO2[[i]], ExpBiomass.std.elevCO2[[i]])
ggsave(filename = paste0("./figs/Fig2A.pdf"),width=3.5,height=2.8,units="in",bg='transparent')

Fig2E <- plot_amb_elev_stem(results[[i]], results.elevCO2[[i]], years[i], ExpBiomass[[i]], ExpBiomass.std[[i]], ExpBiomass.elevCO2[[i]], ExpBiomass.std.elevCO2[[i]])
ggsave(filename = paste0("./figs/Fig2E.pdf"),width=3.5,height=2.8,units="in",bg='transparent')

Fig2I <- plot_amb_elev_pod(results[[i]], results.elevCO2[[i]], years[i], ExpBiomass[[i]], ExpBiomass.std[[i]], ExpBiomass.elevCO2[[i]], ExpBiomass.std.elevCO2[[i]], 0)
ggsave(filename = paste0("./figs/Fig2I.pdf"),width=3.5,height=2.8,units="in",bg='transparent')

i<-2 # 2004
Fig2B <- plot_amb_elev_leaf(results[[i]], results.elevCO2[[i]], years[i], ExpBiomass[[i]], ExpBiomass.std[[i]], ExpBiomass.elevCO2[[i]], ExpBiomass.std.elevCO2[[i]])
ggsave(filename = paste0("./figs/Fig2B.pdf"),width=3.5,height=2.8,units="in",bg='transparent')

Fig2F <- plot_amb_elev_stem(results[[i]], results.elevCO2[[i]], years[i], ExpBiomass[[i]], ExpBiomass.std[[i]], ExpBiomass.elevCO2[[i]], ExpBiomass.std.elevCO2[[i]])
ggsave(filename = paste0("./figs/Fig2F.pdf"),width=3.5,height=2.8,units="in",bg='transparent')

Fig2J <- plot_amb_elev_pod(results[[i]], results.elevCO2[[i]], years[i], ExpBiomass[[i]], ExpBiomass.std[[i]], ExpBiomass.elevCO2[[i]], ExpBiomass.std.elevCO2[[i]], 0)
ggsave(filename = paste0("./figs/Fig2J.pdf"),width=3.5,height=2.8,units="in",bg='transparent')

i<-3 # 2005
Fig2C <- plot_amb_elev_leaf(results[[i]], results.elevCO2[[i]], years[i], ExpBiomass[[i]], ExpBiomass.std[[i]], ExpBiomass.elevCO2[[i]], ExpBiomass.std.elevCO2[[i]])
ggsave(filename = paste0("./figs/Fig2C.pdf"),width=3.5,height=2.8,units="in",bg='transparent')

Fig2G <- plot_amb_elev_stem(results[[i]], results.elevCO2[[i]], years[i], ExpBiomass[[i]], ExpBiomass.std[[i]], ExpBiomass.elevCO2[[i]], ExpBiomass.std.elevCO2[[i]])
ggsave(filename = paste0("./figs/Fig2G.pdf"),width=3.5,height=2.8,units="in",bg='transparent')

Fig2K <- plot_amb_elev_pod(results[[i]], results.elevCO2[[i]], years[i], ExpBiomass[[i]], ExpBiomass.std[[i]], ExpBiomass.elevCO2[[i]], ExpBiomass.std.elevCO2[[i]], 0)
ggsave(filename = paste0("./figs/Fig2K.pdf"),width=3.5,height=2.8,units="in",bg='transparent')

i<-4 # 2006
Fig2D <- plot_amb_elev_leaf(results[[i]], results.elevCO2[[i]], years[i], ExpBiomass[[i]], ExpBiomass.std[[i]], ExpBiomass.elevCO2[[i]], ExpBiomass.std.elevCO2[[i]])
ggsave(filename = paste0("./figs/Fig2D.pdf"),width=3.5,height=2.8,units="in",bg='transparent')

Fig2H <- plot_amb_elev_stem(results[[i]], results.elevCO2[[i]], years[i], ExpBiomass[[i]], ExpBiomass.std[[i]], ExpBiomass.elevCO2[[i]], ExpBiomass.std.elevCO2[[i]])
ggsave(filename = paste0("./figs/Fig2H.pdf"),width=3.5,height=2.8,units="in",bg='transparent')

Fig2L <- plot_amb_elev_pod(results[[i]], results.elevCO2[[i]], years[i], ExpBiomass[[i]], ExpBiomass.std[[i]], ExpBiomass.elevCO2[[i]], ExpBiomass.std.elevCO2[[i]], 0)
ggsave(filename = paste0("./figs/Fig2L.pdf"),width=3.5,height=2.8,units="in",bg='transparent')


# Reproduce Fig 3
i<-1 # 2002
Fig3A <- plot_temps(results[[i]], years[[i]])
ggsave(filename = paste0("./figs/Fig3A.pdf"),width=3.5,height=2.8,units="in",bg='transparent')

i<-2 # 2004
Fig3B <- plot_temps(results[[i]], years[[i]])
ggsave(filename = paste0("./figs/Fig3B.pdf"),width=3.5,height=2.8,units="in",bg='transparent')

Fig3E <- plot_amb_elev_pod(results[[i]], results.elevCO2[[i]], years[i], ExpBiomass[[i]], ExpBiomass.std[[i]], ExpBiomass.elevCO2[[i]], ExpBiomass.std.elevCO2[[i]], 1)
ggsave(filename = paste0("./figs/Fig3E.pdf"),width=3.5,height=2.8,units="in",bg='transparent')

i<-3 # 2005
Fig3C <- plot_temps(results[[i]], years[[i]])
ggsave(filename = paste0("./figs/Fig3C.pdf"),width=3.5,height=2.8,units="in",bg='transparent')

i<-4 # 2006
Fig3D <- plot_temps(results[[i]], years[[i]])
ggsave(filename = paste0("./figs/Fig3D.pdf"),width=3.5,height=2.8,units="in",bg='transparent')


# return warnings to default
options(warn=0)

