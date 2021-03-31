## Optimization problem to solve for partitioning parameters for soybean
library(BioCro)

multiyear_BioCro_optim <- function(optim_params, biocro.fun, ExpData, num_rows, weights, wts2,RootVals){
  
  cost.avg <- 0
  
  for (i in 1:length(biocro.fun)) {
    
    gro.opt <- match.fun(biocro.fun[[i]])
    
    result <- gro.opt(optim_params)
    
    if (nrow(result) < num_rows[i]) {
      
      cost.avg <- 1e10
      break
      
    } else{
      
      TrueValues <- ExpData[[i]]
      
      Pred <- data.frame("DOY"=TrueValues$DOY)
      doy_inds <- which(result$doy_dbl %in% TrueValues$DOY)
      Pred$Stem <- result$Stem[doy_inds]
      Pred$Leaf <- result$Leaf[doy_inds]
      Pred$Pod <- result$Grain[doy_inds]
      
      doy_inds.Root <- which(result$doy_dbl %in% RootVals[[i]]$DOY)
      Pred.Root <- result$Root[doy_inds.Root]
      
      # for specifying points of Root partitioning factor, can ignore this by setting w.kroot (below) to 0
      # pcoef<-data.frame("DVI"=c(0,.25,.5,.75,1,1.25,1.5,1.75))
      # pcoef$kRoot<-c(0.5,0.4,0.3,0.25,.01,0,0,0)
      # pcoef <- data.frame("DVI"=0)
      # pcoef$kRoot <- 0.5
      # 
      # k_ind <- NULL
      # for (i in 1:length(pcoef$DVI)) {
      #   k_ind[i] <- which.min(abs(result$DVI - pcoef$DVI[i]))
      # }
      # 
      # pcoef$kRoot.pred <- result$kRoot[k_ind]
      
      # factor to scale experimental and simulated results between 0 and ~1 for all components
      scale.leaf <- max(TrueValues$Leaf)
      scale.stem <- max(TrueValues$Stem)
      scale.rep <- max(TrueValues$Rep)
      # scale.kRoot <- max(pcoef$kRoot)
      scale.Root <- max(RootVals[[i]]$Root)
      
      # weights
      wts <- weights[[i]]
      
      err.stem <- sum(wts$Stem*(((Pred$Stem-TrueValues$Stem)/scale.stem)^2))/length(Pred$Stem)
      err.leaf <- sum(wts$Leaf*(((Pred$Leaf-TrueValues$Leaf)/scale.leaf)^2))/length(Pred$Leaf)
      err.rep <- sum(wts$Rep*(((Pred$Pod-TrueValues$Rep)/scale.rep)^2))/length(Pred$Pod)
      # err.kRoot <- sum(((pcoef$kRoot.pred-pcoef$kRoot)/scale.kRoot)^2)/length(pcoef$kRoot)
      err.Root <- sum(((Pred.Root-RootVals[[i]]$Root)/scale.Root)^2)/length(Pred.Root)
      
      cost <- wts2$Stem*err.stem + wts2$Rep*err.rep + wts2$Leaf*err.leaf + wts2$Root*err.Root
      
      # err.stem <- sum(((Pred$Stem-TrueValues$Stem)/scale.stem)^2)/length(Pred$Stem)
      # err.leaf <- sum(((Pred$Leaf-TrueValues$Leaf)/scale.leaf)^2)/length(Pred$Leaf)
      # err.rep <- sum(((Pred$Pod-TrueValues$Rep)/scale.rep)^2)/length(Pred$Pod)
      # # err.kRoot <- sum(((pcoef$kRoot.pred-pcoef$kRoot)/scale.kRoot)^2)/length(pcoef$kRoot)
      # 
      # cost <- weights$Stem*err.stem+weights$Rep*err.rep+weights$Leaf*err.leaf+weights$kRoot*err.kRoot
      # 
      cost <- round(100 * cost,2) / length(biocro.fun)
      
      cost.avg <- cost.avg + cost
      
    }
    
  }
  
  write.table(t(c(cost.avg, optim_params)), file = "~/Research/BioCro/SoybeanBioCro/Parameter_Optimization/multiyear_optimization_output.csv",append = TRUE, sep = ',', col.names = FALSE, row.names = FALSE)
  
  return(cost.avg)
  
}



