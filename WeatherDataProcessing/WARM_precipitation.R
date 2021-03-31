
WARM.precipitation <- function(table, weather.warm, time.range) {

  range.begin <- which(weather.warm$year==time.range$year[1])[time.range$doy[1]]
  range.end <- which(weather.warm$year==time.range$year[2])[time.range$doy[2]]
  
  weather.warm.subset <- weather.warm[range.begin:range.end,]
  
  inds.erflag.M <- which(weather.warm.subset$pcer == "M")
  weather.warm.subset$precip[inds.erflag.M] <- 0
  
  weather.warm.subset.hour <- data.frame("year"=rep(weather.warm.subset$year,each=24))
  weather.warm.subset.hour$month <- rep(weather.warm.subset$month,each=24)
  weather.warm.subset.hour$day <- rep(weather.warm.subset$day,each=24)
  weather.warm.subset.hour$hour <- rep(0:23,length(weather.warm.subset$year))
  weather.warm.subset.hour$precip <- rep(25.4*as.numeric(weather.warm.subset$precip)/24,each=24) # convert to mm and divide evenly across the day
  
  range.hour.begin <- which(weather.warm.subset.hour$hour==time.range$hour[1])[1]
  range.hour.end <- which(weather.warm.subset.hour$hour==time.range$hour[2])[length(which(weather.warm.subset.hour$hour==time.range$hour[2]))]
  
  table$precip <- weather.warm.subset.hour$precip[range.hour.begin:range.hour.end]
  
  return(table)
  
}