
check.bounds <- function(inds.prev, inds.next, max.ind) {
  
  oob.prev <- which(inds.prev < 1)
  oob.next <- which(inds.next > max.ind)
  
  inds.prev[oob.prev] <- inds.next[oob.prev]
  inds.next[oob.next] <- inds.prev[oob.next]
  
  inds <- list('Prev' = inds.prev, 'Next' = inds.next)

  return(inds)
  
}

impute_data_avg <- function(data, inds, consec.thresh) {
  
  consec.inds.jump <- which(diff(inds)!=1)
  consec.inds.start <- c(1, consec.inds.jump+1)
  consec.inds.end <- c(consec.inds.jump, length(inds))
  
  # Check for length of consecutive missing values. If greater than some number (e.g., 12?) then do the solar averaging steps)
  consec.num <- 1 + consec.inds.end - consec.inds.start
  
  inds.dayavg <- which(consec.num > consec.thresh)
  inds.avg <- setdiff(1:length(consec.num), inds.dayavg)
  
  consec.inds.start.avg <- consec.inds.start[inds.avg]
  consec.inds.end.avg <- consec.inds.end[inds.avg]
  
  consec.inds.start.dayavg <- consec.inds.start[inds.dayavg]
  consec.inds.end.dayavg <- consec.inds.end[inds.dayavg]
  
  data.imputed <- data
  
  ### avg
  if (length(inds.avg) > 0) {
    
    inds.avg.prev <- inds[consec.inds.start.avg] - 1
    inds.avg.next <- inds[consec.inds.end.avg] + 1
    
  # fix any inds that are out of bounds
    oob <- check.bounds(inds.avg.prev, inds.avg.next, length(data))
    inds.avg.prev <- oob$Prev
    inds.avg.next <- oob$Next
  
  val.prev <- data[inds.avg.prev]
  val.next <- data[inds.avg.next]
  
  val.avg <- colMeans(rbind(val.prev,val.next))
  
  for (j in 1:length(consec.inds.start.avg)) {
    
    consec.range <- inds[consec.inds.start.avg[j]]:inds[consec.inds.end.avg[j]]
    
    data.imputed[consec.range] <- val.avg[j]
    
  }
  }
  ### dayavg
  if (length(inds.dayavg) > 0) {
  inds.dayavg.j <- vector()
  for (j in 1:length(consec.inds.start.dayavg)) {
    inds.dayavg.j <- c(inds.dayavg.j, inds[consec.inds.start.dayavg[j]:consec.inds.end.dayavg[j]])
  
  }
  
  data.imputed <- impute_data_dayavg(data.imputed, inds.dayavg.j)
  }
  
  return(data.imputed)
  
}

impute_data_dayavg <- function(data, inds) {
  
  day.inds.prev <- inds - 24
  day.inds.next <- inds + 24
  
  # Add a thing to check bounds (if day.inds.prev < 1 or day.inds.next > some #)
  
  oob <- check.bounds(day.inds.prev, day.inds.next, length(data))
  day.inds.prev <- oob$Prev
  day.inds.next <- oob$Next
  
  #
  
  impute.values <- colMeans(rbind(data[day.inds.prev], data[day.inds.next]), na.rm = FALSE)
  
  if (any(is.na(impute.values))) {
    
    na.inds <- which(is.na(impute.values))
    
    for (i in na.inds) {
      j.prev <- 1
      j.next <- 1
      while (is.na(data[day.inds.prev[i]])) {
        j.prev <- j.prev + 1
        day.inds.prev[i] <- inds[i] - j.prev * 24
        
      }
      
      while (is.na(data[day.inds.next[i]])) {
        j.next <- j.next + 1
        day.inds.next[i] <- inds[i] + j.next * 24
      }
      
      oob <- check.bounds(day.inds.prev[i], day.inds.next[i], length(data))
      day.inds.prev[i] <- oob$Prev
      day.inds.next[i] <- oob$Next
      
      impute.values[i] <- (j.next * data[day.inds.prev[i]] + j.prev * data[day.inds.next[i]]) / (j.prev + j.next)
      
    }
    
  }
  
  data.imputed <- data
  data.imputed[inds] <- impute.values

  return(data.imputed)
}

imputed.msg <- function(inds, param, type, table, filename) {
  
  if (length(inds)>0) {
    
    msg.head <- paste0(param, ' was estimated from ', type, ' for the following times:')
    msg.body <- paste0(table$year[inds], ' DOY ', table$doy[inds], ' hour ', table$hour[inds], ' (LST)')
    
    write(msg.head, file = filename, append = TRUE)
    write(msg.body, file = filename, append = TRUE)
    
  }
  
}

get.date.range <- function(date.range.input) {
  
  years <- seq(from = date.range.input$year[1], to = date.range.input$year[2], by = 1)
  
  leap.year <- !(years %% 4)
  length.year <- 365 + leap.year
  
  doys <- vector()
  for (i in 1:length(years)) {
    doys <- c(doys, seq(from = 1, to = length.year[i], by = 1))
  }
  
  dates <- data.frame("year" = rep(years,length.year), "doy" = doys)
  
  ind.start <- which(dates$doy == date.range.input$doy[1])[1]
  ind.end <- which(dates$doy == date.range.input$doy[2])[length(years)]
  
  dates <- dates[ind.start:ind.end,]
  
  if (date.range.input$doy[1] == 1) {
    
    prev.year <- date.range.input$year[1]-1
    prev.day <- data.frame("year" = date.range.input$year[1]-1, "doy" = 365+!(prev.year %% 4))
    
  } else {
    
    prev.day <- data.frame("year" = date.range.input$year[1], "doy" = date.range.input$doy[1]-1)
    
  }
  
  if (date.range.input$doy[2] == length.year[length(years)]) {
    
    next.day <- data.frame("year" = date.range.input$year[2]+1, "doy" = 1)
    
  } else {
    
    next.day <- data.frame("year" = date.range.input$year[2], "doy" = date.range.input$doy[2] + 1)
    
  }
  
  dates <- rbind(prev.day , dates, next.day)
  
  return(dates)
  
}

surfrad.weather.processing <- function(date.range.input, site.name, site.shortname, time.change, output.filepath) {

# We refer to two readme files in the comments of this code: the 1) weather readme and 2) surfrad readme
# 1) The weather readme file is the README.md file in the same folder as this code. It can also be
# accessed at: https://github.com/leighmatth/SoybeanBioCro/blob/master/Weather_Data/README.md
# 2) The surfrad readme is the readme included with the surfrad data from their site. It can be accessed at: 
# ftp://aftp.cmdl.noaa.gov/data/radiation/surfrad/Bondville_IL/README


dates <- get.date.range(date.range.input)

filepath <- paste0('ftp://aftp.cmdl.noaa.gov/data/radiation/surfrad/',site.name,'/')

if (!dir.exists(output.filepath)){
  dir.create(output.filepath)
}


if (file.exists(paste0(output.filepath, '/data_imputation.txt'))) {
  if (file.exists(paste0(output.filepath, '/oldversions_data_imputation.txt'))) {
    file.append(paste0(output.filepath, '/data_imputation.txt'), paste0(output.filepath, '/oldversions_data_imputation.txt'))
    file.remove(paste0(output.filepath, '/data_imputation.txt'))
  }
  else {
    file.rename(from = paste0(output.filepath, '/data_imputation.txt'), to = paste0(output.filepath, '/oldversions_data_imputation.txt'))
  }
}


# surfrad variable names (see surfrad readme ftp://aftp.cmdl.noaa.gov/data/radiation/surfrad/Bondville_IL/README)
varnames.surfrad<-c('year','doy','month','day','hour','min','dt','zen','dw_solar','qc_dw','up_solar','qc_up'
            ,'direct_n','qc_dn','diffuse','qc_dif','dw_ir','qc_dwir','dw_casetemp','qc_dwct','dw_dometemp','qc_dwdt'
            ,'uw_ir','qc_uwir','uw_casetemp','qc_uwct','uw_dometemp','qc_uwdt','uvb','qc_uvb',
            'par','qc_par','netsolar','qc_netsol','netir','qc_netir','totalnet','qc_totnet','temp','qc_temp',
            'rh','qc_rh','windspd','qc_ws','winddir','qc_wd','pressure','qc_pres')

varnames.time <- c('year','doy','hour','min','zen')

varnames.biocro <- c('par','netsolar','dw_solar','up_solar','temp',
                     'rh','windspd')
varnames.biocro.qc <- c('qc_par','qc_netsol','qc_dw','qc_up', 'qc_temp',
                        'qc_rh','qc_ws')

table.year<-data.frame() # Initialize table.year  
missing.doy<-NULL
bad.value <- NA
for (i in 1:nrow(dates)) {
  year <- dates$year[i]
  doy <- dates$doy[i]
  # Read file i and store as a table. First two rows of each file contain description that we do not want to load, so skip those lines
  # if (tryCatch(getURL(url = paste0(filepath,year,'/', site.shortname, substr(year,3,4),sprintf("%03d",doy),'.dat')), error = function(e) FALSE) != FALSE) {
  
  table.day<-tryCatch(read.table(file=paste0(filepath,year,'/', site.shortname, substr(year,3,4),sprintf("%03d",doy),'.dat'),skip=2,header = FALSE), error = function(e) FALSE)
  if (is.logical(table.day)) {
    
    table.day <- table.day <- data.frame(matrix(data=1e10,nrow=1, ncol=length(varnames.surfrad)))
    colnames(table.day) <- varnames.surfrad
    table.day$year <- year
    table.day$doy <- doy
    table.day$hour <- 0
    table.day$min <- 0
    
  } else {
  colnames(table.day) <- varnames.surfrad
  }
  table.day.biocro <- table.day[,varnames.biocro]
  table.day.biocro.qc <- table.day[,varnames.biocro.qc]
  
  # Check for any quality chack fails (qc_flags > 0) and replace their value with NA
  qc.fail.inds <- which(table.day.biocro.qc > 0, arr.ind = TRUE)
  table.day.biocro[qc.fail.inds] <- bad.value
  
  # Combine timepoints and variables in one table
  table.day.biocro <- cbind(table.day[,varnames.time],table.day.biocro)
  
  # Check for missing time points
  if (year < 2009) {
    missing.timepoints <- nrow(table.day)!=480 #480 time points, every 3 minutes for 24 hours
  } else {
    missing.timepoints <- nrow(table.day)!=1440 #1440 time points, every 1 minute for 24 hours after Jan 1, 2009
  }
  
  if (missing.timepoints){
    missing.doy<-c(missing.doy,paste(year, doy, sep = '.'))
  }
  
  # Change relative humidity from percent to fraction
  table.day.biocro$rh <- 0.01 * table.day.biocro$rh
  
  table.year <- rbind(table.year,table.day.biocro) # concatanate the data from all of the days into one table for the year 
}

# At night some of the solar values are negative due to thermalpile cooling (see weather readme for more information)
# Set these negative values to 0
solar <- c('par','netsolar','dw_solar','up_solar') # netsolar should not have any negative values (see surfrad readme) but is included for completeness
table.year.solar <- table.year[, solar]
table.year.solar[which(table.year.solar < 0, arr.ind = TRUE)] <- 0
table.year[, solar] <- table.year.solar


# Deal with missing time points and "bad" values
# First, take averages over the hours, ignoring the NAs (bad.values)
doy.hour <- paste(table.year$year, table.year$doy, table.year$hour, sep = '.')
hour.inds <- match(unique(doy.hour), doy.hour)
hour.inds <- c(hour.inds, nrow(table.year) + 1)
table.year.hour <- NULL
for (j in 1:(length(hour.inds) - 1)) {
  
  table.year.hour <- rbind(table.year.hour, colMeans(table.year[hour.inds[j]:(hour.inds[j + 1] - 1),], na.rm = TRUE))

}

table.year.hour <- as.data.frame(table.year.hour)

### Insert NAs for missing timepoints (DOYs)
# Remove missing days from list, if at least one measurement was taken every hour
year.doy <- paste(table.year.hour$year, table.year.hour$doy, sep = '.')
missing.doy.numhours <- NULL
for (i in 1:length(missing.doy)) {
  
  # doy.inds <- which(table.year.hour$doy == doy)
  doy.inds <- which(year.doy == missing.doy[i])
  
  missing.doy.numhours[i] <- length(doy.inds)
}

missing.doy <- missing.doy[!(missing.doy.numhours == 24)] # the days that do not have a measurement for all 24 hours

# For remaining doys with missing hours
old.table.year.hour <- table.year.hour # store this for now just in case
for (doy in missing.doy) {
  year.doy <- paste(table.year.hour$year, table.year.hour$doy, sep = '.')
  
  doy.inds <- which(year.doy == doy)
  
  # Works for missing partial or full day
  missing.hours <- setdiff(0:23, table.year.hour$hour[doy.inds])
  missing.hours.inds <- missing.hours + 1
  
  # Create table for day with missing timepoints
  new.missing.doy <- data.frame(matrix(data=NA,nrow=24, ncol=ncol(table.year.hour)))
  colnames(new.missing.doy) <- colnames(table.year.hour)
  new.missing.doy$year <- as.numeric(unlist(strsplit(doy, split='[.]')))[1]
  new.missing.doy$doy <- as.numeric(unlist(strsplit(doy, split='[.]')))[2]
  new.missing.doy$hour <- 0:23
  
  # Put known measurements into the correct rows of the new day table
  new.missing.doy[setdiff(1:24, missing.hours.inds),] <- table.year.hour[doy.inds,]
  
  # Add day to table.year.hour in correct location
  table.year.hour<- rbind(table.year.hour[1:(doy.inds[1] - 1),], new.missing.doy,table.year.hour[(doy.inds[length(doy.inds)] + 1):nrow(table.year.hour),])
  
}

table.year.hour.raw <- table.year.hour # Data before any data imputation for missing/bad values

# Check PAR and other solar measurements for NaNs (aka "bad" values)
badvals.par <- which(is.na(table.year.hour$par))
badvals.up <- which(is.na(table.year.hour$up_solar))
badvals.net <- which(is.na(table.year.hour$netsolar))
badvals.dw <- which(is.na(table.year.hour$dw_solar))

### Estimate PAR from other solar measurements
par_perc_dw <- 0.43 # Estimate PAR to be 43% of Downwelling solar. See weather readme for citation.

# netsolar = dw_solar - up_solar => dw_solar = netsolar + up_solar
# Using dw_solar = netsolar + up_solar is more accurate than the measured dw_solar value. See weather readme for more information.
# Find indices where we have a bad par value but good up_solar and netsolar values
inds.estpar.upnet <- setdiff(badvals.par,union(badvals.up,badvals.net)) # badvals.par not in either badvals.up or badvals.net

table.year.hour$par[inds.estpar.upnet] <- par_perc_dw * (table.year.hour$netsolar[inds.estpar.upnet] + table.year.hour$up_solar[inds.estpar.upnet])

# For any remaining indices where we have a bad par value but a good dw_solar value
inds.estpar.dw <- setdiff(setdiff(badvals.par,inds.estpar.upnet), badvals.dw) # badvals.par not in badvals.dw
table.year.hour$par[inds.estpar.dw] <- par_perc_dw * table.year.hour$dw_solar[inds.estpar.dw] # estimate par as 43% of dw_solar. See weather readme.

# Remaining PAR inds of badvals
inds.estpar <- setdiff(badvals.par,union(inds.estpar.upnet, inds.estpar.dw))

# badvals for the remaining variables
badvals.temp <- which(is.na(table.year.hour$temp))
badvals.rh <- which(is.na(table.year.hour$rh))
badvals.windspd <- which(is.na(table.year.hour$windspd))

### average over previous and next day at same timepoint
# PAR
table.year.hour$par <- impute_data_dayavg(table.year.hour$par, inds.estpar)

# Downwelling solar
table.year.hour$dw_solar <- impute_data_dayavg(table.year.hour$dw_solar, badvals.dw)

# Upwelling solar
table.year.hour$up_solar <- impute_data_dayavg(table.year.hour$up_solar, badvals.up)

# Netsolar
table.year.hour$netsolar <- impute_data_dayavg(table.year.hour$netsolar, badvals.net)

# average over previous and next timepoints
consec.thresh <- 9
# Temperature
table.year.hour$temp <- impute_data_avg(table.year.hour$temp, badvals.temp, consec.thresh)

# Relative Humidity
table.year.hour$rh <- impute_data_avg(table.year.hour$rh, badvals.rh, consec.thresh)

# Windspeed
table.year.hour$windspd <- impute_data_avg(table.year.hour$windspd, badvals.windspd, consec.thresh)


# Convert PAR from W/m2 to umol/m2s (see surfrad readme)
table.year.hour$par <- 4.6 * table.year.hour$par
table.year.hour.raw$par <- 4.6 * table.year.hour.raw$par


# Surfrad doys and hours are in UTC time. Convert to a given timezone (default central standard time)
timediff <- time.change # e.g., -6 for 6 hours behind for central standard time
table.year.localtime <- table.year.hour
utc <- table.year.hour$doy + table.year.hour$hour / 24
localtime <- utc + timediff / 24
localtime.doy <- floor(localtime)
localtime.hour <- round((localtime-localtime.doy) * 24, 0) # round to make it an integer
localtime.year <- table.year.localtime$year

# deal with potential changing of years
if (timediff < 0) {

  inds.d0 <- which(localtime.doy == 0)
  localtime.year[inds.d0] <- localtime.year[inds.d0] - 1

  localtime.doy[inds.d0] <- 365 + !(localtime.year[inds.d0] %% 4)
}

if (timediff > 0) {
  
  inds.d0 <- which(localtime.doy == 366 + !(localtime.year %% 4))
  localtime.year[inds.d0] <- localtime.year[inds.d0] + 1
  localtime.doy[inds.d0] <- 1
  
}


table.year.localtime$doy <- localtime.doy
table.year.localtime$hour <- localtime.hour
table.year.localtime$year <- localtime.year

table.year.localtime.raw <- table.year.hour.raw
table.year.localtime.raw$doy <- localtime.doy
table.year.localtime.raw$hour <- localtime.hour
table.year.localtime.raw$year <- localtime.year

### Output messages for imputed values
filename = paste0(output.filepath, '/data_imputation.txt')
imputed.msg(inds.estpar.upnet, 'PAR', 'the netsolar and upwelling solar', table.year.localtime, filename)
imputed.msg(inds.estpar.dw, 'PAR', 'the downwelling solar', table.year.localtime, filename)
imputed.msg(inds.estpar, 'PAR', 'the solar averaging function', table.year.localtime, filename)

imputed.msg(badvals.dw, 'Downwelling Solar', 'the day averaging function', table.year.localtime, filename)
imputed.msg(badvals.up, 'Upwelling Solar', 'the day averaging function', table.year.localtime, filename)
imputed.msg(badvals.net, 'Netsolar', 'the day averaging function', table.year.localtime, filename)

imputed.msg(badvals.rh, 'RH', 'the averaging function', table.year.localtime, filename)
imputed.msg(badvals.temp, 'Temperature', 'the averaging function', table.year.localtime, filename)
imputed.msg(badvals.windspd, 'Windspeed', 'the averaging function', table.year.localtime, filename)

year.doy.hour <- paste(table.year.localtime$year, table.year.localtime$doy, table.year.localtime$hour, sep = '.')
start.year.doy.hour <- paste(date.range.input$year[1], date.range.input$doy[1], date.range.input$hour[1], sep = '.')
end.year.doy.hour <- paste(date.range.input$year[2], date.range.input$doy[2], date.range.input$hour[2], sep = '.')

start.ind <- which(year.doy.hour == start.year.doy.hour)
end.ind <- which(year.doy.hour == end.year.doy.hour)

table.year.out <- table.year.localtime[start.ind:end.ind,]
table.year.out.raw <- table.year.localtime.raw[start.ind:end.ind,]

# Remove min column
table.year.out$min <- NULL
table.year.out.raw$min <- NULL

# Rename variable name to biocro names
# units: solar: umol/(m2s), netsolar, dw_solar, up_solar: Watts/m2, temp: degC, rh: fraction, windspeed: m/s 
colnames(table.year.out)[colnames(table.year.out)=='par'] <- 'solar'
colnames(table.year.out)[colnames(table.year.out)=='windspd'] <- 'windspeed'

colnames(table.year.out.raw)[colnames(table.year.out.raw)=='par'] <- 'solar'
colnames(table.year.out.raw)[colnames(table.year.out.raw)=='windspd'] <- 'windspeed'

output <- list("Weather" = table.year.out, "Weather_noimputation" = table.year.out.raw)

return(output)

}

