
# Set working directory to location of this file
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source('SURFRAD_weather_processing.R')
source('WARM_precipitation.R')

year <- 2002
date.range.input <- data.frame("year"=c(year,year),"doy"=c(001,365),"hour"=c(0,23),row.names=c("start","end"))

site.name <- 'Bondville_IL'
site.shortname <- 'bon' # examples include 'bon', 'fpk', 'gwn', 'tbl', 'dra', 'psu', 'sxf'
time.change <- -6 # for Central Standard Time

output.filepath <- paste0('./', year)
dir.create(path = output.filepath, showWarnings = FALSE)


output <- surfrad.weather.processing(date.range.input, site.name, site.shortname, time.change, output.filepath)

# Precipitation from WARM weather
filepath.warm <- './cmiday.txt'
weather.warm <- read.table(filepath.warm,sep='\t', header = TRUE, stringsAsFactors = FALSE)

weather.table <- WARM.precipitation(output$Weather, weather.warm, date.range.input)

write.csv(weather.table, file = paste0(output.filepath, '/', year, '_', site.name, '.csv'), row.names = FALSE)
