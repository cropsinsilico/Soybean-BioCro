# SoybeanBioCro - Weather data
The weather data used for this model comes from the <a href=https://www.esrl.noaa.gov/gmd/grad/surfrad/>SURFRAD (NOAA-ESRL surface radiation)</a> and <a href=https://www.isws.illinois.edu/warm/>WARM (Illinois Water and Atmospheric Resources Monitoring Program)</a> datasets.


The script `get_weather_files.R` collates the SURFRAD and WARM data for a given year into a format for BioCro. This script calls the following functions:
- `surfrad.weather.processing()` located in the `SURFRAD_weather_processing.R` file
- `WARM.precipitation()` located in the `WARM_precipitation.R` file
- `get_day_length()` located in the `day_length_script.R` file

`get_day_length()` calculates the day length using the oscillator clock function in BioCro (Lochocki and McGrath 2021, https://doi.org/10.1093/insilicoplants/diab016). Alternatively, day length can be calculated using celestial mechanic functions (Keisling 1982,  https://doi.org/10.2134/agronj1982.00021962007400040036x).

`get_day_length()` requires the BioCro library to be installed.


## SURFRAD Data
Site: Bondville, IL (40.0519 N, 88.3731 W; ~7 miles west of SoyFACE Farm)

Data: <a href="ftp://aftp.cmdl.noaa.gov/data/radiation/surfrad/Bondville_IL">ftp://aftp.cmdl.noaa.gov/data/radiation/surfrad/Bondville_IL/</a> 
<!-- Github doesn't render ftp links for some reason (as of Dec. 2019), but will leave these links here in case they fix this issue in the future. -->

Measurements were taken from 1995-Present (as of Nov. 24, 2019) and were averaged over 3 minute periods (Before Jan. 1, 2009) and 1 minute periods (after Jan. 1, 2009). 


Data is given in UTC time format (6 hours ahead of Central Standard Time - local Bondville time)


There are several measurements included in the SURFRAD dataset, we select 7 of these (PAR, downwelling solar, net solar, upwelling solar, temperature, relative humidity, and windspeed) to include in the output weather file for use in BioCro.

### Solar
#### Photosynthetically Active Radiation (PAR)
Unit: Wm<sup>-2</sup> we convert to &mu;mol m<sup>-2</sup>s<sup>-1</sup> using conversion factor (&mu;mol m<sup>-2</sup>s<sup>-1</sup> = 4.6 Wm<sup>-2</sup> ) specified in the SURFRAD Read Me <a href="ftp://aftp.cmdl.noaa.gov/data/radiation/surfrad/Bondville_IL/README">(ftp://aftp.cmdl.noaa.gov/data/radiation/surfrad/Bondville_IL/README)</a>


This is the solar value that is passed into BioCro.

#### Downwelling solar
Unit: Wm<sup>-2</sup>

Downwelling solar is not currently used in BioCro, but can be used to estimate PAR value when data points are missing (See <a href="#par">Bad Values - PAR</a>).

#### Upwelling solar
Unit: Wm<sup>-2</sup>

Upwelling solar is not currently used in BioCro, but can be used to estimate PAR value when data points are missing (See <a href="#par">Bad Values - PAR</a>).

#### Net Solar (downwelling solar - upwelling solar)
Unit: Wm<sup>-2</sup>

Net solar is not currently used in BioCro, but is used in other models such as OpenSimRoot (OSR). It is included in the output weather file for instances when BioCro and OSR are combined (crops in-silico project - in progress).

It can also be used to estimate PAR value when data points are missing (See <a href="#par">Bad Values - PAR</a>).

### Temperature
Unit: Degrees Celsius

### Relative Humidity
Unit: Percentage we convert to fraction (e.g., 98% to 0.98)

### Windspeed
Unit: ms<sup>-1</sup>

## WARM Data
Site: Champaign, IL (40.0839 N, 88.2403 W; ~3 miles north of SoyFACE Farm)


Data: <a href=https://www.isws.illinois.edu/warm/cdflist.asp>https://www.isws.illinois.edu/warm/cdflist.asp</a>; Select Champaign, gives a text file for all of the WARM measurments from ~1989-2018 (as of Nov. 26, 2019). We also include this text file (cmiday.txt) with the R Code for weather processing.


Unlike the SURFRAD data set, the WARM data set only provides daily values. From this data set, we use only the daily precipitation.

### Precipitation
Unit: inches; we convert to mm

Each day's rainfall is evenly divided into 24 timepoints.

## Missing Data and Bad Measurements
### Bad Measurements
The SURFRAD data comes with qualtiy check (qc) flags to indicate whether there are any potential issues with any of the reported measurements. A good value will have a qc flag of 0, while any qc flag > 0 indicates some type of problem with the measurement (see the SURFRAD Read Me <a href="ftp://aftp.cmdl.noaa.gov/data/radiation/surfrad/Bondville_IL/README">(ftp://aftp.cmdl.noaa.gov/data/radiation/surfrad/Bondville_IL/README)</a> for more information). In weather_processing.R we check for any values the a qc_flag > 0 and replace any associated reported measurement with NaN.


The NaNs are ignored when averaging each measurement over an hour time period. So if a given hour has at least one measured value that passes the quality check, then nothing further needs to be done. If all measurements during the given hour fail the quality check then we take the steps detailed in the following subsections to estimate a value. 

#### PAR
For datapoints where the PAR quality check (qc) flag is greater than zero we try to approximate PAR as 43% of the downwelling solar value (<a href=https://doi.org/10.1007/978-1-4419-0851-3_451>Mottus et al., 2012 Photosynthetically Active Radiation: Measurement and Modeling</a>; "The global PAR irradiance can be relatively reliably predicted from global shortwave irradiance, I<sub>PAR</sub> = 0.43 I<sub>SW</sub>."). 

As detailed in the SURFRAD Read Me <a href="ftp://aftp.cmdl.noaa.gov/data/radiation/surfrad/Bondville_IL/README">(ftp://aftp.cmdl.noaa.gov/data/radiation/surfrad/Bondville_IL/README)</a>, there is an issue with thermopile cooling issues that cause some of the solar measurements to be negative at night. We correct this by setting all negative values to 0. See the SURFRAD Read Me for more information on the thermopile cooling issues. 

Due to the thermalpile cooling issues, SURFRAD state that calculating the downwelling solar from the direct-normal and diffuse radiation measurements is a better representation of the downwelling solar than what is reported in the downwelling solar column. When calculating the Net Solar (= Downwelling Solar - Upwelling Solar) they used this approach for the downwelling component when possible. Therefore, when estimating a missing PAR value, we use Downwelling Solar = Net Solar + Upwelling Solar as the downwelling amount when possible.

If the Net Solar and Upwelling Solar or Downwelling Solar are not available, then PAR is estimated the same as the <a href="#downwelling-solar-upwelling-solar-net-solar">other missing solar values</a> .

#### Downwelling Solar, Upwelling Solar, Net Solar
Take average of the same hour from the previous known day and the next known day. 

Need to output if an excess of days are missing.

#### Temperature, Relative Humidity, Windspeed
If within a day, take average of previous known timepoint and next known timepoint.
