# Weather data

These are the weather files used to reproduce the results in Matthews et al. Soybean-BioCro: A semi-mechanistic model of soybean growth.

They contain hourly PAR (`solar`), temperatue (`temp`), relative humidity (`rh`), and wind speed (`windspeed`) from the SURFRAD Bondville, IL site. The precipitation (`precip`) is calculated from the WARM Champaign, IL data set, and the day length (`day_length`) is calculated using the oscillator clock module set in BioCro.

The solar zenith angle (`zen`), net solar (`netsolar`), downwelling solar (`dw_solar`), upwelling solar (`up_solar`) parameters are included in these files, but are not used in this version of Soybean-BioCro.

See the `../WeatherDataProcessing` directory for more information about how these files were produced, and to reproduce for scripts to reproduce these files.
