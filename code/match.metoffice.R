match.metoffice<-function(metoffice_df,google_df)
#where metoffice is the output of read.metofficecovid() i.e. a csv of met office data for UK
#where google is the output of read.googlemobility() i.e a csv of google mobility data for UK
    {
  
  # Load packages ----------------------------------------------------------
  #install.packages('plotrix')
  library(plotrix)
  #install.packages('tibble')
  library(tibble)
  #install.packages('ggplot2')
  library(ggplot2)
  #install.packages('gridExtra')
  library(gridExtra)
  #install.packages('grid)
  library(grid)
  #install.packages('ggplot2)
  library(ggplot2)
  #install.packages('lattice')
  library(lattice)
  #install.packages("tidyr")
  library(tidyr)
  #install.packages('reshape2')
  library(reshape2)
  #install.packages('dplyr')
  library(dplyr)
  #install.packages('plyr')
  library(plyr)
  #install.packages('XML')
  library(XML) # HTML processing
  #install.packages('RCurl')
  library(RCurl)
  #install.packages('rvest')
  library(rvest)
  #install.packages('stringr')
  library(stringr)
  #install.packages('plotrix)
  library(plotrix)
  
#load Met Office data for May
metoffice<-read.csv(metoffice_df)
#add empty country column to the met office data frame
metoffice<-metoffice %>% add_column(country='', .after=which(colnames(metoffice)=="name"))

#using vectors of the met office districts (COVID reporting regions) belonging to each country, populate the country column with the right country for each district
#England
metoffice$country[(metoffice$name%in%readRDS('input_data/metoffice_englanddistricts.RDS'))]<-"England"
#Wales
metoffice$country[(metoffice$name%in%readRDS('input_data/metoffice_walesdistricts.RDS'))]<-"Wales"
#Scotland
metoffice$country[(metoffice$name%in%readRDS('input_data/metoffice_Scotlanddistricts.RDS'))]<-"Scotland"
#Northern Ireland
metoffice$country[(metoffice$name%in%readRDS('input_data/metoffice_nirelanddistricts.RDS'))]<-"Northern Ireland"

#subset out only England data from the Met Office dataframe
metoffice_england<-subset(metoffice,country=='England')

#read in google data
google<-read.csv(google_df)

# Match the Met Office COVID Reporting regions to Google regions (this is easy for England because all google districts are coarser resolution) ---------------------------------------------------------------

#add empty google districts column to the met office data frame
metoffice_england<-metoffice_england %>% add_column(google='', .before=which(colnames(metoffice)=="name"))
#load a dataframe showing which google districts each of the met office districts are found in
metoffice_to_google<-readRDS('input_data/metoffice_englanddistricts_googleequiv.RDS')
#populate the google column with the right google district for each met office district
metoffice_england$google<-metoffice_to_google$google[match(metoffice_england$name,metoffice_to_google$name)]

# Aggregate the met office data by each google district (get mean for each of the meterological measures)  --------------------------------------------------------------

metoffice_england<-aggregate(metoffice_england[,7:ncol(metoffice_england)],
                list(metoffice_england$date,
                     metoffice_england$google),mean)

#rename the first two columns
colnames(metoffice_england)[1:2]<-c('date','sub_region_1')

#create vector of days of the week and add to dataframe
metoffice_england<-metoffice_england %>% tibble::add_column(weekdays = weekdays(as.Date(metoffice_england$date)), 
                                                            .before = 1)
metoffice_england<-metoffice_england%>%
  mutate(
    precipitation_flux_max_mean..kg.m.2.s.1.= precipitation_flux_max_mean..kg.m.2.s.1.*3600,
    precipitation_flux_mean_mean..kg.m.2.s.1.= precipitation_flux_mean_mean..kg.m.2.s.1.*3600,
    precipitation_flux_max_variance..m.4.kg2.s.2. = precipitation_flux_max_variance..m.4.kg2.s.2.*3600,
    precipitation_flux_mean_variance..m.4.kg2.s.2. = precipitation_flux_mean_variance..m.4.kg2.s.2.*3600,
    air_temperature_max_mean..K. = air_temperature_max_mean..K. - 273.15,
    air_temperature_mean_mean..K. = air_temperature_mean_mean..K. - 273.15,
    air_temperature_min_mean..K. = air_temperature_min_mean..K. - 273.15,
    air_temperature_max_variance..K2. = air_temperature_max_variance..K2.,
    air_temperature_mean_variance..K2. = air_temperature_mean_variance..K2.,
    air_temperature_min_variance..K2. = air_temperature_min_variance..K2.)

colnames(metoffice_england)<-c('weekdays',
                                 'date',
                                 'sub_region_1',
                                 'temp_max', # temp units = celsius
                                 'temp_max_variance',
                                 'temp_mean',
                                 'temp_mean_variance',
                                 'temp_min',
                                 'temp_min_variance',
                                 'rain_max',# rain units = mm/hr
                                 'rain_max_variance',
                                 'rain_mean',
                                 'rain_mean_variance',
                                 'short_wave_radiation_max', # short wave radiation units = W/m^2 and /m^4 for variance.
                                 'short_wave_radiation_max_variance',
                                 'short_wave_radiation_mean',
                                 'short_wave_radiation_mean_variance',
                                 'specific_humidity_max', # specific humidity = kg of water vapour in kg of air.
                                 'specific_humidity_max_variance',
                                 'specific_humidity_mean',
                                 'specific_humidity_mean_variance',
                                 'specific_humidity_min',
                                 'specific_humidity_min_variance',
                                 'wind_spd_max', # Wind speed/ of gust unit = m/s
                                 'wind_spd_max_variance',
                                 'wind_spd_mean',
                                 'wind_spd_mean_variance',
                                 'wind_spd_min',
                                 'wind_spd_min_variance',
                                 'wind_spd_of_gust_max',
                                 'wind_spd_of_gust_max_variance',
                                 'wind_spd_of_gust_mean',
                                 'wind_spd_of_gust_mean_variance',
                                 'wind_spd_of_gust_min',
                                 'wind_spd_of_gust_min_variance',
                                 'sea_level_pressure_max',# pressure(mean) = PA
                                 'sea_level_pressure_max_variance', # pressure (variance) = m..2.kg.2.s.4
                                 'sea_level_pressure_mean',
                                 'sea_level_pressure_mean_variance',
                                 'sea_level_pressure_min',
                                 'sea_level_pressure_min_variance',
                                 'cloud_area_fraction_assuming_maximum_random_overlap_max',
                                 'cloud_area_fraction_assuming_maximum_random_overlap_max_variance',
                                 'cloud_area_fraction_assuming_maximum_random_overlap_mean',
                                 'cloud_area_fraction_assuming_maximum_random_overlap_mean_variance',
                                 'cloud_area_fraction_assuming_maximum_random_overlap_min',
                                 'cloud_area_fraction_assuming_maximum_random_overlap_min_variance',
                                 'cloud_base_altitude_assuming_only_consider_cloud_area_greater_than2p5_oktas_max', #kft
                                 'cloud_base_altitude_assuming_only_consider_cloud_area_greater_than2p5_oktas_mean',
                                 'cloud_base_altitude_assuming_only_consider_cloud_area_greater_than2p5_oktas_min')
                         
metoffice_england
}

#example implementation
#metoffice_england<-match.metoffice('met.csv', 'goog_england.csv')
#write.csv(metoffice_england, 'metoffice_england.csv')