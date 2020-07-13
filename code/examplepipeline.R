# START -------------------------------------------------------------------
rm(list=ls())
set.seed(1234)
setwd('Github/parksinthepandemic/code/')

# EXAMPLE PIPELINE FOR FUNCTIONS ------------------------------------------

source('read.googlemobility.R')
source('read.metofficecovid.R')
source('read.naturalenglandmene.R')

source('match.metoffice.R')
source('getandmatch.forecast.R')
source('match.mene2google.R')

source('relative2baseline.R')

# READ IN DATASETS --------------------------------------------------------

#google community mobility reports
goog<-read.googlemobility()
google_england<-subset(goog,sub_country=='England')
write.csv(google_england, 'google_england.csv', row.names = F)

#met office covid data
met<-read.metofficecovid()
write.csv(met, 'met.csv', row.names = F)

#natural england MENE data
mene<-read.naturalenglandmene()
write.csv(mene,'mene.csv', row.names=F)

# MATCH DATASETS ----------------------------------------------------------

#match met office with google data and rename columns to fit OpenWeather forecast format
metoffice_england<-match.metoffice(metoffice_df = 'met.csv', google_df = 'google_england.csv')
write.csv(metoffice_england, 'metoffice_england.csv', row.names=F)

#get forecast for a location and match with metoffice format
forecast_bedford<-getandmatch.forecast('Bedford',apikey="./../../APIkey.RDS")
write.csv(forecast_bedford,'forecast_bedford.csv', row.names=F)

#match natural england mene data to google districts
mene_england<-match.mene2google('mene.csv')
write.csv(mene_england,'mene_england.csv', row.names=F)

# MAKE WEATHER DATA RELATIVE TO BASELINE AND MERGE WITH GOOGLE ----------------------------------

metoffice_england_r2b<-relative2baseline('metoffice_england.csv')
googleandmetoffice_england<-merge(metoffice_england_r2b,google_england, all=T)
write.csv(googleandmetoffice_england, 'googleandmetoffice_england.csv')

# TEST PLOT ---------------------------------------------------------------

#temp looks ok
plot(googleandmetoffice_england$parks_percent_change_from_baseline ~ googleandmetoffice_england$date, col='black')
points(googleandmetoffice_england$temp_max ~ googleandmetoffice_england$date, col='red')

#rain looks weird...bug? out by 3 orders of mag?
plot(googleandmetoffice_england$parks_percent_change_from_baseline ~ googleandmetoffice_england$date, col='black')
points(googleandmetoffice_england$rain_mean ~ googleandmetoffice_england$date, col='red')

