# START -------------------------------------------------------------------
rm(list=ls())
set.seed(1234)
setwd('~/Github/parksinthepandemic/code')

# EXAMPLE PIPELINE FOR FUNCTIONS ------------------------------------------

source('read.googlemobility.R')
source('read.metofficecovid.R')
source('read.naturalenglandmene.R')
source('getandmatch.mene.R')

source('match.metoffice.R')
source('getandmatch.forecast.R')
source('match.mene2google.R')
source('match.OSgreenspace2google.R')

source('relative2baseline.R')

# READ IN DATASETS --------------------------------------------------------

#google community mobility reports
goog<-read.googlemobility()
google_england<-subset(goog,sub_country=='England')
write.csv(google_england, 'input_data/testdata/google_england.csv', row.names = F)

#met office covid data
met<-read.metofficecovid()
write.csv(met, 'input_data/testdata/met.csv', row.names = F)

#natural england MENE data
mene<-getandmatch.mene()
write.csv(mene,'input_data/testdata/mene.csv', row.names=F)

# MATCH DATASETS ----------------------------------------------------------

#match met office with google data and rename columns to fit OpenWeather forecast format
metoffice_england<-match.metoffice('input_data/testdata/met.csv', 'input_data/testdata/google_england.csv')
write.csv(metoffice_england, 'input_data/testdata/metoffice_england.csv', row.names=F)

#get forecast for a location and match with metoffice format
forecast_bedford<-getandmatch.forecast('Bedford',apikey="~/GitHub/apikey.RDS")
write.csv(forecast_bedford,'input_data/testdata/forecast_bedford.csv', row.names=F)

#match natural england mene data to google districts
mene<-read.menedata(relative2greenspace = F,relative2population = F)
mene_population<-read.menedata(relative2population = T,relative2greenspace = F)
mene_greenspace<-read.menedata(relative2greenspace = T,relative2population = F)
mene_both<-read.menedata(relative2greenspace = T,relative2population = T)

mene_england<-cbind.data.frame("sub_region_1" = mene$google_district,
                    "estimated_average_annual_visit_count" = mene$estimated_average_annual_visit_count,
                    "annual_visits_per_capita" = mene_population$annual_visits_per_capita,
                    "annual_visits_per_km2_greenspace_1km_radius" = mene_greenspace$annual_visits_per_km2_greenspace_1km_radius,
                    "annual_visits_per_capita_per_km2_greenspace_1km_radius" = mene_both$annual_visits_per_capita_per_km2_greenspace_1km_radius)

write.csv(mene_england,'input_data/testdata/mene_england.csv', row.names=F)

#Create garden_acess data frame and saves to testdata.
garden_access<-match.OSgreenspace2google()
write.csv(garden_access,'input_data/testdata/garden_access.csv', row.names=F)

# MAKE WEATHER DATA RELATIVE TO BASELINE AND MERGE WITH GOOGLE ----------------------------------

metoffice_england_r2b<-relative2baseline('input_data/testdata/metoffice_england.csv')
googleandmetoffice_england<-merge(google_england,metoffice_england_r2b, all=T)
write.csv(googleandmetoffice_england, 'input_data/testdata/googleandmetoffice_england.csv')

# TEST PLOT ---------------------------------------------------------------

#temp looks ok
plot(googleandmetoffice_england$parks_percent_change_from_baseline ~ googleandmetoffice_england$date, col='black')
points(googleandmetoffice_england$temp_max ~ googleandmetoffice_england$date, col='red')

#rain looks weird...bug? out by 3 orders of mag?
plot(googleandmetoffice_england$parks_percent_change_from_baseline ~ googleandmetoffice_england$date, col='black')
points(googleandmetoffice_england$rain_mean ~ googleandmetoffice_england$date, col='red')

