# Start -------------------------------------------------------------------
rm(list=ls())
set.seed(1234)
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
getwd()

# LOAD PACKAGES -----------------------------------------------------------

#install.packages('XML')
library(XML) # HTML processing
#install.packages('RCurl')
library(RCurl)
#install.packages('rvest')
library(rvest)
#install.packages("httr")
library(httr)
#install.packages("readr")
library(readr)
#install.packages("stringr")
library(stringr)
#install.packages("data.table")
library(data.table)
#install.packages
library(dplyr)
#install.packages('tibble')
library(tibble)
#install.packages("tidyr")
library(tidyr)
#install.packages("ggplot2")
library(ggplot2)
library(plyr)

source('read.metofficedata.R')
source('read.mobilityreports.R')

# start -------------------------------------------------------------------

#load Met Office data for May
metoffice<-read.metofficedata()
metoffice<-metoffice %>% add_column(country='', .after=which(colnames(metoffice)=="name"))

#England
metoffice$country[(metoffice$name%in%readRDS('input_data/metoffice_englanddistricts.RDS'))]<-"England"
#Wales
metoffice$country[(metoffice$name%in%readRDS('input_data/metoffice_walesdistricts.RDS'))]<-"Wales"
#Scotland
metoffice$country[(metoffice$name%in%readRDS('input_data/metoffice_Scotlanddistricts.RDS'))]<-"Scotland"
#Northern Ireland
metoffice$country[(metoffice$name%in%readRDS('input_data/metoffice_nirelanddistricts.RDS'))]<-"Northern Ireland"

#subset out only England Met data
metoffice_england<-subset(metoffice,country=='England')

#read in google data
google<-read.mobilityreports()
google_england<-subset(google,sub_country=='England')

#make a vector of dates within Google's 5 week 'Baseline' period (Jan 3-6th Feb)
baselinerange<-seq(as.Date('2020-01-03'),as.Date('2020-02-06'),1)
#subset out the met office data from the baseline period
metoffice_england_baseline<-metoffice[as.Date(metoffice$date)%in%baselinerange,]

# Match the Met Office COVID Reporting regions to Google regions ---------------------------------------------------------------

#add google districts column
metoffice_england<-metoffice_england %>% add_column(google='', .before=which(colnames(metoffice)=="name"))
metoffice_to_google<-readRDS('input_data/metoffice_englanddistricts_googleequiv.RDS')
metoffice_england$google<-metoffice_to_google$google[match(metoffice_england$name,metoffice_to_google$name)]

# Aggregate england (simple because all google districts are coarser resolution) --------------------------------------------------------------

levels(as.factor(metoffice_england$google))
levels(as.factor(google_england$sub_region_1))

metoffice_england<-aggregate(metoffice_england[,7:ncol(metoffice_england)],
                list(metoffice_england$date,
                     metoffice_england$google),mean)

colnames(metoffice_england)[1:2]<-c('date','sub_region_1')

# Match google and met office data ----------------------------------------

plot(google_england$parks_percent_change_from_baseline~as.Date(google_england$date))
plot(metoffice_england$short_wave_radiation_max_mean..W.m.2.
     ~as.Date(metoffice_england$date))

count(metoffice_england$sub_region_1)

range(as.Date(metoffice_england$date))
range(as.Date(google_england$date))

merged<-merge(google_england, metoffice_england, all.x=T)
metoffice_england<-merged[,1:52]

plot(merged$parks_percent_change_from_baseline~as.Date(merged$date))
plot(merged$short_wave_radiation_max_mean..W.m.2.
     ~as.Date(merged$date))

rm(metoffice,metoffice_to_google,merged,test,google)
# MESSY CODE FOR CALCULATING BASELINE -------------------------------------


count(metoffice_england$date==google_england$date)


googledata_may<-googledata[month(googledata$date)==5,]
googledata_may_york<-googledata_may[googledata_may$sub_region_1=='York',]
metdata_may_york<-metdata_may[metdata_may$name=='York',]
maydates<-metdata_may_york$date
maydays<-weekdays(as.Date(maydates))

metdata_may_york<-metdata_may_york[,5:ncol(metdata_may_york)]
metdata_baseline_york<-metdata_baseline[metdata_may$name=='York',]
metdata_baseline_york$weekday<-weekdays(as.Date(metdata_baseline_york$date))
metdata_baseline_york<-metdata_baseline_york[,5:ncol(metdata_baseline_york)]

baselineweather<-aggregate(metdata_baseline_york,list(metdata_baseline_york$weekday), median)

wday<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

baselineweather<-baselineweather[match(wday,baselineweather$Group.1),]
daysofweek<-metdata_baseline_york$weekday
baselineweather<-baselineweather[,-c(1,22)]
metdata_baseline_york<-metdata_baseline_york[,-21]

metdata_may_york_rel2baseline<-metdata_may_york

for (w in 1:7){
  for (c in 1:20){
  temp<-metdata_may_york[maydays%in%wday[w],c]-baselineweather[w,c]
  metdata_may_york_rel2baseline[maydays%in%wday[w],c]<-temp/baselineweather[w,c]*100
    }
}

matched<-match(googledata_may_york$date,maydates)
metdata_may_york_rel2baseline<-metdata_may_york[matched,]

plot(googledata_may_york$parks_percent_change_from_baseline~as.Date(googledata_may_york$date),ylim=c(-300,300), col='darkgreen')
lines(googledata_may_york$parks_percent_change_from_baseline[!is.na(googledata_may_york$parks_percent_change_from_baseline)]
      ~as.Date(googledata_may_york$date)[!is.na(googledata_may_york$parks_percent_change_from_baseline)], col='darkgreen')
points(metdata_may_york_rel2baseline$short_wave_radiation_mean_mean..W.m.2.~as.Date(googledata_may_york$date),col='red')
lines(metdata_may_york_rel2baseline$short_wave_radiation_mean_mean..W.m.2.~as.Date(googledata_may_york$date),col='red')
points(metdata_may_york_rel2baseline$precipitation_flux_mean_mean..kg.m.2.s.1.~as.Date(googledata_may_york$date),col='blue')
lines(metdata_may_york_rel2baseline$precipitation_flux_mean_mean..kg.m.2.s.1.~as.Date(googledata_may_york$date),col='blue')

library(rpart)
model<-rpart(googledata_may_york$parks_percent_change_from_baseline[!is.na(googledata_may_york$parks_percent_change_from_baseline)]
                ~as.Date(googledata_may_york$date)[!is.na(googledata_may_york$parks_percent_change_from_baseline)]
                +metdata_may_york_rel2baseline$short_wave_radiation_mean_mean..W.m.2.[!is.na(googledata_may_york$parks_percent_change_from_baseline)])

lines(predict(model)~as.Date(googledata_may_york$date)[!is.na(googledata_may_york$parks_percent_change_from_baseline)])
length(predict(model))
lines(metdata_may_york_rel2baseline[,3]~as.Date(googledata_may_york$date),col='orange')



