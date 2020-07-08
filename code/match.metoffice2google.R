match.metoffice2google<-function(){
  
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
  
source('read.metofficecovid.R')
source('read.googlemobility.R')
  
#load Met Office data for May
metoffice<-read.metofficecovid()
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
google<-read.googlemobility()
#subset out only England data from the Googl dataframe
google_england<-subset(google,sub_country=='England')

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
metoffice_england

}

