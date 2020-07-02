# Start -------------------------------------------------------------------
rm(list=ls())
set.seed(1234)
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
getwd()

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

# Load and clean Google data  ------------------------------------------------------------

#Loads Global Mobility report from the designated work directory
Global_Mobility_Report<-read.csv("inputdata/Global_Mobility_Report.csv")
#Makes the date variables as a date class in R.
Global_Mobility_Report$date<-as.Date(Global_Mobility_Report$date)
#Makes the parks-percentage change from baseline variables numeric. 
Global_Mobility_Report$parks_percent_change_from_baseline<-as.numeric(Global_Mobility_Report$parks_percent_change_from_baseline)

#add sub_country column
Global_Mobility_Report<-Global_Mobility_Report %>% add_column(sub_country = NA, .after = which(colnames(Global_Mobility_Report)=="country_region"))

# Subset the data to only present the UK data, separated by GB cod --------
UK<-subset(Global_Mobility_Report,country_region_code == "GB")

# Assigning country to UK districts ---------------------------------------
Districts_by_country<-readRDS("inputdata/Districts_by_country.RDS")

# Code to assign districts to the new data set ----------------------------
#England
UK$sub_country[(UK$sub_region_1%in%Districts_by_country[[1]])]<-"England"
#Wales
UK$sub_country[(UK$sub_region_1%in%Districts_by_country[[2]])]<-"Wales"
#Scotland
UK$sub_country[(UK$sub_region_1%in%Districts_by_country[[3]])]<-"Scotland"
#Northern Ireland
UK$sub_country[(UK$sub_region_1%in%Districts_by_country[[4]])]<-"Northern Ireland"

google_england<-subset(UK,sub_country=='England')
google_scotland<-subset(UK,sub_country=='Scotland')
google_wales<-subset(UK,sub_country=='Wales')
google_nireland<-subset(UK,sub_country=='Northern Ireland')

# Load and clean Met Office data ------------------------------------------

#Load Met Office data
metoffice<-read.csv('inputdata/UKcovidreportingregions_metoffice_global_daily_bbox_20200625.csv')
metoffice_england<-metoffice[metoffice$country=='England',]
metoffice_scotland<-metoffice[metoffice$country=='Scotland',]
metoffice_wales<-metoffice[metoffice$country=='Wales',]
metoffice_nireland<-metoffice[metoffice$country=='Northern Ireland',]

# aggregate england (simple because all google districts are coarser resolution) --------------------------------------------------------------

metoffice_england_agg<-aggregate(metoffice_england[,7:ncol(metoffice_england)],list(metoffice_england$google, metoffice_england$date),mean)
colnames(metoffice_england_agg)[1:2]<-c('sub_region_1','date')

nlevels(as.factor(google_england$sub_region_1))
nlevels(as.factor(metoffice_england_agg$sub_region_1))

# aggregate n.ireland (simple because all google districts are coarser resolution) --------------------------------------------------------------

metoffice_nireland_agg<-aggregate(metoffice_nireland[,7:ncol(metoffice_nireland)],list(metoffice_nireland$google, metoffice_nireland$date),mean)
colnames(metoffice_nireland_agg)[1:2]<-c('sub_region_1','date')

nlevels(as.factor(google_nireland$sub_region_1))
nlevels(as.factor(metoffice_nireland_agg$sub_region_1))


# aggregate scotland ------------------------------------------------------

#find out in which of the 14 Met Office/COVID Reporting regions each of the rows of the google dataframe are found
google_to_covidreportingregions<-unlist(lapply(google_scotland$sub_region_1, grep, metoffice_scotland$google))
google_to_covidreportingregions<-metoffice_scotland$name[google_to_covidreportingregions]
google_scotland_agg<-aggregate(google_scotland,list(google_to_covidreportingregions,google_scotland$date),mean)
google_scotland_agg<-google_scotland_agg[,c(1,11:ncol(google_scotland_agg))]
colnames(google_scotland_agg)[1]<-'district'


# aggregate wales -----------------------------------------------------

#find out in which of the 14 Met Office/COVID Reporting regions each of the rows of the google dataframe are found
google_to_covidreportingregions<-unlist(lapply(google_wales$sub_region_1, grep, metoffice_wales$google))
google_to_covidreportingregions<-metoffice_wales$name[google_to_covidreportingregions]

levels(as.factor(google_wales$sub_region_1))
levels(as.factor(unlist(strsplit(metoffice_wales$google,'; '))))
  
google_wales_agg<-aggregate(google_wales,list(google_to_covidreportingregions,google_wales$date),mean)
google_wales_agg<-google_wales_agg[,c(1,11:ncol(google_wales_agg))]
colnames(google_scotland_agg)[1]<-'district'










google_scotland_agg[,1:7]<-c(
                          rep('GB',nrow(google_scotland_agg)),
                          rep('United Kingdom',nrow(google_scotland_agg)),
                          rep('Scotland',nrow(google_scotland_agg)),
                          levels(as.factor( metoffice_scotland$name)),
                          rep('',nrow(google_scotland_agg)),
                          rep('',nrow(google_scotland_agg)),
                          rep('',nrow(google_scotland_agg)))
                          
