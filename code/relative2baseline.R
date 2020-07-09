#Argument data, defines the dataset you want to convert to percentage baseline change.

relative2baseline<-function(data){

# Load packages ----------------------------------------------------------
#install.packages('tibble')
library(tibble)
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

source('match.metoffice.R')

# Reads in the met-office england dataset and finds relative baseline values. -------------------------------------------------------------------
metoffice_england<-match.metoffice()

#Makes a vector of dates within Google's Baseline period
baselinerange<-seq(as.Date('2020-01-03'),as.Date('2020-02-06'),1)

#Gets met office england data for the baseline period
metoffice_england_baselineperiod<-metoffice_england[as.Date(metoffice_england$date)%in%baselinerange,]

unique(as.Date(baselinerange))

#create a dataframe of the median values of each meteorological measurement for each district, from the baseline period (as google did for mobility)
baselineweather<-aggregate(metoffice_england_baselineperiod,
                           list(metoffice_england_baselineperiod$weekdays,
                                metoffice_england_baselineperiod$sub_region_1),
                           median)[,-c(1:2)]

# Prepares the specified data set -----------------------------------------

data[,"date"]<-as.Date(data[,"date"])

nonbaselineperiod<-subset(data,data$date >= "2020-02-15")


# For loop PREPARATION ----------------------------------------------------
#make vector of weekdays
wdays<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
#make vector of districts
districts<-levels(as.factor(metoffice_england$sub_region_1))
#copy df to new df which will be relative to baseline df

rel2baseline<-nonbaselineperiod

#This loop calculates the 'baseline change in weather' using Google's method applied to the Met Office data. Process:
#1. Go through each weekday (first would be Monday)
#2. Go through each Google district (first would be Bath and Northeast Somerset)
#3. Go through each of the meteorological variables in columns 4:53 in the Met Office dataframe (first would be air temp mean max K)
#4. Nonbaseline - A temporary object - in first instance, would be all the Met Office air temp mean max K measurements from all the Mondays from Bath and Northeast Somerset in the non-baseline time series
#5. Baseline - A temporary object - in first instance, would be all the Met Office air temp mean max K measurements from all the Mondays from Bath and Northeast Somerset in the baseline time series
#6. Diff_from_baseline - A temporary obect - Subtract baseline from nonbaseline to get absolute change in that variable per weekday per district (as in Google methods)
#7. Write to the relative change dataframe in the appropriate columns - Make the difference relative by dividing it by the baseline and multiplying by 100 to get in percent as for Google data

baseline_name<-c("temp_max","temp_mean","temp_min","rain_mean")

#loop through weekdays
for (w in wdays){
  #loop through districts
  for(d in districts){
    #loop through columns
    for (c in baseline_name){
      
      #for weekday w and district w, get non-baseline median values of meteorological measurement c for
      nonbaseline<-nonbaselineperiod[nonbaselineperiod$weekdays==w & nonbaselineperiod$sub_region_1==d,c]
      
      #for weekday w and district w, get baseline median values of meteorological measurement c
      baseline<-baselineweather[baselineweather$weekdays==w & baselineweather$sub_region_1==d,c]
      
      #subtract the baseline values of meteorological measurement c for weekday w and district w
      diff_from_baseline<-nonbaseline-baseline
      
      #object is the 'nonbaseline rows in the new df (rel2baseline)
      #function is to divide the difference from baseline by the baseline to get relative difference the times by 100 to get in percent
      rel2baseline[nonbaselineperiod$weekdays==w & nonbaselineperiod$sub_region_1==d,c]<-diff_from_baseline/baseline*100
      
    }
  }
}
}
