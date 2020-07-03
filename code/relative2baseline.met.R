relative2baseline.met<-function(){
  
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
  
source('match.metoffice2google.R')

# read in met office england dataset and add column for weekdays -------------------------------------------------------------------

metoffice_england<-match.metoffice2google()
#create vector of days of the week and add to dataframe
metoffice_england<-metoffice_england %>% tibble::add_column(weekdays = weekdays(as.Date(metoffice_england$date)), 
                                 .before = 1)

# make relative to baseline -----------------------------------------------

#make a vector of dates within Google's non-baseline/time series period
nonbaselinerange<-seq(as.Date('2020-02-15'),as.Date('2020-06-27'),1)
#get met office england data for the nonbaseline period
metoffice_england_nonbaselineperiod<-metoffice_england[as.Date(metoffice_england$date)%in%nonbaselinerange,]

#make a vector of dates within Google's Baseline period
baselinerange<-seq(as.Date('2020-01-03'),as.Date('2020-02-06'),1)
#get met office england data for the baseline period
metoffice_england_baselineperiod<-metoffice_england[as.Date(metoffice_england$date)%in%baselinerange,]

unique(as.Date(metoffice_england$date))
unique(as.Date(baselinerange))

#create a dataframe of the median values of each meteorological measurement for each district, from the baseline period (as google did for mobility)
baselineweather<-aggregate(metoffice_england_baselineperiod,
                           list(metoffice_england_baselineperiod$weekdays,
                                metoffice_england_baselineperiod$sub_region_1),
                           median)[,-c(1:2)]

#make vector of weekdays
wdays<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
#make vector of districts
districts<-levels(as.factor(metoffice_england$sub_region_1))

#copy df to new df which will be relative to baseline df
metoffice_england_rel2baseline<-metoffice_england_nonbaselineperiod

#This loop calculates the 'baseline change in weather' using Google's method applied to the Met Office data. Process:
  #1. Go through each weekday (first would be Monday)
  #2. Go through each Google district (first would be Bath and Northeast Somerset)
  #3. Go through each of the meteorological variables in columns 4:53 in the Met Office dataframe (first would be air temp mean max K)
  #4. Nonbaseline - A temporary object - in first instance, would be all the Met Office air temp mean max K measurements from all the Mondays from Bath and Northeast Somerset in the non-baseline time series
  #5. Baseline - A temporary object - in first instance, would be all the Met Office air temp mean max K measurements from all the Mondays from Bath and Northeast Somerset in the baseline time series
  #6. Diff_from_baseline - A temporary obect - Subtract baseline from nonbaseline to get absolute change in that variable per weekday per district (as in Google methods)
  #7. Write to the relative change dataframe in the appropriate columns - Make the difference relative by dividing it by the baseline and multiplying by 100 to get in percent as for Google data

#loop through weekdays
for (w in wdays){
  print(w)
  #loop through districts
  for(d in districts){
    print(d)
    #loop through columns
    for (c in 4:ncol(metoffice_england_rel2baseline)){
      print(c)
    
    #for weekday w and district w, get non-baseline median values of meteorological measurement c for
    nonbaseline<-metoffice_england_nonbaselineperiod[metoffice_england_nonbaselineperiod$weekdays==w & metoffice_england_nonbaselineperiod$sub_region_1==d,c]
      
    #for weekday w and district w, get baseline median values of meteorological measurement c
    baseline<-baselineweather[baselineweather$weekdays==w & baselineweather$sub_region_1==d,c]
    
    #subtract the baseline values of meteorological measurement c for weekday w and district w
    diff_from_baseline<-nonbaseline-baseline
    
    #object is the 'nonbaseline rows in the new df (rel2baseline)
    #function is to divide the difference from baseline by the baseline to get relative difference the times by 100 to get in percent
    metoffice_england_rel2baseline[metoffice_england_nonbaselineperiod$weekdays==w & metoffice_england_nonbaselineperiod$sub_region_1==d,c]<-diff_from_baseline/baseline*100
    
    }
  }
}
#Reads in google mobility data
google_mobility<-read.googlemobility()
#Extracts the google mobility dates
google_date<-google_mobility$date[!duplicated(google_mobility$date)]
#Extracts the metoffice dates
metoffice_date<-as.Date(metoffice_england_rel2baseline$date[!duplicated(metoffice_england_rel2baseline$date)])
#Works out the missing dates from between google mobility and metoffice and prints them with a logical value aside them
missing_date<-paste(google_date,(as.character(google_date%in%metoffice_date)),sep = "")
#Extracts all the dates that contain FALSE (as in they do not appear in the metoffice data set)
missing_date<-as.character(missing_date[grep("FALSE",missing_date)])
#This removes the FALSE string from the vector and sets them as a date value. 
missing_date<-gsub("FALSE","",missing_date)
#Creates a vector that repeats the number of districts corresponding to the length of missing dates.
v<-vector()
for(i in districts){
  district<-rep(c(i),time = length(missing_date))
  v<-c(v,district)
}
#Creates a vector that contains the missing dates, repeated 86 times for each district.
date<-as.Date(rep(c(missing_date),time = 86))
#Creates a vector that contains the weekdays corresponding to each missing_date
weekday<-weekdays(date)
#Creates a matrix that makes up the missing values corresponding to the metoffice_data length. (So if another descriptor is added this will take that into consideration.)
Na_matrix<-matrix(data = NA, nrow = 86*length(missing_date), ncol = (ncol(metoffice_england_rel2baseline)-3))
#Creates a data frame corresponding to the length of the metoffice data and changes their column names.
missing_metoffice<-data.frame(weekday,date,v,Na_matrix)
colnames(missing_metoffice)<-c(colnames(metoffice_england_rel2baseline))
#Binds the missing dataset with the met office data set.
metoffice_england_rel2baseline[,2]<-as.Date(metoffice_england_rel2baseline$date)
metoffice_england_rel2baseline<-rbind(metoffice_england_rel2baseline,missing_metoffice)
}