match.forecast<-function(){
  
# Load packages ----------------------------------------------------------
##Calculate baseline

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
#install.packages("owmr")
library(owmr)

# Inserting in the open weather data --------------------------------------

#Set's the system to my own API key.
Sys.setenv(OWM_API_KEY = "31af4c38fca8bb4ec366db9695bebdc0")
#This gets forecasted weather for the specified "district"
  forecast <-get_forecast("Bedford", units = "metric")%>%
    owmr_as_tibble()

#This extracts the date and time column into two seperate strings.
date_time<-t(as.data.frame(strsplit(forecast$dt_txt," ")))
#Then combines them to the main data frame.
forecast<-as.data.frame(cbind("date" = (as.Date(date_time[,1])), "time" = date_time[,2],forecast))[,-3]

forecast<-subset(forecast, select = c("date","temp","temp_min","temp_max","rain_3h"))

forecast<-forecast%>%
  mutate(
    rain_3h = rain_3h/3
  )

forecast_2<-aggregate(forecast, list(forecast$date), FUN = mean, na.rm = T )[,-1]
forecast_2<-cbind.data.frame("weekdays" = weekdays(forecast_2$date),
                  "date" = forecast_2$date,
                  "sub_region_1" ="Bedford",           
                  "temp_max" = forecast_2$temp_max,
                  "temp_mean" = forecast_2$temp, 
                  "temp_min" =  forecast_2$temp_min,
                  "rain_mean" = forecast_2$rain_3h) 
                  
}
