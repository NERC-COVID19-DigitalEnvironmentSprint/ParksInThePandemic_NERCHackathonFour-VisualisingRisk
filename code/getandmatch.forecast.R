getandmatch.forecast<-function(location = "Bedford", apikey=readRDS("./../APIkey.RDS")){
# Load packages ----------------------------------------------------------
##Calculate baseline

#install.packages('tibble')
library(tibble)
#install.packages('dplyr')
library(dplyr)
#install.packages("owmr")
library(owmr)

# Inserting in the open weather data --------------------------------------
#Saving location for later
end_location<-location

#Retrieves the APIkey from the offline Github folder containing your cloned repos
APIkey<-apikey
  
#Set's the system to my own API key.
Sys.setenv(OWM_API_KEY = APIkey)

#Convert location, if necessary
location_1<-ifelse(location == "Greater Manchester", location <- c("Bolton","Bury","Manchester","Oldham","Rochdale","Salford","Tameside","Trafford","Wigan"),
                 ifelse(location == "Tyne and Wear", location <- c("Gateshead","Newcastle upon Tyne","South Tyneside","Sunderland","Holystone"),
                        ifelse(location == "Merseyside", location <- c("Knowsley","Liverpool","Sefton","St. Helens","Stockport","Wirral"),
                               ifelse(location == "West Yorkshire", location <-c("Bradford","Calderdale","Kirklees","Leeds","Wakefield"),
                                      ifelse(location == "South Yorkshire", location <- c("Barnsley","Doncaster","Rotherham","Sheffield"),
                                             ifelse(location == "West Midlands", location <- c("Birmingham","Coventry","Dudley","Sandwell","Solihull","Telford and Wrekin","Walsall","Wolverhampton"),
                                                    ifelse(location == "Windsor and Maidenhead", location <- c("Windsor","Maidenhead"),
                                                           ifelse(location == 'Bristol City', location <- c('Bristol'),
                   location))))))))

#Get's the forecast and creates a tibble for the first location in the location vector.
forecast <-owmr::get_forecast(location[1], units = "metric")%>%
  owmr_as_tibble()%>%
#Then adds a column for it's designated sub_region_1.
  tibble::add_column(sub_region_1 = location[1],.after = "dt_txt")

#If the location vector is above 1, it adds the additional location data into the forecasting data frame.
if(length(location) > 1){
         forecast<-forecast
         for (i in 2:length(location)) {
            forecast_1<-owmr::get_forecast(location[i], units = "metric")%>%
                        owmr_as_tibble()%>%
                        tibble::add_column(sub_region_1= location[i], .after = "dt_txt")
           forecast<-merge(forecast,forecast_1, all=T)
  }
}

#This extracts the date and time column into two separate strings.
date_time<-t(as.data.frame(strsplit(forecast$dt_txt," ")))
#Then combines them to the main data frame.
forecast<-as.data.frame(cbind("date" = (as.Date(date_time[,1])), "time" = date_time[,2],forecast))[,-3]
#Selects a subset of the forecast dataframe for easing transformations of the table.
forecast<-subset(forecast, select = c("date","sub_region_1","temp","temp_min","temp_max","rain_3h"))
#Converts the rain per 3 hours into rain per hour to be compatible with the metoffice.
forecast<-forecast%>%
  dplyr::mutate(
    rain_3h = rain_3h/3
  )
#coerces NAs to zero
forecast$rain_3h[is.na(forecast$rain_3h)] <-0

#If location is not equal to the end location (i.e. it's a vector) then it will aggregate all the values
ifelse (location != end_location,
      forecast_2<-aggregate(forecast[,3:length(forecast)], list(forecast$date,forecast$sub_region_1), FUN = mean, na.rm = T ),
      forecast_2<-forecast
      )
#This renames the column names as the above code will change the column names if an aggregation has taken place.
colnames(forecast_2)<-c("date","sub_region_1","temp","temp_min","temp_max","rain_3h")

#This calculates the aggregation value for mean temperature by taking the mean.
forecast_temp_mean<-aggregate(forecast_2$temp, list(forecast_2$date), FUN = mean, na.rm = T )
#This calculates the max temperature by taking the max.
forecast_temp_max<-aggregate(forecast_2$temp_max, list (forecast_2$date), FUN = max, na.rm = T)
#This calculates the min temperature by taking the min.
forecast_temp_min<-aggregate(forecast_2$temp_min, list (forecast_2$date), FUN = min, na.rm = T)
#This calculates the aggregation value for the mean rainfall per hour by taking the mean.
forecast_rain_mean<-aggregate(forecast_2$rain_3h, list (forecast_2$date), FUN = mean, na.rm = T)

#This combines the above vectors created and adds weekdays for each value, date, and creates a column for sub_region_1 based from the first definition.
forecast_2<-cbind.data.frame(
                  "date" = forecast_temp_mean$Group.1,
                  'weekday' = weekdays(forecast_temp_mean$Group.1),
                  "temp_mean" = forecast_temp_mean$x,
                  "temp_max" = forecast_temp_max$x,
                  "temp_min" =  forecast_temp_min$x,
                  "rain_mean" = forecast_rain_mean$x)

#change NaNs to NAs in dataframe
forecast_2[do.call(cbind, lapply(forecast_2,is.nan))]<-NA

#Reads in menedata
mene_england = read.csv("code/input_data/testdata/mene_england.csv")
#Reads in garden_access data
garden_access = read.csv("code/input_data/testdata/garden_access.csv")
#Removes one column that is not required for modelling from garden_access.
garden_access<-garden_access[-3]
#Combines the appropriate location information from both datasets to the forecast datatset and removes another column from garden_access.
forecast_2<-cbind.data.frame(forecast_2,subset(garden_access[-4],google_district == end_location)[,-1],"annual_visits_per_capita_per_km2_greenspace_1km_radius" = subset(mene_england,sub_region_1 == end_location)[,4])
}

