plot.parkvisits<-function(googleandmetoffice, model, forecast, district="Bedford", dayofweek=data.table::wday(as.Date(Sys.Date()))){
  
  #borrowed from Kirill (https://stackoverflow.com/questions/32434549/how-to-find-next-particular-day)
  nextweekday <- function(date, wday) {
    date <- as.Date(date)
    diff <- wday - data.table::wday(date)
    if( diff < 0 )
      diff <- diff + 7
    return(date + diff)
  }
  
  
  # Load packages ----------------------------------------------------------
  #install.packages('tibble')
  library(tibble)
  #install.packages('ggplot2')
  library(ggplot2)
  #install.packages(randomForest)
  library(randomForest) 
  
  #manually set options to test (assuming all data downloaded from OSF into 'data' folder)
  #googleandmetoffice<-read.csv('data/temporal/googleandmetoffice_england.csv')
  #model<-readRDS('data/model/RF_model.RDS')  
  #forecast<-read.csv('data/model/forecasts_england.csv')
  #district = 'Bedford'
  #dayofweek=2
  
  wkdays_eng=c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  dayofweek_eng<-wkdays_eng[dayofweek]
  
  # GET FORECAST FOR THE DAY UNDER CONSIDERATION AND PLOT PREDICTED VISITS BASED ON IT ----------------------------
  
  #Subsets out the google_metoffice data to only have the relevant factors.
  google_metoffice<-subset(googleandmetoffice, select = c("parks_percent_change_from_baseline",
                                              "sub_region_1",
                                              "date",
                                              "temp_mean",
                                              "temp_max",
                                              "temp_min",
                                              "rain_mean"))
  #Converting each rows class for analysis
  google_metoffice$parks_percent_change_from_baseline<-as.numeric(google_metoffice$parks_percent_change_from_baseline)
  google_metoffice$date<-as.Date(google_metoffice$date)
  
  #Ensures the dates are in order for analysis.
  google_metoffice<-google_metoffice[order(as.Date(google_metoffice$date)),]
  
  #Ensures that infinite values are treated as 0.
  google_metoffice$rain_mean[is.infinite(google_metoffice$rain_mean)]<-0
  
  # Reading in the forecasting data and getting predictions based on it -----------------------------------------
  
  forecast<-forecast
  forecast<-subset(forecast,sub_region_1==district)
  
  
  #if selected weekday exists in forecast, make a prediction row, else, make one with NAs
  if(dayofweek_eng%in%forecast$weekday==TRUE){
    
    prediction_row<-cbind(parks_percent_change_from_baseline = predict(model,forecast[forecast$weekday==dayofweek_eng,]),
                          sub_region_1 = district,
                          forecast[forecast$weekday==dayofweek_eng,])
    prediction_row<-subset(prediction_row, select = c("date", "parks_percent_change_from_baseline"))
    
  } else
  {
    
    n_wday<-as.Date(nextweekday(Sys.Date(),dayofweek))
    prediction_row<-c(date=n_wday,parks_percent_change_from_baseline=NA)
  }
                    
 
  google_metoffice_current_district_and_weekday<-subset(google_metoffice,
                                        google_metoffice$sub_region_1 == district
                                        & weekdays(google_metoffice$date) == dayofweek_eng,
                                        select = c ("date", "parks_percent_change_from_baseline"))
  
  #coerce to character before binding to avoid really annoying date issues
  google_metoffice_current_district_and_weekday$date<-as.character(google_metoffice_current_district_and_weekday$date)
  #bind predictions to bottom
  google_metoffice_current_district_and_weekday<-rbind(google_metoffice_current_district_and_weekday,prediction_row)
  #recoerce to date
  google_metoffice_current_district_and_weekday$date<-as.Date(google_metoffice_current_district_and_weekday$date)

#START OF THE PLOTTING FUNCTION

  #Cleans the type title to ensure that _ do not exist in the y axis and recreates the y-axis.
  country_ylab<-paste("Visit changes for parks (%) relative to per-weekday winter baselines \n(Google Community Mobility data)")
  
  colours<-ifelse(google_metoffice_current_district_and_weekday$parks_percent_change_from_baseline>0,'darkgreen','grey')
  colours[length(colours)]<-'white'
  
  District_graph<-ggplot2::ggplot() +
  #Plots the bar graphs, with a black outing and dark orange fill. 
  geom_col(google_metoffice_current_district_and_weekday, mapping = aes(x = as.Date(date), y = parks_percent_change_from_baseline),position = position_dodge(width=0.2), size=0.25,colour = 'black', fill =colours) +
  #Limits the size of the graph.
  coord_cartesian(ylim=c(-100,400)) +
  #plots a horizontal line where no percentage change occurs.
  geom_hline(yintercept=0) + 
  #Ensure the background is white, the border is black and removes grid lines.
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey"),
        strip.text = element_blank())+
  #x-label
  xlab("Date") +
  #y-label using the previous clean code done outside the plot.
  ylab(country_ylab)+
  #Add a title for the district data this graph represents.
  ggtitle(district)
District_graph

}

