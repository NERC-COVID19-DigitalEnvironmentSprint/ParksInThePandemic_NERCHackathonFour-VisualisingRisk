plot.parkvisits<-function(googleandmetoffice, model, forecast, district="Bedford", dayofweek=wday(as.Date(Sys.Date())), predict_missing){
  
  #borrowed from Kirill (https://stackoverflow.com/questions/32434549/how-to-find-next-particular-day)
  nextweekday <- function(date, wday) {
    date <- as.Date(date)
    diff <- wday - wday(date)
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
  library(lubridate)
  
  
  
  #manually set options to test (assuming all data downloaded from OSF into 'data' folder)
  #googleandmetoffice<-read.csv('data/temporal/googleandmetoffice_england.csv')
  #model<-readRDS('data/model/RF_model.RDS')  
  #forecast<-read.csv('data/model/forecasts_england.csv')
  #district = 'Bedford'
  #dayofweek=3
  #predict_missing = T
  
  wkdays_eng=c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  dayofweek_eng<-wkdays_eng[dayofweek]
  
  # GET FORECAST FOR THE DAY UNDER CONSIDERATION AND PLOT PREDICTED VISITS BASED ON IT ----------------------------
  
  #Subsets out the google_metoffice data to only have the relevant factors.
  google_metoffice<-subset(googleandmetoffice, select = c("parks_percent_change_from_baseline",
                                                          "sub_region_1",
                                                          "date",
                                                          "weekday",
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
  
  #Ensures the dates are in order for analysis.
  google_metoffice<-google_metoffice[order(as.Date(google_metoffice$date)),]
  
  #Extracting the most recent up-to-date date for the metoffice data
  metoffice_date<-na.omit(google_metoffice)[nrow(na.omit(google_metoffice)),3]
  
  # Identifying missing na values for prediction values ---------------------
  #Identifies missing NA values.
  google_metoffice_missing<-subset(google_metoffice,is.na(google_metoffice$parks_percent_change_from_baseline))
  #Only extracts missing values relevant to district
  google_metoffice_missing<-subset(google_metoffice_missing, sub_region_1 == district)[,-1]
  #Removes those that NA value for weather data
  google_metoffice_missing<-na.omit(google_metoffice_missing)[-1]
  
  # Reading in the forecasting data and getting predictions based on it -----------------------------------------
  
  forecast<-forecast[,-1]
  forecast<-subset(forecast,sub_region_1==district)[,-1]
  #Takes out the district information to be added the missing dataset.
  forecast_district<-forecast[1,7:13]
  
  google_metoffice_missing<-cbind.data.frame(google_metoffice_missing,forecast_district)
  
  google_metoffice_missing<-subset(google_metoffice_missing, weekday == dayofweek_eng)
  
  ifelse(length(google_metoffice_missing$date) == 0,
    google_metoffice_missing<-0,
    google_metoffice_missing<-cbind.data.frame("date" = google_metoffice_missing$date,
                                               parks_percent_change_from_baseline = predict(model,google_metoffice_missing)))
  
  #google_metoffice_missing<-cbind.data.frame("date" = google_metoffice_missing$date,
    #                                         parks_percent_change_from_baseline = predict(model,google_metoffice_missing))
  
  #if selected weekday exists in forecast, make a prediction row, else, make one with NAs
  if(dayofweek_eng%in%forecast$weekday==TRUE){
    
    prediction_row<-cbind(parks_percent_change_from_baseline = predict(model,forecast[forecast$weekday==dayofweek_eng,]),
                          sub_region_1 = district,
                          forecast[forecast$weekday==dayofweek_eng,])
    prediction_row<-subset(prediction_row, select = c("date", "parks_percent_change_from_baseline"))
    
  } else
  {
    
    n_wday<-as.Date(nextweekday(Sys.Date(),dayofweek))
    prediction_row<-t(as.data.frame(c("date"=n_wday,parks_percent_change_from_baseline=NA)))
    rownames(prediction_row) = NULL
  }
  
  if(google_metoffice_missing == 0){
    prediction_row<-as.data.frame(prediction_row)
  }else
    {
    ifelse(predict_missing == T, 
         prediction_row<-rbind.data.frame(google_metoffice_missing,prediction_row),
         prediction_row<-as.data.frame(prediction_row))
  }
  
  prediction_row$colour_choice<-"white"
  
  google_metoffice_current_district_and_weekday<-subset(google_metoffice,
                                                        google_metoffice$sub_region_1 == district
                                                        & weekdays(google_metoffice$date) == dayofweek_eng,
                                                        select = c ("date", "parks_percent_change_from_baseline"))
  
  #Add colour choice column
  google_metoffice_current_district_and_weekday$colour_choice<-ifelse(google_metoffice_current_district_and_weekday$parks_percent_change_from_baseline>0,'darkgreen','grey')
  #Remove unnecessary NAs'
  google_metoffice_keep_na<-subset(google_metoffice_current_district_and_weekday, date >= metoffice_date)
  google_metoffice_keep_na<-subset(google_metoffice_keep_na, is.na(parks_percent_change_from_baseline))
  
  ifelse(length(google_metoffice_keep_na$date) == 0,
         google_metoffice_keep_na<-0,
         google_metoffice_keep_na<-cbind.data.frame("date"= google_metoffice_keep_na$date, "parks_percent_change_from_baseline" = 0, "colour_choice" = "white"))
  #google_metoffice_keep_na<-cbind.data.frame("date"= google_metoffice_keep_na$date, "parks_percent_change_from_baseline" = 0, "colour_choice" = "white")
  #Remove Nas'
  google_metoffice_current_district_and_weekday<-na.omit(google_metoffice_current_district_and_weekday)
  
  #rebinding the dataframe
  if(google_metoffice_keep_na == 0){
    google_metoffice_current_district_and_weekday<-rbind(google_metoffice_current_district_and_weekday,prediction_row)
  }else
  {
    google_metoffice_current_district_and_weekday<-rbind(google_metoffice_current_district_and_weekday,google_metoffice_keep_na,prediction_row)
  }
  
  #Order for plotting
  google_metoffice_current_district_and_weekday<-google_metoffice_current_district_and_weekday[order(as.Date(google_metoffice_current_district_and_weekday$date)),]
  #change parks percentage change from basline to numeric
  google_metoffice_current_district_and_weekday$parks_percent_change_from_baseline<-as.numeric(google_metoffice_current_district_and_weekday$parks_percent_change_from_baseline)
  
  
  #START OF THE PLOTTING FUNCTION
  
  #making the colour vector
  colour_vector<-google_metoffice_current_district_and_weekday$colour_choice[!duplicated(google_metoffice_current_district_and_weekday$colour_choice)]
  aes_vector<-cbind.data.frame(
    "colour_name" = c("darkgreen","grey","white"),
    "label_name" = c("actual positive value","actual negative value","predicted value"))
  aes_number<-match(colour_vector,aes_vector$colour_name)
  aes_number<-sort(aes_number)
  
  ifelse(length(aes_number) == 1,
         aes_vector<- subset(aes_vector,aes_vector$colour_name == colour_vector[(aes_number[1])]),
         ifelse(length(aes_number) == 2,
                aes_vector<-rbind(subset(aes_vector,aes_vector$colour_name == aes_vector$colour_name[(aes_number[1])]),
                                  subset(aes_vector,aes_vector$colour_name == aes_vector$colour_name[(aes_number[2])])),
                aes_vector<-aes_vector))
  
  #Cleans the type title to ensure that _ do not exist in the y axis and recreates the y-axis.
  country_ylab<-paste("Visit changes for parks (%) relative to per-weekday winter baselines \n(Google Community Mobility data)")
  
  District_graph<-ggplot2::ggplot() +
    #Plots the bar graphs, with a black outing and dark orange fill. 
    geom_col(data = google_metoffice_current_district_and_weekday, mapping = aes(x = as.Date(date), y = parks_percent_change_from_baseline, fill = colour_choice),position = position_dodge(width=0.2), size=0.25,colour = 'black') +
    scale_fill_manual(values = aes_vector$colour_name,
                      name = "Date type", 
                      labels = aes_vector$label_name)+
    #Limits the size of the graph.
    coord_cartesian(ylim=c(-100,400)) +
    #plots a horizontal line where no percentage change occurs.
    geom_hline(yintercept = 0) + 
    #geom_vline(xintercept = forecast_date, colour = "red", linetype = "dashed")+
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
#plot.parkvisits(googleandmetoffice = googleandmetoffice,model = model, forecast = forecast, dayofweek = 1,predict_missing = T)
#plot.parkvisits(googleandmetoffice = googleandmetoffice,model = model, forecast = forecast, dayofweek = 2,predict_missing = T)
#plot.parkvisits(googleandmetoffice = googleandmetoffice,model = model, forecast = forecast, dayofweek = 3,predict_missing = T)
#plot.parkvisits(googleandmetoffice = googleandmetoffice,model = model, forecast = forecast, dayofweek = 4,predict_missing = T)
#plot.parkvisits(googleandmetoffice = googleandmetoffice,model = model, forecast = forecast, dayofweek = 5,predict_missing = T)
#plot.parkvisits(googleandmetoffice = googleandmetoffice,model = model, forecast = forecast, dayofweek = 6,predict_missing = T)
#plot.parkvisits(googleandmetoffice = googleandmetoffice,model = model, forecast = forecast, dayofweek = 7,predict_missing = T)


