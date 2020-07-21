plot.parkvisits<-function(google, model, district="Bedford", dayofweek=weekdays(Sys.Date()), predict_missing){
  
  #model = create.model()
  ##OR
  model = readRDS("input_data/RF_model.RDS")
  google = read.csv("input_data/testdata/googleandmetoffice_england.csv")
  district = 'Bedford'
  dayofweek = 'Wednesday'
  predict_missing = FALSE
  
  
  # Load packages ----------------------------------------------------------
  #install.packages('tibble')
  library(tibble)
  #install.packages('ggplot2')
  library(ggplot2)
  
  # GET FORECAST FOR THE DAY UNDER CONSIDERATION AND PLOT PREDICTED VISITS BASED ON IT ----------------------------
  
  
  #Subsets out the google_metoffice data to only have the relevant factors.
  google_metoffice<-subset(google, select = c("parks_percent_change_from_baseline",
                                              "sub_region_1",
                                              "date",
                                              "temp_mean",
                                              "temp_max",
                                              "temp_min",
                                              "rain_mean"))
  #Converting each rows class for analysis
  google_metoffice$parks_percent_change_from_baseline<-as.numeric(google_metoffice$parks_percent_change_from_baseline)
  google_metoffice$date<-as.Date(google_metoffice$date)
  
  #Adds weekdays for prediction analysis for modelling
  google_metoffice<-add_column(google_metoffice, "weekday" = weekdays(google_metoffice$date),.after = "date")
  
  #Ensures the dates are in order for analysis.
  google_metoffice<-google_metoffice[order(as.Date(google_metoffice$date)),]
  
  #Ensures that infinite values are treated as 0.
  google_metoffice$rain_mean[is.infinite(google_metoffice$rain_mean)]<-0
  
  #Extracting the most recent up-to-date date for the metoffice data
  metoffice_date<-na.omit(google_metoffice)[nrow(na.omit(google_metoffice)),3]
  
# Identifying missing na values for prediction values ---------------------
  #Identifies missing NA values.
  google_metoffice_missing<-subset(google_metoffice,is.na(google_metoffice$parks_percent_change_from_baseline))
  #Only extracts missing values relevant to district
  google_metoffice_missing<-subset(google_metoffice_missing, sub_region_1 == district)[,-1]
  #Removes those that NA value for weather data
  google_metoffice_missing<-na.omit(google_metoffice_missing)
  
  
  # Reading in the forecasting data and getting predictions based on it -----------------------------------------
  
  source('getandmatch.forecast.R')
  forecast<-getandmatch.forecast(district)
  #Takes out the district information to be added the missing dataset.
  forecast_district<-forecast[1,7:13]
  #Combine this with the google_metoffice_missing data for compatibility with model
  google_metoffice_missing<-cbind.data.frame(google_metoffice_missing,forecast_district)
  #Combine with the forecasting data.
  ifelse(predict_missing == T, 
        forecast_all<-rbind.data.frame(google_metoffice_missing[-1],forecast),
        forecast_all<-forecast)
  
  prediction_row<-cbind(parks_percent_change_from_baseline = predict(model,forecast_all[forecast_all$weekday==dayofweek,]),
                        sub_region_1 = district,
                        forecast_all[forecast_all$weekday==dayofweek,]
                        )
  prediction_row<-subset(prediction_row, select = c("date", "parks_percent_change_from_baseline"))
  prediction_row$colour_choice<-"white"
  
  google_metoffice_current_district_and_weekday<-subset(google_metoffice,
                                        google_metoffice$sub_region_1 == district
                                        & weekdays(google_metoffice$date) == dayofweek,
                                        select = c ("date", "parks_percent_change_from_baseline"))
  
  #Add colour choice column
  google_metoffice_current_district_and_weekday$colour_choice<-ifelse(google_metoffice_current_district_and_weekday$parks_percent_change_from_baseline>0,'darkgreen','grey')
  #Remove unnecessary NAs'
  google_metoffice_keep_na<-subset(google_metoffice_current_district_and_weekday, date >= metoffice_date)
  google_metoffice_keep_na<-cbind.data.frame("date"= google_metoffice_keep_na$date, "parks_percent_change_from_baseline" = 0, "colour_choice" = "white")
  #Remove Nas'
  google_metoffice_current_district_and_weekday<-na.omit(google_metoffice_current_district_and_weekday)
  
  google_metoffice_current_district_and_weekday<-rbind(google_metoffice_current_district_and_weekday,google_metoffice_keep_na,prediction_row)
  google_metoffice_current_district_and_weekday<-google_metoffice_current_district_and_weekday[order(as.Date(google_metoffice_current_district_and_weekday$date)),]
  
  #Extract the forecasting date
  forecast_date<-google_metoffice_current_district_and_weekday[nrow(google_metoffice_current_district_and_weekday),"date"]-3
  
  #START OF THE PLOTTING FUNCTION

  #Cleans the type title to ensure that _ do not exist in the y axis and recreates the y-axis.
  country_ylab<-paste("Visit changes for parks (%) relative to per-weekday winter baselines \n(Google Community Mobility data)")
  
  District_graph<-ggplot2::ggplot() +
  #Plots the bar graphs, with a black outing and dark orange fill. 
  geom_col(data = google_metoffice_current_district_and_weekday, mapping = aes(x = as.Date(date), y = parks_percent_change_from_baseline, fill = colour_choice),position = position_dodge(width=0.2), size=0.25,colour = 'black') +
    scale_fill_manual(values = c("darkgreen","grey","white"),
                      name = "Date type", 
                      labels = c("actual positive value",
                                                     "actual negative value",
                                                     "predicted value"))+
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


#example implementation
#google = read.csv("input_data/testdata/google_england.csv")
#plot.googlemobilitydistricts(google)
