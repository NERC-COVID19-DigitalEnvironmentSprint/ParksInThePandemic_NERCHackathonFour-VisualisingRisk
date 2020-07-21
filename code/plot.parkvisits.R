plot.parkvisits<-function(google, model, district="Bedford", dayofweek=weekdays(Sys.Date())){
  
  #model = create.model()
  ##OR
  #model = readRDS("input_data/RF_model.RDS")
  #google = read.csv("input_data/testdata/googleandmetoffice_england.csv")
  #district = 'Bedford'
  #dayofweek = 'Wednesday'
  
  
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
  
  #Ensures the dates are in order for analysis.
  google_metoffice<-google_metoffice[order(as.Date(google_metoffice$date)),]
  
  #Ensures that infinite values are treated as 0.
  google_metoffice$rain_mean[is.infinite(google_metoffice$rain_mean)]<-0
  
  # Reading in the forecasting data and getting predictions based on it -----------------------------------------
  
  source('getandmatch.forecast.R')
  forecast<-getandmatch.forecast(district)
  
  prediction_row<-cbind(parks_percent_change_from_baseline = predict(model,forecast[rownames(forecast)==dayofweek,]),
                        sub_region_1 = district,
                        forecast[rownames(forecast)==dayofweek,]
                        )
  prediction_row<-subset(prediction_row, select = c("date", "parks_percent_change_from_baseline"))
  
  google_metoffice_current_district_and_weekday<-subset(google_metoffice,
                                        google_metoffice$sub_region_1 == district
                                        & weekdays(google_metoffice$date) == dayofweek,
                                        select = c ("date", "parks_percent_change_from_baseline"))
  
  google_metoffice_current_district_and_weekday<-rbind(google_metoffice_current_district_and_weekday,prediction_row)
  
  
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


#example implementation
#google = read.csv("input_data/testdata/google_england.csv")
#plot.googlemobilitydistricts(google)
