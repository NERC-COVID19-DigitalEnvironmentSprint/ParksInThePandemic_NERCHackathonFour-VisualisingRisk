##Plotting predicted mobility

##ACKNOWLEDGMENT##
#Forecasting data is extracted from the OpenWeather data sets using the owmr package.
#For more information on the OpenWeather dataset, this can be accessed from https://openweathermap.org/forecast5 

##To check it works## - This has to be before the function.
  #district = "Bedford"
  #api = "~/GitHub/apikey.RDS"

    #Sources the code to retrieve the forecasting data.
    ##source("~/GitHub/parksinthepandemic/code/getandmatch.forecast.R")  
    #Sources the code to make the forecasting relative to baseline.
    ##source("~/GitHub/parksinthepandemic/code/relative2baseline.R")
    #Runs the getandmatch.forecast code to retrieve the forecasting data.
    ##forecast<-getandmatch.forecast(location = district, apikey = api)
    #Runs the code that converts the forcasting data to be relative to baseline.
    ##forecast<-relative2baseline(data = forecast)

##EXAMPLES of values to be inputted##
#forecast = output of getandmatch.forecast and after converted to relative to baseline.
#days = 2


plot.forecast<-function(forecast, days = 2){

# Loading packages --------------------------------------------------------
#install.packages('data.table')
require(data.table)
#install.packagaes('randomForest')
require(randomForest)
#install.packages('tibble')
require(tibble)
#install.packages('ggplot2')
require(ggplot2)
#install.packages('raster')
require(raster)
 
#Read in google mobility data##
#Reads in the google_metoffice data
google_metoffice<-read.csv("~/GitHub/parksinthepandemic/code/input_data/testdata/googleandmetoffice_england.csv")
#Subsets out the google_metoffice data to only have the relevant factors.
google_metoffice<-subset(google_metoffice, select = c("parks_percent_change_from_baseline",
                          "sub_region_1",
                          "date",
                          "weekdays",
                          "temp_mean",
                          "temp_max",
                          "temp_min",
                          "rain_mean"))
#Converting each rows class for analysis
google_metoffice$parks_percent_change_from_baseline<-as.numeric(google_metoffice$parks_percent_change_from_baseline)
google_metoffice$date<-as.Date(google_metoffice$date)
google_metoffice$weekdays<-as.factor(google_metoffice$weekdays)
#Ensures the dates are in order for analysis.
google_metoffice<-google_metoffice[order(as.Date(google_metoffice$date)),]
#Ensures that infinite values are treated as 0.
google_metoffice$rain_mean[is.infinite(google_metoffice$rain_mean)]<-0
#Removes all NA data for analysis. 
google_metoffice_na<-na.omit(google_metoffice)


# Reading in the forecasting data -----------------------------------------


#Extracts the factors that are used within the model. 
forecast_1<-cbind.data.frame( "date" = as.Date(forecast$date),
                                      "weekdays" = as.factor(forecast$weekdays),
                                      "temp_mean" = forecast$temp_mean, 
                                      "temp_max" = forecast$temp_max,
                                      "temp_min" = forecast$temp_min, 
                                      "rain_mean" = forecast$rain_mean)

# Using a model to predict values -----------------------------------------
#This extracts the sub_region to maintain the subregions
##sub_region_1<-google_metoffice_na$sub_region_1

#Ensures the sub_region column is not used in the analysis.
google_metoffice_na<-google_metoffice_na[,-2]

#This code generates the model used to produce the predictor values. 
RF_model<-randomForest::randomForest(google_metoffice_na[,-1],google_metoffice_na$parks_percent_change_from_baseline)


##PLOT##------------------------------------------------------
#Preparing data to plot with##

#Ensures the headings are compatible with one another for predicting.
forecast_test<-raster::bind(google_metoffice_na[1,],forecast_1)
#Removes the parks_percent_change_from_baseline value and the first row that was integrated above.
forecast_test<-forecast_test[-1,-1]
#Creates a small dataframe that only corresponds for date and parks_percent_change_from_baseline.
Forecast_predict<-cbind.data.frame("date" = forecast_test$date,
                                   "parks_percent_change_from_baseline" = predict(RF_model,forecast_test))
#This adds a column to allow for subsetting positive and negative percentage change values.
Forecast_predict<-tibble::add_column(Forecast_predict,mean_colour =  
                                       (ifelse(Forecast_predict[,"parks_percent_change_from_baseline"] >= 0,"green","grey")))

#This creates a column to maintain the length of the vector required
Forecast_length<-Forecast_predict$date
#This creates a vector that contains all values when they are positive and substitutes them for 0 if not.
Data_green<-ifelse(Forecast_predict[1,3]=="green",Forecast_predict[1,2],0)
for(i in 2:nrow(Forecast_predict)){
  Data_green_1<-ifelse(Forecast_predict[i,3]=="green",Forecast_predict[i,2],0)
  Data_green<-cbind(Data_green,Data_green_1)
}
#If all values are not green, but grey, this ensures the vector is a vector of 0 and not a single value.
ifelse(length(Data_green) == 0, Data_green <- rep(0, length(Forecast_length)),0)
#This creates a vector that contains all values when they are negative and substitutes them for 0 if not.
Data_grey<-ifelse(Forecast_predict[1,3]=="grey",Forecast_predict[1,2],0)
for(i in 2:nrow(Forecast_predict)){
  Data_grey_1<-ifelse(Forecast_predict[i,3]=="grey",Forecast_predict[i,2],0)
  Data_grey<-cbind(Data_grey,Data_grey_1)
}
#If all the values are not grey, but green, this ensures the vector is a vector of 0 and not a single value.
ifelse(length(Data_grey) == 0, Data_green <- rep(0, length(Forecast_length)),0)
#This adds the new vector to the forecast_predict dataset.
Forecast_predict$green<-t(Data_green)
#This adds the new vector to the forecast_predict dataset.
Forecast_predict$grey<-t(Data_grey)

#Subset out for 2
Forecast_predict<-Forecast_predict[1:days,]

#START OF THE PLOTTING FUNCTION
District_graph<-ggplot2::ggplot(data = Forecast_predict) +
  #Plots the bar graphs, with a black outing and dark orange fill. 
  geom_col(mapping = aes(x = as.Date(date), y = green),position = position_dodge(width=0.2), size=0.25,colour = 'black', fill ='dark green') +
  geom_col(mapping = aes (x = as.Date(date), y = grey), position = position_dodge(width = 0.2), size = 0.25, colour = 'black', fill = 'grey') +
  #Limits the size of the graph.
  coord_cartesian(ylim=c(-100,160)) +
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
  ylab("Visit changes for parks (%) relative to per-weekday winter baselines \n(Google Community Mobility data)")+
  #Add a title for the district data this graph represents.
  ggtitle("Predicted")
District_graph
}

##To test to see if the plot works##
#plot.forecast(forecast = forecast)
