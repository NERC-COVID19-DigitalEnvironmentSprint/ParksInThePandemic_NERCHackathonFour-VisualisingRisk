#This function plots bar graphs from the mobility data

#To RUN THE FUNCTION (## = Notes, # = code to run beforehand)
##Acquires the data set for the function

##This outlines the functions options (##=Notes/ function title, # = options )

##location (Needs to be in "")
#parks
#grocery_and_pharmacy
#retail_and_recreation
#transit_stations
#workplaces
#residential
##country
#England
#N_ireland
#Scotland
#Wales

#EXAMPLE WORKFLOW FROM THE PARKSINTHEPANDEMIC DIRECTORY

##get the report##

#to download and use the latest dataset
#source('code/read.googlemobility.R')
#google<-read.googlemobility()
#OR...
#to use previously downloaded dataset
#google<-read.csv('data/google/GoogleMobilityReport.csv')


##plot the report##
#source('code/plot.googlemobility.R')
#plot.googlemobility(google, 'parks','England')


plot.googlemobility<-function(Data,location="parks",country){
  
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
  #install.packages('plyr')
  library(plyr)
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
  
  
  #Creates a character vector that resembles the column name within the google mobility data.
  Loc<-paste(location,"_percent_change_from_baseline",sep = "")
  #Works out the mean value for each date and country.
  UK_mean<-as.data.frame(t(tapply(Data[,Loc], list(Data$sub_country,Data$date),mean, na.rm=T)))
  #Ensure the date column is within the dataset
  UK_mean<-rownames_to_column(UK_mean,var="Date")
  #Converts the date column to a date classification for analysis
  UK_mean<-mutate(UK_mean, Date = as.Date(Date))
  #Changes the column names for making it easier to put into a plotting function.
  colnames(UK_mean)<-c("Date","England_mean","N_ireland_mean","Scotland_mean","Wales_mean")
  #Works out the standard error (SE) valueS for each date and country.
  UK_SE<-as.data.frame(t(tapply(Data[,Loc], list(Data$sub_country,Data$date),std.error, na.rm=T)))
  #ChaNges the column names for making it easier to put into a plotting function.
  colnames(UK_SE)<-c("England_se","N_ireland_se","Scotland_se","Wales_se")
  #Binds both the mean values and SE values into one dataset. 
  UK_all<-cbind.data.frame(UK_mean,UK_SE)
  #Takes out the set of values corresponding to the appropriate countries mean defined by the function.
  country_mean_v<-UK_all[,paste(country,"_mean",sep = "")]
  #Takes out the set of values corresponding to the appropriate countries SE defined by the function
  country_se_v<-UK_all[,paste(country,"_se",sep = "")]
  #Cleans the location title to ensure that _ do not exist in the y axis and recreates the y-axis.
  country_ylab<-paste("Visit changes for",gsub("_"," ",location),"(%) relative to per-weekday winter baselines (Google Community Mobility data)")
  #START OF THE PLOTTING FUNCTION 
  England_graph<-ggplot(data=UK_all,aes(x=as.Date(Date),y=country_mean_v)) +
    #Plots the bar graphs, with a black outing and dark orange fill. 
    geom_col(position = position_dodge(width=0.2), size=0.25,colour = 'black', fill ='#D55E00') +
    #Plots the standard erorr bars for each bar graph.
    geom_errorbar(aes(ymin=country_mean_v - country_se_v,
                      ymax=country_mean_v + country_se_v), 
                  width=0, size=0.25, position = position_dodge(width=0.9)) +
    #Limits the size of the graph.
    coord_cartesian(ylim=c(-100,160)) +
    #plots a horizontal line where no percentage change occurs.
    geom_hline(yintercept=0) + 
    #Ensure the background is white, the border is black and removes grid lines.
    theme(panel.background = element_rect(fill = "white", colour = "black", size = 1, linetype = "solid"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          strip.text = element_blank())+
    #x-label
    xlab("Date") +
    #y-label using the previous clean code done outside the plot.
    ylab(country_ylab) 
    England_graph
}
