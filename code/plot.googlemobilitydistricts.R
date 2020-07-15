#DEFAULT SETTINGs TO PARKS and BEDFORD (Data stil needs to be unputted)

plot.googlemobilitydistricts<-function(google,type="parks",district="Bedford"){
  
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
  Loc<-paste(type,"_percent_change_from_baseline",sep = "")
  #Data
  Data<-read.csv(google)
  #Subsetting by colour
  Data<-add_column(Data,mean_colour =  
                     (ifelse(Data[,Loc] >= 0,"green","grey")))
  #Creates a smaller data set that only corresponds to the values needed.
  district_data<-subset(Data,Data$sub_region_1 == district,select = c("date","sub_region_1",Loc,"mean_colour"))
  
  #Subset for positive values
  Data_green<-subset(district_data, mean_colour == "green" )
  #Subset for negative values
  Data_grey<-subset(district_data, mean_colour == "grey")
  #Cleans the type title to ensure that _ do not exist in the y axis and recreates the y-axis.
  country_ylab<-paste("Visit changes for",gsub("_"," ",type),"(%) relative to per-weekday winter baselines \n(Google Community Mobility data)")
  
  #START OF THE PLOTTING FUNCTION
  District_graph<-ggplot() +
    #Plots the bar graphs, with a black outing and dark orange fill. 
    geom_col(Data_green, mapping = aes(x = as.Date(date), y = parks_percent_change_from_baseline),position = position_dodge(width=0.2), size=0.25,colour = 'black', fill ='dark green') +
    geom_col(Data_grey, mapping = aes (x = as.Date(date), y = parks_percent_change_from_baseline), position = position_dodge(width = 0.2), size = 0.25, colour = 'black', fill = 'grey') +
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
    ylab(country_ylab)+
    #Add a title for the district data this graph represents.
    ggtitle(district)
  District_graph
}

#example implementation
#plot.googlemobilitydistricts('input_data/testdata/google_england.csv')
