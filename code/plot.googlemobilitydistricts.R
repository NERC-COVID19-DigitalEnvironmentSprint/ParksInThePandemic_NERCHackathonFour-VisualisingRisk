
#DEFAULT SETTINGs TO PARKS and BEDFORD (Data stil needs to be unputted)

plot.googlemobilitydistricts<-function(Data,location="parks",district="Bedford"){
  
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
  #Creates a smaller data set that only corresponds to the values needed.
  district_data<-subset(Data,select = c("date","sub_region_1",Loc))
  #Makes many data frames for each district.
  district_all<-split(district_data,district_data$sub_region_1)
  #Makes a data frame of the single district required.
  district_df<-as.data.frame(district_all[district])
  #Changes the column names for making it easier to put into a plotting function.
  colnames(district_df)<-c("Date","District","Mobility")
  #Cleans the location title to ensure that _ do not exist in the y axis and recreates the y-axis.
  country_ylab<-paste("Visit changes for",gsub("_"," ",location),"(%) relative to per-weekday winter baselines (Google Community Mobility data)")
  #START OF THE PLOTTING FUNCTION 
  District_graph<-ggplot(data=district_df,aes(x=as.Date(Date),y=Mobility)) +
    #Plots the bar graphs, with a black outing and dark orange fill. 
    geom_col(position = position_dodge(width=0.2), size=0.25,colour = 'black', fill ='#D55E00') +
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
    ylab(country_ylab)+
    #Add a title for the district data this graph represents.
    ggtitle(district)
  District_graph
}
