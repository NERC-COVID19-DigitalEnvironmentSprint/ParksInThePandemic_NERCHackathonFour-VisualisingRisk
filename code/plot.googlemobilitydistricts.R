plot.googlemobilitydistricts<-function(google,type="parks",district="Bedford"){
  
  #google = read.csv("input_data/testdata/google_england.csv")
  #type = "parks"
  #district = "Bedford"
  
  # Load packages ----------------------------------------------------------
  #install.packages('tibble')
  library(tibble)
  #install.packages('ggplot2')
  library(ggplot2)
  
  #Creates a character vector that resembles the column name within the google mobility data.
  Loc<-paste(type,"_percent_change_from_baseline",sep = "")
  #Data
  Data<-google
  Data[,"date"]<-as.Date(Data$date)
  
  #Subsetting by colour
  Data<-tibble::add_column(Data,mean_colour =  
                     (ifelse(Data[,Loc] >= 0,"green","grey")))
  #Creates a smaller data set that only corresponds to the values needed.
  district_data<-subset(Data,Data$sub_region_1 == district,select = c("date","sub_region_1",Loc,"mean_colour"))
  
  #Subset for positive values
  Data_green<-subset(district_data, mean_colour == "green" )
  #Subset for negative values
  Data_grey<-subset(district_data, mean_colour == "grey")
  
  Data_not_grey<-cbind.data.frame("date" = Data_green$date,"sub_region_1" = Data_green$sub_region_1,"parks_percent_change_from_baseline" = 0, "mean_colour" = Data_green$mean_colour)
  
  Data_not_green<-cbind.data.frame("date" = Data_grey$date,"sub_region_1" = Data_grey$sub_region_1,"parks_percent_change_from_baseline" = 0, "mean_colour" = Data_grey$mean_colour)
  
  Data_green_all<-rbind(Data_green,Data_not_green)
  
  Data_grey_all<-rbind(Data_grey, Data_not_grey)
  
  #Cleans the type title to ensure that _ do not exist in the y axis and recreates the y-axis.
  country_ylab<-paste("Visit changes for",gsub("_"," ",type),"(%) relative to per-weekday winter baselines \n(Google Community Mobility data)")
  
  #START OF THE PLOTTING FUNCTION
  District_graph<-ggplot2::ggplot() +
    #Plots the bar graphs, with a black outing and dark orange fill. 
    geom_col(Data_green_all, mapping = aes(x = as.Date(date), y = parks_percent_change_from_baseline),position = position_dodge(width=0.2), size=0.25,colour = 'black', fill ='dark green') +
    geom_col(Data_grey_all, mapping = aes (x = as.Date(date), y = parks_percent_change_from_baseline), position = position_dodge(width = 0.2), size = 0.25, colour = 'black', fill = 'grey') +
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