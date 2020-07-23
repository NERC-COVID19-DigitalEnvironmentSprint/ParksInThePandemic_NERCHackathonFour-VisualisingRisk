plot.googlemobilitydistricts<-function(google,type="parks",district="Bedford",rain = F, temp = F){
  
  #google = read.csv("input_data/testdata/google_england.csv")
  #type = "parks"
  #district = "Bedford"
  #rain = F
  #temp = F
  #google = googleandmetoffice
  
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
  #Ensures that infinite values are treated as 0.
  Data$rain_mean[is.infinite(Data$rain_mean)]<-0
  
  #Work out scaling factors
  Scalefactor_postemp<-max(Data$parks_percent_change_from_baseline, na.rm = T)/max(Data$temp_mean, na.rm = T)
  Scalefactor_negtemp<-min(Data$parks_percent_change_from_baseline, na.rm = T)/min(Data$temp_mean, na.rm = T)
  Scalefactor_posrain<-max(Data$parks_percent_change_from_baseline, na.rm = T)/max(Data$rain_mean, na.rm = T)
  Scalefactor_negrain<-min(Data$parks_percent_change_from_baseline, na.rm = T)/min(Data$rain_mean, na.rm = T)
  
  #Subsetting by colour
  Data<-tibble::add_column(Data,mean_colour =  
                             (ifelse(Data[,Loc] >= 0,"darkgreen","grey")))
  #Creates a smaller data set that only corresponds to the values needed.
  district_data<-subset(Data,Data$sub_region_1 == district,select = c("date","sub_region_1",Loc,"mean_colour","temp_mean","rain_mean"))
  
  #Cleans the type title to ensure that _ do not exist in the y axis and recreates the y-axis.
  country_ylab<-paste("Visit changes for",gsub("_"," ",type),"(%) relative to per-weekday winter baselines \n(Google Community Mobility data)")
  
  #Scaling
  district_data$scale_temp<-ifelse( district_data$temp_mean>0,Scalefactor_postemp,Scalefactor_negtemp)
  district_data$scale_rain<-ifelse( district_data$rain_mean>0,Scalefactor_posrain,Scalefactor_negrain)
  
  district_data<- district_data%>%
    mutate(
      temp_mean = temp_mean*scale_temp,
      rain_mean = rain_mean*scale_rain
    )
  
  if(rain == F){
    district_data$rain_mean<-0
  }
  if(temp == F){
    district_data$temp_mean<-0
  }
  
  
  #START OF THE PLOTTING FUNCTION
  District_graph<-ggplot2::ggplot() +
    #Plots the bar graphs, with a black outing and dark orange fill. 
    geom_col(district_data, mapping = aes(x = as.Date(date), y = parks_percent_change_from_baseline),position = position_dodge(width=0.2), size=0.25,colour = "black" , fill =district_data$mean_colour) +
    geom_line(district_data, mapping = aes(x = as.Date(date), y = temp_mean),color = "red", linetype = "dashed")+
    geom_line(district_data, mapping = aes(x = as.Date(date), y = rain_mean),color = "blue", linetype = "dashed")+
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