create.model<-function(){
  
  #google = read.csv("input_data/testdata/googleandmetoffice_england.csv")
  #mene_england = read.csv("input_data/testdata/mene_england.csv")
  #garden_access = read.csv("input_data/testdata/garden_access.csv")
  
  # Load packages ----------------------------------------------------------
  #install.packages('tibble')
  library(tibble)
  
  
  # GET FORECAST FOR THE DAY UNDER CONSIDERATION AND PLOT PREDICTED VISITS BASED ON IT ----------------------------
  
  
  #Subsets out the google_metoffice data to only have the relevant factors.
  google_metoffice<-subset(google, select = c("parks_percent_change_from_baseline",
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
  #Removes all NA data for analysis. 
  google_metoffice_na<-na.omit(google_metoffice)
  #Remove district column
  google_metoffice_na<-google_metoffice_na
  
  #Add the garden access into google_metoffice_na dataset
  garden_access_2<-as.data.frame(garden_access[,2][match(google_metoffice_na$sub_region_1,garden_access$google_district)])
  
  for (i in 3:length(garden_access)) {
    garden_access_3<-as.data.frame(garden_access[,i][match(google_metoffice_na$sub_region_1,garden_access$google_district)])
    garden_access_2<-cbind(garden_access_2,garden_access_3[,1])
    }
  colnames(garden_access_2)<-c(colnames(garden_access[2:9]))
  
  #Add the mene data into google_metoffice_na dataset
  mene_england_2<-as.data.frame(mene_england[,2][match(google_metoffice_na$sub_region_1,mene_england$sub_region_1)])
  
  for (i in 3:length(mene_england)) {
    mene_england_3<-as.data.frame(mene_england[,i][match(google_metoffice_na$sub_region_1,mene_england$sub_region_1)])
    mene_england_2<-cbind(mene_england_2,mene_england_3[,1])
  }
  colnames(mene_england_2)<-c(colnames(mene_england[2:5]))
  
  #Creates a dataset that combines the metoffice na dataset, the mene and the garden_access datatset.
  model_data<-cbind(google_metoffice_na,mene_england_2,garden_access_2)[,-2]
  
  # Using a model to predict values -----------------------------------------
  #Adjusting the garden_access data
  garden_access_name<-colnames(garden_access_2)[-2]
  
  #This code generates the model used to produce the predictor values. 
  model_data<-subset(model_data, select = c(colnames(google_metoffice_na[-2]),garden_access_name[-3], "annual_visits_per_capita_per_km2_greenspace_1km_radius"))
  
  set.seed(1234)
  #RF_model_old<-randomForest::randomForest(model_data_1[,-1],model_data_1$parks_percent_change_from_baseline)
  RF_model<-randomForest::randomForest(model_data[,-1],model_data$parks_percent_change_from_baseline)
 }

#saveRDS(RF_model,"input_data/RF_model.RDS")
