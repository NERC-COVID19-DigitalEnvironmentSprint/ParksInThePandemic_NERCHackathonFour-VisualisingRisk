update.forecasts <- function() {
  
  source('getandmatch.forecast.R')
  
  districts<-readRDS('input_data/google_englanddistricts.RDS')
  
  forecasts_england<-c()
  
  for (d in districts){
    currentforecast<-cbind(sub_region_1 = d, getandmatch.forecast(d))
    forecasts_england<-rbind(currentforecast,forecasts_england)
    Sys.sleep(1)
  }
}

#example implementation
#write.csv(forecasts_england,'forecasts_england.csv')
