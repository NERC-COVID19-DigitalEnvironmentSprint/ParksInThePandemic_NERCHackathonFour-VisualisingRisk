#Argument data, defines the dataset you want to convert to percentage baseline change.
#example implementation: relative2baseline('metoffice_england.csv')
#where 'metoffice_england.csv' is the output of the match.metoffice function (met office data matched to Google and with columns renamed in line with OpenWeather forecast format)
relative2baseline<-function(data){
  
  data = forecasts_england
  #Find the non-baseline values  -----------------------------------------
  
  nonbaselineperiod<-subset(data,data$date >= "2020-02-15")
  
  #Read in the metoffice baseline value database.
  baselineweather<-readRDS("code/input_data/metofficebaseline_weekday.RDS")
  
  # For loop PREPARATION ----------------------------------------------------
  #make a vector of weekdays
  wdays<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  
  #make vector of districts
  districts<-readRDS("code/input_data/google_englanddistricts.RDS")
  
  #make a vector of the column names
  baseline_name<-c("temp_max","temp_mean","temp_min","rain_mean")
  
  #copy df to new df which will be relative to baseline df
  rel2baseline<-nonbaselineperiod
  
  #This loop calculates the 'baseline change in weather' using Google's method applied to the Met Office data. Process:
  #1. Go through each weekday (first would be Monday)
  #2. Go through each Google district (first would be Bath and Northeast Somerset)
  #3. Go through each of the meteorological variables in columns 4:53 in the Met Office dataframe (first would be air temp mean max K)
  #4. Nonbaseline - A temporary object - in first instance, would be all the Met Office air temp mean max K measurements from all the Mondays from Bath and Northeast Somerset in the non-baseline time series
  #5. Baseline - A temporary object - in first instance, would be all the Met Office air temp mean max K measurements from all the Mondays from Bath and Northeast Somerset in the baseline time series
  #6. Diff_from_baseline - A temporary obect - Subtract baseline from nonbaseline to get absolute change in that variable per weekday per district (as in Google methods)
  #7. Write to the relative change dataframe in the appropriate columns - Make the difference relative by dividing it by the baseline and multiplying by 100 to get in percent as for Google data
  
  
  #loop through weekdays
  for (w in wdays){
    #loop through districts
    for(d in districts){
      #loop through columns
      for (c in baseline_name){
        
        #for weekday w and district w, get non-baseline median values of meteorological measurement c for
        nonbaseline<-nonbaselineperiod[nonbaselineperiod$weekday==w & nonbaselineperiod$sub_region_1==d,c]
        
        #for weekday w and district w, get baseline median values of meteorological measurement c
        baseline<-baselineweather[baselineweather$weekday==w & baselineweather$sub_region_1==d,c]
        
        #subtract the baseline values of meteorological measurement c for weekday w and district w
        diff_from_baseline<-nonbaseline-baseline
        
        #object is the 'nonbaseline rows in the new df (rel2baseline)
        #function is to divide the difference from baseline by the baseline to get relative difference the times by 100 to get in percent
        rel2baseline[nonbaselineperiod$weekday==w & nonbaselineperiod$sub_region_1==d,c]<-diff_from_baseline/baseline*100
      
    }
  }
}
  rel2baseline
}
