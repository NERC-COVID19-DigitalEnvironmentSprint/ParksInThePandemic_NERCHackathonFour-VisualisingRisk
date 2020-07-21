update.OSFinputs <- function() {
  
  # INITIALISE OSF ------------------------------------------------------
  
  library(osfr)
  # Authenticate with full access token
  osf_auth(readLines("./../../osfauth.txt", n=1))
  
  # READ IN GOOGLE DATA & WRITE TO OSF --------------------------------------------------------
  #make temporary directory
  dir.create("./temporary")
  
  source('read.googlemobility.R')
  #google community mobility reports
  google<-read.googlemobility()
  google_england<-subset(google,sub_country=='England')
  write.csv(google_england, 'temporary/google_england.csv', row.names = F)
  
  # Get tibble of data on the OSF store
  pp_project <- osf_ls_files(osf_retrieve_node("c7kg4"))
  # Get latest temporal directory
  temporal<-osf_ls_files(pp_project[which(pp_project$name=='temporal'),])
  temporal
  #remove existing google_england file (workaround as overwrite doesn't work right now)
  osf_rm(temporal[which(temporal$name=='google_england.csv'),], check=FALSE)
  #write new version in it's place
  osf_upload(pp_project[which(pp_project$name=='temporal'),],
             'temporary/google_england.csv')
  
  # READ IN MET OFFICE DATA, MATCH TO GOOGLE AND WRITE TO OSF -------------------------------------------------
  
  #read
  source('read.metofficecovid.R')
  #met office covid data
  metoffice_england<-read.metofficecovid()
  
  #match
  source('match.metoffice.R')
  #match met office with google data and rename columns to fit OpenWeather forecast format
  metoffice_england<-match.metoffice(metoffice_england,google_england)
  write.csv(metoffice_england, 'temporary/metoffice_england.csv', row.names=F)
  
  #write
  # Get tibble of data on the OSF store
  pp_project <- osf_ls_files(osf_retrieve_node("c7kg4"))
  # Get latest temporal directory
  temporal<-osf_ls_files(pp_project[which(pp_project$name=='temporal'),])
  temporal
  #remove existing google_england file (workaround as overwrite doesn't work right now)
  osf_rm(temporal[which(temporal$name=='metoffice_england.csv'),], check=FALSE)
  #write new version in it's place
  osf_upload(pp_project[which(pp_project$name=='temporal'),],
             'temporary/metoffice_england.csv')
  
  # MAKE WEATHER DATA RELATIVE TO BASELINE AND MERGE WITH GOOGLE ----------------------------------
  source('relative2baseline.R')
  metoffice_england_r2b<-relative2baseline(metoffice_england)
  googleandmetoffice_england<-merge(google_england,metoffice_england_r2b, all=T)
  write.csv(googleandmetoffice_england, 'temporary/googleandmetoffice_england.csv')
  
  #write
  # Get tibble of data on the OSF store
  pp_project <- osf_ls_files(osf_retrieve_node("c7kg4"))
  # Get latest temporal directory
  temporal<-osf_ls_files(pp_project[which(pp_project$name=='temporal'),])
  temporal
  #remove existing google_england file (workaround as overwrite doesn't work right now)
  osf_rm(temporal[which(temporal$name=='googleandmetoffice_england.csv'),], check=FALSE)
  
  #write new version in it's place
  osf_upload(pp_project[which(pp_project$name=='temporal'),],
             'temporary/googleandmetoffice_england.csv')


  #delete temporary directory
  unlink("./temporary", recursive=T)
  
  # DOWNLOAD ALL OF THE UPLOADED DATA & UPDATE THE MODEL ------------------------------------------------------------
  
  #make data directory
  dir.create("./data")
  
  #download latest data just uploaded to OSF
  pp_project <- osf_ls_files(osf_retrieve_node("c7kg4"))
  osf_download(pp_project[which(pp_project$name%in%c('temporal','singlemeasure', 'model')),], path="./data", verbose=TRUE, progress=TRUE, recurse=TRUE, conflicts="overwrite")
  
  source('create.model.R')
  
  #read in freshly downloaded model inputs
  googleandmetoffice_england<-read.csv('data/temporal/googleandmetoffice_england.csv')
  mene_england<-read.csv('data/singlemeasure/mene_england.csv')
  gardenaccess_england<-read.csv('data/singlemeasure/gardenaccess_england.csv')
  
  #rerun model
  RF_model<-create.model(googleandmetoffice_england, mene_england, gardenaccess_england)
  #save model locally
  saveRDS(RF_model,'data/model/RF_model.RDS')
  
  # Get latest model directory
  model<-osf_ls_files(pp_project[which(pp_project$name=='model'),])
  model
  #remove existing google_england file (workaround as overwrite doesn't work right now)
  osf_rm(model[which(model$name=='RF_model.RDS'),], check=FALSE)
  
  #write new version in it's place
  osf_upload(pp_project[which(pp_project$name=='model'),],
             'data/model/RF_model.RDS')
  

  # UPDATE FORECASTS --------------------------------------------------------

  source('getandmatch.forecast.R')
  
  districts<-readRDS('input_data/google_englanddistricts.RDS')
  
  forecasts_england<-c()
  
  #loops through each district to get forecasts, pausing for 1 second in between each one so doesn't overload OpenWeather API
  for (d in districts){
    currentforecast<-cbind(sub_region_1 = d, getandmatch.forecast(d))
    forecasts_england<-rbind(currentforecast,forecasts_england)
    Sys.sleep(1)
  }
  
  #write forecasts to local directory
  write.csv(forecasts_england,'data/model/forecasts_england.csv')
  
  # Get latest model directory
  model<-osf_ls_files(pp_project[which(pp_project$name=='model'),])
  model
  
  #remove existing google_england file (workaround as overwrite doesn't work right now)
  osf_rm(model[which(model$name=='forecasts_england.csv'),], check=FALSE)
  
  #write new version in it's place
  osf_upload(pp_project[which(pp_project$name=='model'),],
             'data/model/forecasts_england.csv')
  
}

#example implementation
update.OSFinputs()
