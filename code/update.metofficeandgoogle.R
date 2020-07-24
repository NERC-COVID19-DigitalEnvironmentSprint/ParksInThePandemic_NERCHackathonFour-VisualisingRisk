update.metofficeandgoogle <- function() {
  
  # INITIALISE OSF ------------------------------------------------------
  
  library(osfr)
  # Authenticate with full access token
  osf_auth(readLines("./../../osfauth.txt", n=1))
  
  # READ IN GOOGLE DATA & WRITE TO OSF --------------------------------------------------------
  
  source('read.googlemobility.R')
  #google community mobility reports
  google<-read.googlemobility()
  google_england<-subset(google,sub_country=='England')
  write.csv(google_england, 'input_data/testdata/google_england.csv', row.names = F)
  
  # Get tibble of data on the OSF store
  pp_project <- osf_ls_files(osf_retrieve_node("c7kg4"))
  # Get latest temporal directory
  temporal<-osf_ls_files(pp_project[which(pp_project$name=='temporal'),])
  temporal
  #remove existing google_england file (workaround as overwrite doesn't work right now)
  osf_rm(temporal[which(temporal$name=='google_england.csv'),], check=FALSE)
  #write new version in it's place
  osf_upload(pp_project[which(pp_project$name=='temporal'),],
             'input_data/testdata/google_england.csv')
  
  # READ IN MET OFFICE DATA, MATCH TO GOOGLE AND WRITE TO OSF -------------------------------------------------
  
  #read
  source('read.metofficecovid.R')
  #met office covid data
  metoffice_england<-read.metofficecovid()
  
  #match
  source('match.metoffice.R')
  #match met office with google data and rename columns to fit OpenWeather forecast format
  metoffice_england<-match.metoffice(metoffice_england,google_england)
  write.csv(metoffice_england, 'input_data/testdata/metoffice_england.csv', row.names=F)
  
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
             'input_data/testdata/metoffice_england.csv')
  
  # MAKE WEATHER DATA RELATIVE TO BASELINE AND MERGE WITH GOOGLE ----------------------------------
  source('relative2baseline.R')
  metoffice_england_r2b<-relative2baseline(metoffice_england)
  googleandmetoffice_england<-merge(google_england,metoffice_england_r2b, all=T)
  write.csv(googleandmetoffice_england, 'input_data/testdata/googleandmetoffice_england.csv')
  
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
             'input_data/testdata/googleandmetoffice_england.csv')



}
