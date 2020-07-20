##Code for a function that reads in the MENE data, assigns google districts and aggregates the weighted and unweighted accounts according to the google districts.##

match.mene2google<-function(mene_df,google_df){
 
  # Load packages ----------------------------------------------------------
  #install.packages('tibble')
  library(tibble)
  
  
  #Creates a character vector of the English districts according to the Google mobility data.
  England_districts<-c("Bath and North East Somerset","Bedford","Blackburn with Darwen","Blackpool","Borough of Halton","Bracknell Forest","Brighton and Hove","Bristol City","Buckinghamshire","Cambridgeshire","Central Bedfordshire","Cheshire East","Cheshire West and Chester","Cornwall","County Durham","Cumbria","Darlington","Derby","Derbyshire","Devon","Dorset","East Riding of Yorkshire","East Sussex","Essex","Gloucestershire","Greater London","Greater Manchester","Hampshire","Hartlepool","Herefordshire","Hertfordshire","Isle of Wight","Kent","Kingston upon Hull","Lancashire","Leicester","Leicestershire","Lincolnshire","Luton","Medway","Merseyside","Middlesbrough","Milton Keynes","Norfolk","North East Lincolnshire","North Lincolnshire","North Somerset","North Yorkshire","Northamptonshire","Northumberland","Nottingham","Nottinghamshire","Oxfordshire","Peterborough","Plymouth","Portsmouth","Reading","Redcar and Cleveland","Rutland","Shropshire","Slough","Somerset","South Gloucestershire","South Yorkshire","Southampton","Southend-on-Sea","Staffordshire","Stockton-on-Tees","Stoke-on-Trent","Suffolk","Surrey","Swindon","Thurrock","Torbay","Tyne and Wear","Warrington","Warwickshire","West Berkshire","West Midlands","West Sussex","West Yorkshire","Wiltshire","Windsor and Maidenhead","Wokingham","Worcestershire","York")
  #Reads in the menedata set.
  MENE_data<-read.csv(mene_df)
  #Extracts the MENE districts (151 of them) and ensure this is a character vector for manipulation.
  MENE_districts<-as.character(MENE_data$DESTINATION_UPPERTIER_LOCALAUTHORITY)
  #Creates a converted table to include the newly formed MENE_districts,their average annual unweighted and weighted count.
  MENE_conversion<-as.data.frame(t(rbind(MENE_districts,MENE_data$average_annual_unweighted_count, MENE_data$average_annual_weighted_count)))
  #Changes the column names to a more suitable name for analysis.
  colnames(MENE_conversion)<-c("MENE_localauthority","average_annual_unweighted_counts","average_annual_weighted_count")
  #Creates a vector according to the MENE_districts and converts initially all the districts that are named slightly differently, then converts all districts that are a subset of a bigger districts according to google mobility data, then sets the rest of the districts to the same value as MENE_districts that were unchanged.
  Google_localauthority<-ifelse(MENE_conversion$MENE_localauthority%in%"Bristol, City of","Bristol City",
                              ifelse(MENE_conversion$MENE_localauthority%in%"Halton","Borough of Halton",
                                     ifelse(MENE_conversion$MENE_localauthority%in%"Herefordshire, County of","Herefordshire",
                                           ifelse(MENE_conversion$MENE_localauthority%in%"Kingston upon Hull, City of","Kingston upon Hull",
                                                  ifelse(MENE_conversion$MENE_localauthority%in%c("Barking and Dagenham","Barnet","Bexley","Brent","Bromley","Camden","City of London","Croydon","Ealing","Enfield","Greenwich","Hackney","Hammersmith and Fulham","Haringey","Harrow","Havering","Hillingdon","Hounslow","Islington","Kensington and Chelsea","Kingston upon Thames","Lambeth","Lewisham","Merton","Newham","Redbridge","Richmond upon Thames","Southwark","Sutton","Tower Hamlets","Waltham Forest","Wandsworth","Westminster"),"Greater London",
                                                         ifelse(MENE_conversion$MENE_localauthority%in%c("Bolton","Bury","Manchester","Oldham","Rochdale","Salford","Tameside","Trafford","Wigan"),"Greater Manchester",
                                                                ifelse(MENE_conversion$MENE_localauthority%in%c("Gateshead","Newcastle upon Tyne","North Tyneside","South Tyneside","Sunderland"),"Tyne and Wear",
                                                                       ifelse(MENE_conversion$MENE_localauthority%in%c("Knowsley","Liverpool","Sefton","St. Helens","Stockport","Wirral"),"Merseyside",
                                                                              ifelse(MENE_conversion$MENE_localauthority%in%c("Bradford","Calderdale","Kirklees","Leeds","Wakefield"),"West Yorkshire",
                                                                                     ifelse(MENE_conversion$MENE_localauthority%in%c("Barnsley","Doncaster","Rotherham","Sheffield"),"South Yorkshire",
                                                                                            ifelse(MENE_conversion$MENE_localauthority%in%c("Birmingham","Coventry","Dudley","Sandwell","Solihull","Telford and Wrekin","Walsall","Wolverhampton"),"West Midlands",
                                                                                                   ifelse(MENE_conversion$MENE_localauthority%in%c("Bournemouth","Poole"),"Dorset",
                                                                                                          ifelse(MENE_conversion$MENE_localauthority%in%c("Isles of Scilly"),"Cornwall",
                                              MENE_conversion$MENE_localauthority)))))))))))))
  #Adds the Google local authority column to the conversion data frame for analysis.
  MENE_conversion<-MENE_conversion%>% tibble::add_column(Google_localauthority = Google_localauthority, .after = which(colnames(MENE_conversion)=="MENE_localauthority"))
  #Removes the last row that contains NA values.
  MENE_conversion<-MENE_conversion[-152,]
  #Ensures that both the unweighted AND weighted counts are treated as numeric variables.
  MENE_conversion$average_annual_unweighted_counts<-as.numeric(MENE_conversion$average_annual_unweighted_counts)
  MENE_conversion$average_annual_weighted_count<-as.numeric(MENE_conversion$average_annual_weighted_count)
  #Creates a new data frame that aggregates each of the unweighted and weighted counts according to the google districts.
  MENE_data_reformed<-cbind.data.frame(tapply(MENE_conversion$average_annual_unweighted_counts, MENE_conversion$Google_localauthority,mean, na.rm = TRUE),
                                       tapply(MENE_conversion$average_annual_weighted_count, MENE_conversion$Google_localauthority,mean, na.rm = TRUE))
  #Changes the column names to something more readable/presentable.
  colnames(MENE_data_reformed)<-c("average_annual_unweighted_counts","average_annual_weighted_count")
  #Copies the Google_localauthorities from the rownames into a new devised column.
  MENE_data_reformed<-tibble::rownames_to_column(MENE_data_reformed, var = "Google_localauthority" )
}

#example implementation
#mene_england<-match.mene2google('mene_england.csv')
#write.csv(mene_england,'mene_england.csv', row.names=F)