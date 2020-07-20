########## FUNCTION FOR READING IN MENE DATA, MATCHING TO GOOGLE DISTRICT, ESTIMATING VISIT FREQUENCY, AND NORMALISING BY POPULATION AND GREENSPACE ACCESS ##########

read.menedata <- function(relative2population, relative2greenspace){
  
  ##### LOADING REQUIRED PACKAGES #####
  
  require(tidyverse)
  require(haven)
  require(devtools)
  
  ##### GENERATING AVERAGE ANNUAL ESTIMATES OF VISITS TO THE 'OUTDOORS' (I.E. GREEN SPACES) BY GOOGLE DISTRICT #####
  
  # read in mene - visit file - where each row is a visit rather than a respondent
  
  mene_visit <- read_sav("http://publications.naturalengland.org.uk/file/6746346730291200") 
  
  # create tibble of annual average unweighted and weighted counts of visits
  
  mene_visit2 <- mene_visit %>%
    mutate(new_weight=if_else(year<=7, ConvertedWeekVweight, ConvertedMonthVweight)) %>% 
    # have to do some weights workarounds for weighted data because of changes to question frequency from year 8 onwards
    # these are the correct ones for county (upper tier LA) level according to NE's guidance
    group_by(DESTINATION_UPPERTIER_LOCALAUTHORITY) %>%
    summarise(average_annual_unweighted_count=n()/10,
              average_annual_weighted_count=sum(new_weight)*1000/10) %>% # because all weights should be read as thousands and /10 as 10 years of data
    mutate(DESTINATION_UPPERTIER_LOCALAUTHORITY=as_factor(DESTINATION_UPPERTIER_LOCALAUTHORITY)) %>% # to remove labelled class
    ungroup() %>%
    select(UTLA=DESTINATION_UPPERTIER_LOCALAUTHORITY, average_annual_weighted_count) %>%
    filter(!is.na(UTLA)) %>%
    mutate(UTLA=as.character(UTLA))
  
  # vector of england google districts
  
  england_districts <- c("Bath and North East Somerset","Bedford","Blackburn with Darwen","Blackpool","Borough of Halton","Bracknell Forest","Brighton and Hove","Bristol City","Buckinghamshire","Cambridgeshire","Central Bedfordshire","Cheshire East","Cheshire West and Chester","Cornwall","County Durham","Cumbria","Darlington","Derby","Derbyshire","Devon","Dorset","East Riding of Yorkshire","East Sussex","Essex","Gloucestershire","Greater London","Greater Manchester","Hampshire","Hartlepool","Herefordshire","Hertfordshire","Isle of Wight","Kent","Kingston upon Hull","Lancashire","Leicester","Leicestershire","Lincolnshire","Luton","Medway","Merseyside","Middlesbrough","Milton Keynes","Norfolk","North East Lincolnshire","North Lincolnshire","North Somerset","North Yorkshire","Northamptonshire","Northumberland","Nottingham","Nottinghamshire","Oxfordshire","Peterborough","Plymouth","Portsmouth","Reading","Redcar and Cleveland","Rutland","Shropshire","Slough","Somerset","South Gloucestershire","South Yorkshire","Southampton","Southend-on-Sea","Staffordshire","Stockton-on-Tees","Stoke-on-Trent","Suffolk","Surrey","Swindon","Thurrock","Torbay","Tyne and Wear","Warrington","Warwickshire","West Berkshire","West Midlands","West Sussex","West Yorkshire","Wiltshire","Windsor and Maidenhead","Wokingham","Worcestershire","York")
  
  # matching UTLAs to google districts
  
  mene_visit2 <- mene_visit2 %>%
    mutate(google_district=if_else(UTLA%in%"Bristol, City of","Bristol City",
                                   if_else(UTLA%in%"Halton","Borough of Halton",
                                           if_else(UTLA%in%"Herefordshire, County of","Herefordshire",
                                                   if_else(UTLA%in%"Kingston upon Hull, City of","Kingston upon Hull",
                                                           if_else(UTLA%in%c("Barking and Dagenham","Barnet","Bexley","Brent","Bromley","Camden","City of London","Croydon","Ealing","Enfield","Greenwich","Hackney","Hammersmith and Fulham","Haringey","Harrow","Havering","Hillingdon","Hounslow","Islington","Kensington and Chelsea","Kingston upon Thames","Lambeth","Lewisham","Merton","Newham","Redbridge","Richmond upon Thames","Southwark","Sutton","Tower Hamlets","Waltham Forest","Wandsworth","Westminster"),"Greater London",
                                                                   if_else(UTLA%in%c("Bolton","Bury","Manchester","Oldham","Rochdale","Salford","Tameside","Trafford","Wigan"),"Greater Manchester",
                                                                           if_else(UTLA%in%c("Gateshead","Newcastle upon Tyne","North Tyneside","South Tyneside","Sunderland"),"Tyne and Wear",
                                                                                   if_else(UTLA%in%c("Knowsley","Liverpool","Sefton","St. Helens","Stockport","Wirral"),"Merseyside",
                                                                                           if_else(UTLA%in%c("Bradford","Calderdale","Kirklees","Leeds","Wakefield"),"West Yorkshire",
                                                                                                   if_else(UTLA%in%c("Barnsley","Doncaster","Rotherham","Sheffield"),"South Yorkshire",
                                                                                                           if_else(UTLA%in%c("Birmingham","Coventry","Dudley","Sandwell","Solihull","Telford and Wrekin","Walsall","Wolverhampton"),"West Midlands",
                                                                                                                   if_else(UTLA%in%c("Bournemouth","Poole"),"Dorset",
                                                                                                                           if_else(UTLA%in%c("Isles of Scilly"),"Cornwall",
                                                                                                                                   UTLA))))))))))))))
  
  # checking out the match
  
  fct_count(mene_visit2$google_district) # 86 - all good
  
  # aggregating at google district level
  
  mene_visit3 <- mene_visit2 %>%
    group_by(google_district) %>%
    summarise(estimated_average_annual_visit_count = sum(average_annual_weighted_count))
  
  ##### USING MENE RESPONDENT FILE TO GENERATE ESTIMATES OF ADULT POPULATION BY GOOGLE DISTRCIT FOR NORMALISATION OF VISIT ESTIMATES #####
  
  # reading in the respondent file
  
  mene_resp <- read_sav("http://publications.naturalengland.org.uk/file/5636811710005248")
  
  # aggregating average annual population estimates at upper-tier local authority level
  
  mene_resp2 <- mene_resp %>%
    group_by(RESIDENCE_UPPERTIER_LOCALAUTHORITY) %>%
    summarise(population=sum(WeekWeightANNUAL*1000/10)) %>% # multiplying by 1,000 as all weighted estimates should be read as thousands, and divding by 10 because of ten years of data
    mutate(UTLA=as.character(as_factor(RESIDENCE_UPPERTIER_LOCALAUTHORITY))) %>% # to save as explicit character vector of UTLAs
    select(UTLA, population)
  mene_resp2
  
  # matching to google district - NOTE MAKING SOME EDITS BECAUSE CAPITALISATIONS ARE DIFFERENT IN MENE RESPONDENT FILE FOR SOME REASON
  
  mene_resp2 <- mene_resp2 %>%
    mutate(google_district=if_else(UTLA%in%"Bristol, City of","Bristol City",
                                   if_else(UTLA%in%"Halton","Borough of Halton",
                                           if_else(UTLA%in%"Herefordshire, County of","Herefordshire",
                                                   if_else(UTLA%in%"Kingston Upon Hull, City of","Kingston upon Hull",
                                                           if_else(UTLA%in%c("Barking and Dagenham","Barnet","Bexley","Brent","Bromley","Camden","City of London","Croydon","Ealing","Enfield","Greenwich","Hackney","Hammersmith and Fulham","Haringey","Harrow","Havering","Hillingdon","Hounslow","Islington","Kensington and Chelsea","Kingston Upon Thames","Lambeth","Lewisham","Merton","Newham","Redbridge","Richmond Upon Thames","Southwark","Sutton","Tower Hamlets","Waltham Forest","Wandsworth","Westminster"),"Greater London",
                                                                   if_else(UTLA%in%c("Bolton","Bury","Manchester","Oldham","Rochdale","Salford","Tameside","Trafford","Wigan"),"Greater Manchester",
                                                                           if_else(UTLA%in%c("Gateshead","Newcastle Upon Tyne","North Tyneside","South Tyneside","Sunderland"),"Tyne and Wear",
                                                                                   if_else(UTLA%in%c("Knowsley","Liverpool","Sefton","St. Helens","Stockport","Wirral"),"Merseyside",
                                                                                           if_else(UTLA%in%c("Bradford","Calderdale","Kirklees","Leeds","Wakefield"),"West Yorkshire",
                                                                                                   if_else(UTLA%in%c("Barnsley","Doncaster","Rotherham","Sheffield"),"South Yorkshire",
                                                                                                           if_else(UTLA%in%c("Birmingham","Coventry","Dudley","Sandwell","Solihull","Telford and Wrekin","Walsall","Wolverhampton"),"West Midlands",
                                                                                                                   if_else(UTLA%in%c("Bournemouth","Poole"),"Dorset",
                                                                                                                           if_else(UTLA%in%c("Isles of Scilly"),"Cornwall",
                                                                                                                                   if_else(UTLA%in%"Isle Of Wight", "Isle of Wight",
                                                                                                                                           if_else(UTLA%in%"Blackburn With Darwen", "Blackburn with Darwen",
                                                                                                                                                   if_else(UTLA%in%"Stockton-On-Tees","Stockton-on-Tees",
                                                                                                                                                           if_else(UTLA%in%"Stoke-On-Trent","Stoke-on-Trent",
                                                                                                                                                                   if_else(UTLA%in%"Southend-On-Sea","Southend-on-Sea",
                                                                                                                                                                           UTLA)))))))))))))))))))
  
  # checking out the match
  
  fct_count(mene_resp2$google_district) # 87 - one NA row
  
  # filter out NAs
  
  mene_resp2 <- mene_resp2 %>%
    filter(!is.na(google_district))
  
  # aggregating at google district level
  
  mene_resp3 <- mene_resp2 %>%
    group_by(google_district) %>%
    summarise(population=sum(population))
  
  ##### USING THE OS GREENSPACE TO GOOGLE DISTRICT FUNCTION TO GENERATE GREENSPACE DATA TO NORMALISE BY #####
  
  # reading in the function from our github
  
  source_url("https://github.com/befriendabacterium/parksinthepandemic/blob/develop/code/match.OSgreenspace2google.R?raw=TRUE")
  
  # saving the data as an object
  
  os_greenspace_data <- match.OSgreenspace2google()
  glimpse(os_greenspace_data)
  
  # selecting only the district and average combined size of public greensapce within 1000m radius of a typical address
  
  os_greenspace_data2 <- os_greenspace_data %>%
    select(google_district, avg_combined_size_public_green_1000m_radius_m2_new)
  
  ##### MERGING THE THREE DATASETS TOGETHER #####
  
  mene_merge <- left_join(mene_visit3, mene_resp3, by="google_district") %>% left_join(os_greenspace_data2, by="google_district")
  
  ##### CREATING PER CAPITA VISIT ESTIMATES AND PER CAPITA PER KM2 GREENSPACE ESTIMATES #####
  
  mene_full <- mene_merge %>%
    mutate(annual_visits_per_capita=estimated_average_annual_visit_count/population,
           annual_visits_per_km2_greenspace_1km_radius=estimated_average_annual_visit_count/(avg_combined_size_public_green_1000m_radius_m2_new/1000),
           annual_visits_per_capita_per_km2_greenspace_1km_radius=(estimated_average_annual_visit_count/population)/(avg_combined_size_public_green_1000m_radius_m2_new/1000))
  
  ##### RETURNING DIFFERENT TIBBLES CONTINGENT ON OPTIONS #####
  
  if (relative2population==TRUE & relative2greenspace==TRUE) return(mene_full %>% select(google_district, annual_visits_per_capita_per_km2_greenspace_1km_radius))
  if (relative2population==TRUE & relative2greenspace==FALSE) return(mene_full %>% select(google_district, annual_visits_per_capita))
  if (relative2population==FALSE & relative2greenspace==TRUE) return(mene_full %>% select(google_district, annual_visits_per_km2_greenspace_1km_radius))
  if (relative2population==FALSE & relative2greenspace==FALSE) return(mene_full %>% select(google_district, estimated_average_annual_visit_count))
  
}

# example implementation
# mene <- read.menedata(relative2population=TRUE, relative2greenspace=TRUE)
# write.csv(mene, 'mene_england.csv', row.names=F)