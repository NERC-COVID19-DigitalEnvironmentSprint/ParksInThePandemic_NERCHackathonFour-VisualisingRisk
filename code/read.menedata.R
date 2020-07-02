##### FUNCTION TO DOWNLOAD MENE DATA AND CREATE A TIBBLE OF UPPER TIER LA's WITH AVERAGE ANNUAL VISIT COUNTS TO THEM #####

read.menedata <- function(){
  require(tidyverse)
  require(haven)
  
  # read in mene - visit file - where each row is a visit rather than a respondent
  
  mene <- read_sav("http://publications.naturalengland.org.uk/file/6746346730291200") 
  
  # create tibble of annual average unweighted and weighted counts of visits
  
  mene %>%
    mutate(new_weight=if_else(year<=7, ConvertedWeekVweight, ConvertedMonthVweight)) %>% 
    # have to do some weights workarounds for weighted data because of changes to question frequency from year 8 onwards
    # these are the correct ones for county (upper tier LA) level according to NE's guidance
    group_by(DESTINATION_UPPERTIER_LOCALAUTHORITY) %>%
    summarise(average_annual_unweighted_count=n()/10,
              average_annual_weighted_count=sum(new_weight)*1000/10) %>% # because all weights should be read as thousands and /10 as 10 years of data
    mutate(DESTINATION_UPPERTIER_LOCALAUTHORITY=as_factor(DESTINATION_UPPERTIER_LOCALAUTHORITY)) %>% # to remove labelled class
    ungroup()
}

##### END #####