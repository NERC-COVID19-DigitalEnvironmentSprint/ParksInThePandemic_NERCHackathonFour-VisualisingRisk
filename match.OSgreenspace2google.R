########## FUNCTION FOR PROCESSING OS GREENSPACE AND GARDENS DATA FOR USE WITH GOOGLE DATA ###########

match.OSgreenspace2google <- function(){

##### LOADING REQUIRED PACKAGES #####

require(tidyverse)
require(readxl)
require(httr)

##### READING IN DATA #####

# read in gardens data

GET("https://www.ons.gov.uk/file?uri=%2feconomy%2fenvironmentalaccounts%2fdatasets%2faccesstogardensandpublicgreenspaceingreatbritain%2faccesstogardenspacegreatbritainapril2020/osprivateoutdoorspacereferencetables.xlsx",
    write_disk(tf1 <- tempfile(fileext = ".xlsx")))

os_gardens <- read_excel(tf1, sheet=4L, skip = 1)

# read in greenspace data

GET("https://www.ons.gov.uk/file?uri=%2feconomy%2fenvironmentalaccounts%2fdatasets%2faccesstogardensandpublicgreenspaceingreatbritain%2faccesstopublicparksandplayingfieldsgreatbritainapril2020/ospublicgreenspacereferencetables.xlsx",
    write_disk(tf2 <- tempfile(fileext = ".xlsx")))

os_parks <- read_excel(tf2, sheet=6L)

# remove temporary files

rm(tf1, tf2)

##### TIDYING DATA #####

# tidy gardens data

glimpse(os_gardens) # it's a mess - but column 6 is LA name, and the latter 5 refer to total properties (not just a property type), so we need to keep these
os_gardens <- os_gardens %>%
  select(LAD=...6,
         address_count=`Address count...20`,
         address_w_private_outdoor_space_count = `Adress with private outdoor space count...21`,
         total_area_private_outdoor_space_m2 = `Private outdoor space total area (m2)...22`,
         percent_addresses_w_private_outdoor_space = `Percentage of adresses with private outdoor space...23`,
         average_size_private_outdoor_space_m2 = `Average size of private outdoor space (m2)...24`)
glimpse(os_gardens)

# tidy parks and parks/fields data

glimpse(os_parks) # easier to tidy
os_parks <- os_parks %>%
  select(LAD=`LAD name`,
         avg_dist_nearest_public_green_m=`Average distance to nearest Park, Public Garden, or Playing Field (m)`,
         avg_size_nearest_public_green_m2=`Average size of nearest Park, Public Garden, or Playing Field (m2)`,
         avg_count_public_green_1000m_radius=`Average number of  Parks, Public Gardens, or Playing Fields within 1,000 m radius`,
         avg_combined_size_public_green_1000m_radius_m2=`Average combined size of  Parks, Public Gardens, or Playing Fields within 1,000 m radius (m2)`)
glimpse(os_parks)

##### MERGING DATA #####

# testing a merge

left_join(os_gardens, os_parks, by="LAD") # seems to be a mismatch in terms of number of rows. 374 in each individually, but 380 when combined

# investigating discrepanices
  
os_parks %>% filter(is.na(LAD)) # 3 NA rows
os_gardens %>% filter(is.na(LAD)) # another 3 NA rows
  # so the join looks to be combining all possibilities of these - best get rid before joining
  
# ridding trailing NA lines
  
os_parks <- os_parks %>% filter(!is.na(LAD))
os_gardens <- os_gardens %>% filter(!is.na(LAD))
  
# testing the merge again

left_join(os_gardens, os_parks, by="LAD") # 371 looks good
  
# merging
  
os_data <- left_join(os_gardens, os_parks, by="LAD")

# removing original tibbles
  
rm(os_gardens, os_parks)

glimpse(os_data) # looks good

##### APPENDING GOOGLE DISTRICTS #####

# reading in a lookup table of lower tier authorities to upper tier local authorities - these appear to be most similar to google districts

lt_to_ut_lookup <- read_csv("https://opendata.arcgis.com/datasets/85e5bca2a6d549fd80cf3b8fb777a0cd_0.csv")

# filtering to english LTLAs

lt_to_ut_lookup <- lt_to_ut_lookup %>%
  filter(str_detect(LTLA19CD, "^E"))

# tidying this lookup table to just named lt and ut authorities

lt_to_ut_lookup <- lt_to_ut_lookup %>%
  select(ends_with("NM")) %>%
  select(LAD=LTLA19NM, UTLA=UTLA19NM)
glimpse(lt_to_ut_lookup)

# merging with os_data

os_data_v2 <- left_join(os_data, lt_to_ut_lookup, by="LAD")
glimpse(os_data_v2) # notable NAs - these are because they are only for england - we're only interested in england so not a problem

# filtering to only england

os_data_v3 <- os_data_v2 %>%
  filter(!is.na(UTLA))

# checking how many UTLAs we have

fct_count(os_data_v3$UTLA) # 151

# matching google districts to UTLAs - in a similar way to the mene match

england_districts <- c("Bath and North East Somerset","Bedford","Blackburn with Darwen","Blackpool","Borough of Halton","Bracknell Forest","Brighton and Hove","Bristol City","Buckinghamshire","Cambridgeshire","Central Bedfordshire","Cheshire East","Cheshire West and Chester","Cornwall","County Durham","Cumbria","Darlington","Derby","Derbyshire","Devon","Dorset","East Riding of Yorkshire","East Sussex","Essex","Gloucestershire","Greater London","Greater Manchester","Hampshire","Hartlepool","Herefordshire","Hertfordshire","Isle of Wight","Kent","Kingston upon Hull","Lancashire","Leicester","Leicestershire","Lincolnshire","Luton","Medway","Merseyside","Middlesbrough","Milton Keynes","Norfolk","North East Lincolnshire","North Lincolnshire","North Somerset","North Yorkshire","Northamptonshire","Northumberland","Nottingham","Nottinghamshire","Oxfordshire","Peterborough","Plymouth","Portsmouth","Reading","Redcar and Cleveland","Rutland","Shropshire","Slough","Somerset","South Gloucestershire","South Yorkshire","Southampton","Southend-on-Sea","Staffordshire","Stockton-on-Tees","Stoke-on-Trent","Suffolk","Surrey","Swindon","Thurrock","Torbay","Tyne and Wear","Warrington","Warwickshire","West Berkshire","West Midlands","West Sussex","West Yorkshire","Wiltshire","Windsor and Maidenhead","Wokingham","Worcestershire","York")
  # this is from the mene match code

os_data_v4 <- os_data_v3 %>%
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
  # this nested if_else statement was also cribbed from the mene match code

glimpse(os_data_v4)
fct_count(os_data_v4$google_district) # 87 - 1 more than there should be?

# investigating this

levels(as_factor(os_data_v4$google_district)) %in% england_districts # number 20 is the issue
levels(as_factor(os_data_v4$google_district))[20] # Bournemouth, Christchurch and Poole - should be Dorset

# changing bournemouth to dorset

os_data_v5 <- os_data_v4 %>%
  mutate(google_district=fct_recode(google_district, "Dorset"="Bournemouth, Christchurch and Poole"))
levels(os_data_v5$google_district) # 86!

##### AGGREGATING AT GOOGLE DISTRICT LEVEL #####

glimpse(os_data_v5)

os_data_v6 <- os_data_v5 %>%
  group_by(google_district) %>%
  summarise(address_count_new=sum(address_count), # number of addresses in google district
            address_w_private_outdoor_space_count_new = sum(address_w_private_outdoor_space_count), # number of addresses with private outdoor space in google district
            total_area_private_outdoor_space_m2_new = sum(total_area_private_outdoor_space_m2), # total area of private outdoor space in m2 in google district
            average_size_private_outdoor_space_m2_new = weighted.mean(average_size_private_outdoor_space_m2,
                                                                  weights=address_w_private_outdoor_space_count), # average size of private outdoor space in m2 in google district weighted by number of addresses with outdoor private space
            avg_dist_nearest_public_green_m_new = weighted.mean(avg_dist_nearest_public_green_m,
                                                            weights=address_count), # average distance to public greenspace in google district weighted by number of addresses
            avg_count_public_green_1000m_radius_new = weighted.mean(avg_count_public_green_1000m_radius,
                                                                weights=address_count), # average count of public greenspaces in 1000m of a given address weighted by number of addresses
            avg_combined_size_public_green_1000m_radius_m2_new = weighted.mean(avg_combined_size_public_green_1000m_radius_m2,
                                                                           weights=address_count)) %>% # average combined size of public greenspace in 1000m of a given address weighted by number of addresses
  mutate(percent_addresses_w_private_outdoor_space_new = address_w_private_outdoor_space_count_new / address_count_new) %>%
  ungroup() %>%
  select(google_district,
         address_count_new,
         address_w_private_outdoor_space_count_new,
         percent_addresses_w_private_outdoor_space_new,
         everything())

rm(os_data, os_data_v2, os_data_v3, os_data_v4, os_data_v5, england_districts, lt_to_ut_lookup)

os_data_v6

}

########## END OF SCRIPT ##########