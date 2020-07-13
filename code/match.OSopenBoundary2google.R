library(sf)
library(stringr)

# read in OSopen boundary-line (https://www.ordnancesurvey.co.uk/business-government/products/boundaryline) Open Government Licence Contains OS data © Crown copyright and database right 2020
#ENG_district <- st_read("GB/district_borough_unitary_region.shp")
#ENG_county <- st_read("GB/county_region.shp")
#ENG_cer_county <- st_read("Supplementary_Ceremonial/Boundary-line-ceremonial-counties_region.shp")

source('read.googlemobility.R')
#read in google data
mobility<-read.googlemobility()
#subset out only England data from the Google dataframe
mobility_england<-subset(mobility,sub_country=='England')

# subset and clean region names in mobility data
England_districts<- mobility_england$sub_region_1
England_districts_CleanName <- str_remove_all(England_districts,coll(" City"))
England_districts_CleanName <- str_remove_all(England_districts_CleanName,coll("Borough of "))
England_districts <- data.frame(Mobility_name=England_districts, CleanName= England_districts_CleanName)

# clean OS district names and match
ENG_district$NAMEclean <- str_remove_all(ENG_district$NAME,coll(" (B)"))
ENG_district$NAMEclean <- str_remove_all(ENG_district$NAMEclean,coll(" District"))
ENG_district$NAMEclean <- str_remove_all(ENG_district$NAMEclean,coll("City of "))
ENG_district$NAMEclean <- str_remove_all(ENG_district$NAMEclean,coll("The "))
match_district <- subset(ENG_district,ENG_district$NAMEclean %in% England_districts$CleanName)
match_district <- subset(match_district, !(match_district$NAMEclean =="Shropshire")) # remove as used at county level in mobility data

# get the mobility regions that don't match OS districts
unmatched <- setdiff(England_districts$CleanName, match_district$NAMEclean)

# clean OS county data
ENG_county$NAMEclean <- str_remove_all(ENG_county$NAME,coll(" County"))
ENG_county$NAMEclean <- str_remove_all(ENG_county$NAMEclean,coll(" Authority"))

# get mobility regions that match OS county names
match_county <- subset(ENG_county,ENG_county$NAMEclean %in% unmatched)

# get mobility region names that still have no match
unmatched2 <- setdiff(unmatched, match_county$NAMEclean)

# get mobility regions that match OS ceremonial county names
ENG_cer_county$NAMEclean <- gsub("&","and",ENG_cer_county$NAME)
match_cer_county <- subset(ENG_cer_county,ENG_cer_county$NAMEclean %in% unmatched2)

# combine
outlines <- rbind(match_district[,c("NAME","NAMEclean","geometry")],match_county[,c("NAME","NAMEclean","geometry")],match_cer_county[,c("NAME","NAMEclean","geometry")])
outlines <- merge(outlines,England_districts, by.x = "NAMEclean", by.y = "CleanName", all.x = TRUE)

# identify any missing mobility regions 
setdiff(England_districts$Mobility_name ,outlines$Mobility_name)
plot(st_geometry(outlines),col="green")

# simplify and plot
outlines_simp <- st_simplify(outlines, dTolerance=50)
plot(st_geometry(outlines_simp),col="blue")

# write out shapefile in OGSGB36
st_write(outlines_simp, "googleboundaries_OGSGB36.shp")

# write out shapefile in WGS84
outlines_simp <- spTransform(outlines_simp, CRS("+init=epsg:4326"))
writeOGR(outlines_simp, "googleboundaries_WGS84.shp", layer='googleboundaries_OGSGB36', driver="ESRI Shapefile")

