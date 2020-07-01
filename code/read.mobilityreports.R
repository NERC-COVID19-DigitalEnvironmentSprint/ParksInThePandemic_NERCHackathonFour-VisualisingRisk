#A function that downloads the google mobility data, subsets for just the UK data set and creates a column that assigns each subregion with their UK country.

read.mobilityreports<-function(){
  #Define the url link where the google mobility data can be extracted from. 
  base.url<-"https://www.google.com/covid19/mobility/"
  #Extract the information from this link and maintain all the hyperlinks within it.
  base.url2<-getURL(base.url)
  parsed<-htmlParse(base.url2)
  #Extract all hyperlinks that are present on this webpage.
  doc.links<-xpathSApply(parsed,path = "//a",xmlGetAttr,"href")
  #Extract all files that are csv.
  csv.url<- as.character(doc.links[grep('csv', doc.links)])
  #Read in the Google mobility data (this is the only csv file on the webpage)
  Google_Mobility_data<-read.csv(csv.url, header = TRUE)
  #Ensure that the date column is treated as a date classification.
  Google_Mobility_data$date<-as.Date(Google_Mobility_data$date)
  #Subset out all the UK data points. 
  UK<-subset(Google_Mobility_data,country_region_code == "GB")
  #Create a vector that contains the assigning values for each country according to the sub_region_1 value.
  Sub_country<-ifelse(UK$sub_region_1%in%c("Bath and North East Somerset","Bedford","Blackburn with Darwen","Blackpool","Borough of Halton","Bracknell Forest","Brighton and Hove","Bristol City","Buckinghamshire","Cambridgeshire","Central Bedfordshire","Cheshire East","Cheshire West and Chester","Cornwall","County Durham","Cumbria","Darlington","Derby","Derbyshire","Devon","Dorset","East Riding of Yorkshire","East Sussex","Essex","Gloucestershire","Greater London","Greater Manchester","Hampshire","Hartlepool","Herefordshire","Hertfordshire","Isle of Wight","Kent","Kingston upon Hull","Lancashire","Leicester","Leicestershire","Lincolnshire","Luton","Medway","Merseyside","Middlesbrough","Milton Keynes","Norfolk","North East Lincolnshire","North Lincolnshire","North Somerset","North Yorkshire","Northamptonshire","Northumberland","Nottingham","Nottinghamshire","Oxfordshire","Peterborough","Plymouth","Portsmouth","Reading","Redcar and Cleveland","Rutland","Shropshire","Slough","Somerset","South Gloucestershire","South Yorkshire","Southampton","Southend-on-Sea","Staffordshire","Stockton-on-Tees","Stoke-on-Trent","Suffolk","Surrey","Swindon","Thurrock","Torbay","Tyne and Wear","Warrington","Warwickshire","West Berkshire","West Midlands","West Sussex","West Yorkshire","Wiltshire","Windsor and Maidenhead","Wokingham","Worcestershire","York"),"England",
                         ifelse(UK$sub_region_1%in%c("Blaenau Gwent","Bridgend County Borough","Caerphilly County Borough","Cardiff","Carmarthenshire","Ceredigion","Conwy Principal Area","Denbighshire","Flintshire","Gwynedd","Isle of Anglesey","Merthyr Tydfil County Borough","Monmouthshire","Neath Port Talbot Principle Area","Newport","Pembrokeshire","Powys","Rhondda Cynon Taff","Swansea","Torfaen Principal Area","Vale of Glamorgan","Wrexham Principal Area"),"Wales",
                          ifelse(UK$sub_region_1%in%c("Aberdeen City","Aberdeenshire","Angus Council","Argyll and Bute Council","Clackmannanshire","Dumfries and Galloway","Dundee City Council","East Ayrshire Council","East Dunbartonshire Council","East Lothian Council","East Renfrewshire Council","Edinburgh","Falkirk","Fife","Glasgow City","Highland Council","Inverclyde","Midlothian","Moray","Na h-Eileanan an Iar","North Ayrshire Council","North Lanarkshire","Orkney","Perth and Kinross","Renfrewshire","Scottish Borders","Shetland Islands","South Ayrshire Council","South Lanarkshire","Stirling","West Dunbartonshire Council","West Lothian"),"Scotland",
                            ifelse(UK$sub_region_1%in%c("Antrim and Newtownabbey","Ards and North Down","Armagh City, Banbridge and Craigavon","Belfast","Causeway Coast and Glens","Derry and Strabane","Fermanagh and Omagh","Lisburn and Castlereagh","Mid and East Antrim","Mid Ulster","Newry","Mourne and Down"),"Northern Ireland", NA))))
  #Add the new vector into the data-frame after the county_region column. 
  UK_1<-UK %>% add_column(sub_country = Sub_country, .after = which(colnames(UK)=="country_region"))
    }


