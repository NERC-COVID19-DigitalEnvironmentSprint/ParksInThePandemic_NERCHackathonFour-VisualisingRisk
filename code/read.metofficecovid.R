#Code for a function that downloads all the metoffice data sets and merge them together into one big data set.

read.metofficecovid<-function(){
  
  # Load packages ----------------------------------------------------------
  #install.packages('plotrix')
  library(plotrix)
  #install.packages('tibble')
  library(tibble)
  #install.packages('ggplot2')
  library(ggplot2)
  #install.packages('gridExtra')
  library(gridExtra)
  #install.packages('grid)
  library(grid)
  #install.packages('ggplot2)
  library(ggplot2)
  #install.packages('lattice')
  library(lattice)
  #install.packages("tidyr")
  library(tidyr)
  #install.packages('reshape2')
  library(reshape2)
  #install.packages('dplyr')
  library(dplyr)
  #install.packages('plyr')
  library(plyr)
  #install.packages('XML')
  library(XML) # HTML processing
  #install.packages('RCurl')
  library(RCurl)
  #install.packages('rvest')
  library(rvest)
  #install.packages('stringr')
  library(stringr)
  #install.packages('plotrix)
  library(plotrix)  
  
  
  #Defines the url link that the data can be extracted from.
  metoffice.link<-"https://metdatasa.blob.core.windows.net/covid19-response/index.html"
  #Extracts all the information from the webpage and maintains the information from the other htmls.
  metoffice.link2<-getURL(metoffice.link)
  parsed<-htmlParse(metoffice.link2)
  #Extracts all links for the urls present on the webpage.
  doc.links<-xpathSApply(parsed,path = "//a",xmlGetAttr,"href")
  #Subsets all the links that are a csv document.
  csv.url<- as.character(doc.links[grep('csv',doc.links)])
  #Subsets all the links that are only for the UK covid reporting regions
  csv.url<-csv.url[grep("UKcovidreportingregions",csv.url)]
  #Re-adds the beginning of the webpage url to allow for downloading directly from the webpage.
  csv.url2<-paste("https://metdatasa.blob.core.windows.net/covid19-response/",csv.url,sep = "")
  #Compiles all the datasets together and prints this dataframe to your environment. 
  ldply(csv.url2, read.csv)
}

#example implementation
#met<-read.metofficecovid()
#write.csv(met, 'met.csv', row.names = F)