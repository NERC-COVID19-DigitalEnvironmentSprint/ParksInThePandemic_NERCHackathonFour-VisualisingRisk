# START -------------------------------------------------------------------
rm(list=ls())
set.seed(1234)

# LOAD PACKAGES -----------------------------------------------------------

#install.packages('XML')
library(XML) # HTML processing
#install.packages('RCurl')
library(RCurl)
#install.packages('rvest')
library(rvest)
#install.packages('stringr')
library(stringr)
#install.packages('tibble')
library(tibble)


read.mobilityreports<-function(){
  base.url<-"https://www.google.com/covid19/mobility/"
  base.url2<-getURL(base.url)
  parsed<-htmlParse(base.url2)
  doc.links<-xpathSApply(parsed,path = "//a",xmlGetAttr,"href")
  csv.url<- as.character(doc.links[grep('csv', doc.links)])
  Google_Mobility_data<<-read.csv(csv.url, header = TRUE)
}

read.mobilityreports()



