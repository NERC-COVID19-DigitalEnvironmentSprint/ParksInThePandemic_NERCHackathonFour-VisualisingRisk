#Code for a function that downloads the recent Google Mobility Report

read.mobilityreports<-function(){
  #Defines the url where the mobility is taken from.
  base.url<-"https://www.google.com/covid19/mobility/"
  #Extracts data from the webpage at the defined url and maintains the other htmls information.
  base.url2<-getURL(base.url)
  parsed<-htmlParse(base.url2)
  #Extracts all links that are present on this page.
  doc.links<-xpathSApply(parsed,path = "//a",xmlGetAttr,"href")
  #Extracts links that are only csvs'
  csv.url<- as.character(doc.links[grep('csv', doc.links)])
  #Downloads the recent data set
  Google_Mobility_data<<-read.csv(csv.url, header = TRUE)
}
