#A function that scrapes the England COVID-19 R numbers for Gov.Uk website, splits up data into usable columns and generates medians and reformats added last updated date

getRnumbers<-function(){
  
# LOAD PACKAGES -----------------------------------------------------------

#install.packages('XML')
library(XML) # HTML processing
#install.packages('RCurl')
library(RCurl)
#install.packages('rvest')
library(rvest)
#install.packages("httr")
library(httr)
#install.packages("readr")
library(readr)
#install.packages("stringr")
library(stringr)
#install.packages("data.table")
library(data.table)
#install.packages
library(dplyr)
#install.packages('tibble')
library(tibble)
#install.packages("tidyr")
library(tidyr)

url<-'https://www.gov.uk/guidance/the-r-number-in-the-uk'

Rnumbers<-as.data.frame(readHTMLTable(doc = content((GET(url[[1]])),"text")))
colnames(Rnumbers)<-c('Region','R','Growthrateperday_percent')
Rnumbers<-separate(data = Rnumbers, col ='Region', into=c('Region','Lowcase'), sep='\\*', remove=F)
Rnumbers$Lowcase<-print(is.na(Rnumbers$Lowcase))


#seperate R min and max and get median
Rnumbers<-separate(data = Rnumbers, col ='R', into=c('R_min','R_max'), sep='\\-', remove=T)
Rnumbers<-add_column(Rnumbers, .after = 'R_max', 'R_med'=NA)
Rnumbers$R_med<-median(c(Rnumbers$Rmin,Rnumbers$Rmax))
#make columns numeric
Rnumbers[,1:2]<-sapply(Rnumbers[,1:2],as.numeric)
#populate R_med column with median R values 
Rnumbers$R_med<-apply(Rnumbers[,1:2], 1, median)


#seperate growth rate min and max and get median
Rnumbers<-separate(data = Rnumbers, col ='Growthrateperday_percent', into=c('Growthrateperday_percent_min','Growthrateperday_percent_max'), sep='\\ to ', remove=T)
Rnumbers<-add_column(Rnumbers, .after = 'Growthrateperday_percent_max', 'Growthrateperday_percent_med'=NA)
Rnumbers$Growthrateperday_percent_med<-median(c('Growthrateperday_percent_min','Growthrateperday_percent_max'))
#make columns numeric
Rnumbers[,5:6]<-sapply(Rnumbers[,5:6],as.numeric)
#populate R_med column with median R values 
Rnumbers$Growthrateperday_percent_med<-apply(Rnumbers[,5:6], 1, median)

urltext<-read_html(url)
urltext<- urltext %>% html_nodes("body") %>% html_text()
lastupdated<-str_match(urltext, "Last updated\\s*(.*?)\\s*\n")[,2]
lastupdated<-as.Date(lastupdated, '%d %b %Y')


Rnumbers<-data.frame(
                'Lastupdated' = as.Date(lastupdated),
                'Region' = Rnumbers$Region,
                'Lowcase' = Rnumbers$Lowcase,
                'R_min' = Rnumbers$R_min,
                'R_max' = Rnumbers$R_max,Rnumbers$R_med,
                'Growthrateperday_percent_min' = Rnumbers$Growthrateperday_percent_min,
                'Growthrateperday_percent_max' = Rnumbers$Growthrateperday_percent_max,
                'Growthrateperday_percent_med' = Rnumbers$Growthrateperday_percent_med)

Rnumbers

}

#example implementation
#Rnumbers<-getRnumbers()
#write.csv(Rnumbers)