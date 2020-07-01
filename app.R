## app.R ##
#need to make it a package 

library(here)
library(rgdal)
library(leaflet)
library(tidyverse)
library(shinydashboard)
library(shinythemes)
library(shiny)
library(leaflet.extras)
library(dashboardthemes)
library(htmlwidgets)
library(htmltools)

#Build the shiny App
#source(here("/Parks_and_pandemic/get_spatial_data.R"))

bedford<-readOGR(dsn=paste0(here(),"/data/spatial"), layer="TL_GreenspaceSite")
shapeData <- spTransform(bedford, CRS("+init=epsg:4326"))

ui <- dashboardPage(
  
  dashboardHeader(
    
    title = "Parks in the Pandemic Dashboard",
    
    titleWidth = 400
    
  ),
  
  dashboardSidebar(out=h3("Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
")),
  
  dashboardBody(tags$head(tags$style(HTML('.box {margin: 0px;}'))),
    
   fluidRow(
     leafletOutput("map1")
   ),
  fluidRow(
    box(dateRangeInput("daterange1", "Date range:",start = "2020-01-01",end   = "2020-08-31")),
    box(plotOutput("plot1"))
          )        
          )
        )
 
    

  

  

server <- function(input, output) {
    #slow but wont be here in the final app - we can have a function to update the data and run the function here
    #need to change the path here
    UK_latlon <- readRDS(here("/data/UK_dat_ggplot.RDS"))
  
    
    UK_Mobility <- readRDS(here("/data/UK_Mobility.RDS"))
    
   
    UKdata<-reactive({
      UK_Mobility %>% 
        filter(between(date,min(as.Date(input$daterange1)), max(as.Date(input$daterange1))))
      
    })
  
    #
    #output$mymap1<- renderLeaflet({ m@map }) 
      # leaflet(UK_latlon,width = "50%", height = 400) %>%
        #    addTiles() %>%
         #   addPolygons()
    #})
    
    output$map1<-renderLeaflet({
      map <- leaflet()  %>% addTiles() %>% 
        setView(lng = -0.46, lat=52.13,zoom=12) %>% 
        addPolygons(data=shapeData,weight=5,col = 'green')
      map
    })
    # output$map2<-renderLeaflet({
    #   map <- leaflet()  %>% addTiles() %>% 
    #     setView(lng = -0.46, lat=52.13,zoom=12) %>% 
    #     addPolygons(data=shapeData,weight=5,col = 'red')
    #   map
    # })
    # output$map3<-renderLeaflet({
    #   map <- leaflet()  %>% addTiles() %>% 
    #     setView(lng = -0.46, lat=52.13,zoom=12) %>% 
    #     addPolygons(data=shapeData,weight=5,col = 'blue')
    #   map
    # })
    output$plot1<-renderPlot({
      UKdata() %>%
        ggplot(aes(date, parks_percent_change_from_baseline))+
        geom_point()
    })

}

shinyApp(ui, server)