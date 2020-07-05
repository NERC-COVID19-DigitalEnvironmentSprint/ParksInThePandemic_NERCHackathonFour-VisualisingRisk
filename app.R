# Be sure to set the root directory as the working directory

# Libraries
# ---------
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

# Import our functions
# --------------------
#source("/Parks_and_pandemic/get_spatial_data.R")
source("code/plot.googlemobilitydistricts.R")
source("code/read.googlemobility.R")

# Import data
# -----------
# Read google mobility data if possible, otherwised download it
#if (file.exists("data/google/mobility_UK.csv")) {
#  google <- read.csv("data/google/mobility_UK.csv")
#} else {
#  google <- read.googlemobility()
#  # google <- read.googlemobility("data/google/mobility_UK.csv")
#}
# Question: Would it be possible to save the downloaded google mobility data to a given file path, like:
# read.googlemobility(path.to.destination)
# To save dowloading it on each run

bedford <- readOGR(dsn="data/spatial", layer="TL_GreenspaceSite")
shapeData <- spTransform(bedford, CRS("+init=epsg:4326"))

# Widgets
# -------
text.box <- box(
  title="How busy is my local park likely to be?", 
  footer=paste(rep("blah", 20), collapse=" "), 
  height=300,
  width=12
)
date.box <- box(
  dateRangeInput("daterange1", "Date range:", start="2020-01-01", end="2020-08-31")
)
graph <- plotOutput("plot1")
map <- leafletOutput("map1")

ui <- dashboardPage(
  dashboardHeader(title="Parks in the Pandemic Dashboard", titleWidth=400),
  dashboardSidebar(out=h3("DASHBOARD")),
  dashboardBody(
    tags$head(tags$style(HTML('.box {margin: 0px;}'))),
    fluidRow(
      column(6, text.box, fluidRow(graph)),
      column(6, map)
    )
  )
)
 
    
# server
#---------

server <- function(input, output) {
  
    # MAKE MAP
  
    #slow but wont be here in the final app - we can have a function to update the data and run the function here
    #need to change the path here
    UK_latlon <- readRDS("data/UK_dat_ggplot.RDS")
    UK_Mobility <- readRDS("data/UK_Mobility.RDS")
    UKdata<-reactive({
      UK_Mobility %>% 
        filter(between(date,min(as.Date(input$daterange1)), max(as.Date(input$daterange1))))}
    )
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
    
    # MAKE PLOT
    output$plot1<-renderPlot({print(plot.googlemobilitydistricts(google,"parks","Bedford"))})
}

# Run the application
shinyApp(ui, server)