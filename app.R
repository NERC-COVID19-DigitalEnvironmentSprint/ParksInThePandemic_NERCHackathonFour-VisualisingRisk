# Set the root directory as the working directory

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

# Import functions from repo
# --------------------------

source("code/plot.googlemobilitydistricts.R")
source("code/read.googlemobility.R")

# Import data from repo OR online
# -------------------------------

# Read google mobility data if possible, otherwised download it
if (file.exists("data/temporal/google_and_metoffice.csv")) {
  google <- read.csv("data/temporal/google_and_metoffice.csv")
} else {
  google <- read.googlemobility()
}

#import mobility data
#mobilitydata <- read.csv("data/temporal/google_and_metoffice.csv")

# make the map
bedford <- readOGR(dsn="data/spatial", layer="TL_GreenspaceSite")
shapeData <- spTransform(bedford, CRS("+init=epsg:4326"))
#UK_latlon <- readRDS("data/UK_dat_ggplot.RDS")
#UK_Mobility <- readRDS("data/UK_Mobility.RDS")


# Widgets
# -------

text.box <- box(title = "How busy is my local park likely to be?", footer = "Insert Text", width = 12)

date.box <- dateRangeInput("daterange1", "Date range:", start="2020-01-01", end="2020-08-31")

graph <- plotOutput("plot1")

map <- leafletOutput("map1", height = 600)

# UI.R code
# ---------

header <- dashboardHeader(title="Parks in the Pandemic", titleWidth = 250)

sidebar <- dashboardSidebar(date.box, width = 250)

body <- dashboardBody(
  fluidRow(
    column(
      6,
      text.box,
      box(width=12, graph)
    ),
    column(6, box(width=12, map))
  )
)

# server
# ------

server <- function(input, output) {
  google_react<-reactive({
    google %>% 
      dplyr::filter(between(as.Date(date),min(as.Date(input$daterange1)), max(as.Date(input$daterange1))))}
  )
  #map
    output$map1<-renderLeaflet({
      map <- leaflet()  %>% addTiles() %>% 
        setView(lng = -0.46, lat=52.13,zoom=12) %>% 
        addPolygons(data=shapeData,weight=5,col = 'green')
      map
      })
  #plot
    output$plot1<-renderPlot({print(plot.googlemobilitydistricts(google_react(), "parks", "Bedford"))})
}
  
# Run
# ---

shinyApp(
  ui = dashboardPage(header, sidebar, body),
  server = server
)
  