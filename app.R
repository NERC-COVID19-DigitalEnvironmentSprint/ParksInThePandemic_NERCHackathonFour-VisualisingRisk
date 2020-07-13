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
library(remotes)
library(osfr)
library(here)
library(conflicted)
conflict_prefer("box", "shinydashboard")


# Import functions
# ----------------

source("code/plot.googlemobilitydistricts.R")
source("code/read.googlemobility.R")


# Load data
# ---------

# Google mobility data
if (file.exists("data/temporal/google_and_metoffice_england.csv")) {
  google <- read.csv("data/temporal/google_and_metoffice_england.csv")
} else {
  google <- read.googlemobility()
}

# AB: What are the following 7 lines for? do we still need them?

#import mobility data
#mobilitydata <- read.csv("data/temporal/google_and_metoffice.csv")
# make the map
#bedford <- readOGR(dsn="data/spatial", layer="TL_GreenspaceSite")
#shapeData <- spTransform(bedford, CRS("+init=epsg:4326"))
#UK_latlon <- readRDS("data/UK_dat_ggplot.RDS")
#UK_Mobility <- readRDS("data/UK_Mobility.RDS")

# Shape data
if (file.exists("data/spatial/googleboundaries_WGS84.shp")) {
  shapeData<-readOGR(dsn="data/spatial", layer="googleboundaries_WGS84")
} else {
  #import google boundaries shapefile from Open Science Framework data repository
  pp_project <- osf_retrieve_node("c7kg4")
  osf_ls_files(pp_project, pattern='WGS84') %>% osf_download(path='data/spatial')
  shapeData<-readOGR(dsn="data/spatial", layer="googleboundaries_WGS84")
  
}

# Variables
# ---------

titleWidth <- 250

# Widgets
# -------

text.box <- box(
  title="How busy are my local parks likely to be?", 
  footer = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. In suscipit malesuada dolor, ac auctor mi venenatis nec. Suspendisse ipsum augue, luctus venenatis sagittis sit amet, posuere non velit. Praesent aliquam finibus consectetur. Phasellus laoreet nisi tincidunt lorem fringilla tincidunt. Fusce quis pulvinar ipsum, a blandit nisl. Sed quis est rhoncus, porta nunc a, cursus tellus. Morbi euismod erat felis. Sed varius quam vel eros congue, vel convallis justo ullamcorper. Maecenas posuere, justo sed dapibus posuere, leo ex dapibus est, id viverra neque odio in lorem. Proin lobortis ante est, sed aliquet orci varius sed.", 
  width = 12
)
graph <- plotOutput("plot1")
map <- plotOutput("map1", height=700, width=400)

# Input widgets
date.input <- sidebarMenu(dateRangeInput("daterange", "Date range", start="2020-01-01", end="2021-01-01"))
place.input <- selectInput("place", "Choose a region", choices=unique(shapeData$NAME), selected="Bedford (B)")

# UI code
# -------

header <- dashboardHeader(title="Parks in the Pandemic", titleWidth=titleWidth)

sidebar <- dashboardSidebar(
  date.input,
  place.input,
  width=titleWidth
)

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

# Server
# ------

server <- function(input, output) {
  google_react<-reactive({
    google %>% 
      dplyr::filter(between(as.Date(date),min(as.Date(input$daterange1)), max(as.Date(input$daterange1)))) %>% 
      select(sub_region_1, date, parks_percent_change_from_baseline) %>% 
      group_by(sub_region_1) %>% 
      dplyr::summarise(mn=mean(parks_percent_change_from_baseline, na.rm=TRUE)) %>% 
      dplyr::mutate(NAME=sub_region_1)
  })
  
  google_react2<-reactive({
    google %>% 
      dplyr::filter(between(as.Date(date),min(as.Date(input$daterange1)), max(as.Date(input$daterange1)))) 
  })
  
  shapeData2 <- reactive({
    shapeData2 <- shapeData[shapeData$NAME==input$place,]  
  })
     
  google_shp_merge <- reactive({merge(shapeData, google_react())})
 
  output$map1 <- renderPlot({
    par(mar=c(3, 0, 3, 0))
    mycolours <- RColorBrewer::brewer.pal(8, "Blues")
    mybreaks <- c(-60,-40,-20,0, 20,40,60)
    mycolours <- RColorBrewer::brewer.pal(8, "Blues")
    mybreaks <- c(-60,-40,-20,0, 20,40,60)
    #cut(google_shp_merge$mn, mybreaks)
    mycolourscheme <- mycolours[findInterval(google_shp_merge()$mn, vec = mybreaks)]
    plot(google_shp_merge(), xlim=c(-5.5,1.5), ylim=c(50,56), col = mycolourscheme)
    plot(shapeData2(), xlim=c(-5.5,1.5), ylim=c(50,56), add=TRUE, density=2,lwd=4)
  })
  
  output$plot1 <- renderPlot({print(plot.googlemobilitydistricts(google_react2(), "parks", "Bedford"))})
}
  
# Run
# ---

shinyApp(
  ui = dashboardPage(header, sidebar, body),
  server = server
)
  