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
#bedford <- readOGR(dsn="data/spatial", layer="TL_GreenspaceSite")
#shapeData <- spTransform(bedford, CRS("+init=epsg:4326"))
#UK_latlon <- readRDS("data/UK_dat_ggplot.RDS")
#UK_Mobility <- readRDS("data/UK_Mobility.RDS")

#import google boundaries shapefile from Open Science Framework
pp_project <- osf_retrieve_node("c7kg4")
shapeData<-osf_retrieve_file("https://osf.io/hzkm7/") %>%
  osf_download()

# Widgets
# -------

text.box <- box(title = "How busy is my local park likely to be?", footer = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. In suscipit malesuada dolor, ac auctor mi venenatis nec. Suspendisse ipsum augue, luctus venenatis sagittis sit amet, posuere non velit. Praesent aliquam finibus consectetur. Phasellus laoreet nisi tincidunt lorem fringilla tincidunt. Fusce quis pulvinar ipsum, a blandit nisl. Sed quis est rhoncus, porta nunc a, cursus tellus. Morbi euismod erat felis. Sed varius quam vel eros congue, vel convallis justo ullamcorper. Maecenas posuere, justo sed dapibus posuere, leo ex dapibus est, id viverra neque odio in lorem. Proin lobortis ante est, sed aliquet orci varius sed.", width = 12)

date.box <- dateRangeInput("daterange1", "Date range:", start="2020-01-01", end="2020-08-31")

graph <- plotOutput("plot1")

map <- leafletOutput("map1", height = 600)

# UI.R code
# ---------

header <- dashboardHeader(title="Parks in the Pandemic", titleWidth = 250)


sidebar <- dashboardSidebar(date.box, width = 250)

body <- dashboardBody(
  tags$head(tags$style(HTML('
                                
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #FFFFFF;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color:	#FFFFFF ;
                                }

                                '))),
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
  