# Set the root directory as the working directory

# Libraries
# ---------
library(rgdal)
#library(leaflet)
library(tidyverse)
library(shinydashboard)
library(shinythemes)
library(shiny)
#library(shinyWidgets)
#library(leaflet.extras)
library(dashboardthemes)
library(htmlwidgets)
library(htmltools)
library(remotes)
library(osfr)
library(here)
library(conflicted)
conflict_prefer("box", "shinydashboard")

# Import functions from repo
# --------------------------

source("code/plot.googlemobilitydistricts.R")
source("code/read.googlemobility.R")
source("code/getRnumbers.R")

# Download data from OSF
# ----------------------

# If the data folder does not exist or is empty
if (length(list.files("data")) == 0) {
  
  # Authenticate with read-only token
  osf_auth("kEvueIO13WfWg4HpPcAXF9nx8eh7IfqoK2fcKGH5jErKfFurf0Y4efhk75frTvMwMJb3pz")
  
  # Get tibble of data on the OSF store
  data <- osf_ls_files(osf_retrieve_node("c7kg4"))
  
  # Download to ./data
  dir.create("./data")
  osf_download(data, path="./data", verbose=TRUE, progress=TRUE, recurse=TRUE, conflicts="skip")
}

# NOTE: To re-download all the data from OSF, just delete your local "data" directory and re-run app.R

# Read data 
# ----------


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

shapeData <- readOGR(dsn="data/spatial", layer="googleboundaries_WGS84")
shapeData$NAME <- gsub( " *\\(.*?\\) *", "", shapeData$NAME)
shapeData$NAME <- gsub( "City of ", "", shapeData$NAME)
shapeData$NAME <- gsub( "The Brighton and Hove", "Brighton and Hove", shapeData$NAME)

Rnumbers <- getRnumbers()

# Widgets
# -------

# this layout is very tidy 
text.box <- box(title = "How busy are my local parks likely to be?", footer = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. In suscipit malesuada dolor, ac auctor mi venenatis nec. Suspendisse ipsum augue, luctus venenatis sagittis sit amet, posuere non velit. Praesent aliquam finibus consectetur. Phasellus laoreet nisi tincidunt lorem fringilla tincidunt. Fusce quis pulvinar ipsum, a blandit nisl. Sed quis est rhoncus, porta nunc a, cursus tellus. Morbi euismod erat felis. Sed varius quam vel eros congue, vel convallis justo ullamcorper. Maecenas posuere, justo sed dapibus posuere, leo ex dapibus est, id viverra neque odio in lorem. Proin lobortis ante est, sed aliquet orci varius sed.", width = 12)

date.box <- dateRangeInput("daterange1", "Date range:", start="2020-01-01", end="2021-01-01")

day.box<-selectInput("dayOfTheWeek", "Choose a day of the week", choices=c("Monday"='1',"Tuesday"='2', "Wednesday"='3', "Thursday"='4', "Friday"='5', "Saturday"='6', "Sunday"='0'),
                     selected=format(as.Date(Sys.Date()),"%w") , multiple = FALSE, selectize = TRUE, width=NULL, size=NULL)

place.box<-selectInput("place", "Choose a region", choices=unique(shapeData$NAME)
                       , selected = "Bedford", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)

baseline.check<-selectInput("custom_base", "Do you want a custom baseline?", choices=c("No", "Yes"), selected = "No")

#baseline.box<-dateInput("basedate", "Date:", value = "2020-02-29")

graph <- plotOutput("plot1")

map <- plotOutput("map1", height=700*1.5, width=400*1.5)

info.box <-infoBox("R value", value = paste0("England's R number is ", Rnumbers$Rnumbers.R_med[1]), subtitle = NULL,
        icon = shiny::icon("bar-chart"), color = "aqua", width = 4,
        href = TRUE, fill = FALSE)

#map <- leafletOutput("map1", height = 600)

# UI.R code
# ---------

header <- dashboardHeader(title="Parks in the Pandemic", titleWidth = 250)


sidebar <- dashboardSidebar(date.box,place.box, day.box,baseline.check,
                            conditionalPanel("input.custom_base=='Yes'", 
                                             dateInput("basedate", "Date:", value = "2020-02-29")),
                            width = 250)

body <- dashboardBody(
  
  tags$head(tags$style(HTML('
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #308759;
                                }
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #308759;
                                }
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #308759;
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
      box(width=12, graph),
      info.box
    ),
    column(6, box(width=12, map))
  )
)

# server
# ------

server <- function(input, output) {
  google_react<-reactive({
    google %>% 
      dplyr::filter(dplyr::between(as.Date(date),min(as.Date(input$daterange1)), max(as.Date(input$daterange1)))) %>% 
      select(sub_region_1, date, parks_percent_change_from_baseline) %>% 
      group_by(sub_region_1) %>% 
      dplyr::summarise(mn=mean(parks_percent_change_from_baseline, na.rm=TRUE)) %>% 
      dplyr::mutate(NAME=sub_region_1)
  })
  google_react2<-reactive({
    
    
    
    google %>% 
      dplyr::filter(dplyr::between(as.Date(date),min(as.Date(input$daterange1)), max(as.Date(input$daterange1)))) %>% 
      dplyr::filter(sub_region_1==input$place)
  })
  
  shapeData2<-reactive({
    shapeData2 <- shapeData[shapeData$NAME==input$place,]  
  })
  
  
  google_shp_merge<-reactive({merge(shapeData, google_react())})
  
  output$map1<-renderPlot({
    par(mar=c(3, 0, 3, 0))
    myscale<-grDevices::colorRampPalette(colors = c("darkgrey", "white", "darkgreen"))
    mycolours<-myscale(8)
    mybreaks <- c(-60,-40,-20,0, 20,40,60)
    #cut(google_shp_merge$mn, mybreaks)
    mycolourscheme <- mycolours[findInterval(google_shp_merge()$mn, vec = mybreaks)]
    plot(google_shp_merge(), xlim=c(-5.5,1.5), ylim=c(50,54.1), col = mycolourscheme)
    plot(shapeData2(), xlim=c(-5.5,1.5), ylim=c(50,54.1), add=TRUE, lwd=2, border='purple')
    
  })
  #map
  #output$map1<-renderLeaflet({
  # map <- leaflet()  %>% addTiles() %>% 
  #  setView(lng = -0.46, lat=52.13,zoom=5) %>% 
  # addPolygons(data=shapeData,weight=5,col = 'green')
  #map
  #})
  #plot
  output$plot1<-renderPlot({
    print(plot.googlemobilitydistricts(google_react2(), "parks", print(input$place)))})
  # ggplot(data=google_react2(), aes(x=as.Date(date),y=parks_percent_change_from_baseline)) +
  #   geom_col(position = position_dodge(width=0.2), size=0.25,colour = 'black', fill ='#D55E00') +
  # #Limits the size of the graph.
  # coord_cartesian(ylim=c(-100,160)) +
  # #plots a horizontal line where no percentage change occurs.
  # geom_hline(yintercept=0) + 
  # #Ensure the background is white, the border is black and removes grid lines.
  # theme(panel.background = element_rect(fill = "white", colour = "black", size = 1, linetype = "solid"),
  #       panel.grid.major = element_blank(), 
  #       panel.grid.minor = element_blank(),
  #       strip.text = element_blank())+
  # #x-label
  # xlab("Date") +
  # #y-label using the previous clean code done outside the plot.
  # ylab("Visit changes for parks(%) relative to per-weekday winter baselines \n(Google Community Mobility data)")+
  # #Add a title for the district data this graph represents.
  # ggtitle(print(input$place))
  #})
  
  
}

# Run
# ---

shinyApp(
  ui = dashboardPage(header, sidebar, body),
  server = server
)
