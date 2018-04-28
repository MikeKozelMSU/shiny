rm(list=ls())
setwd('~/MSU/PLS - 900/Final Project')


library(dplyr)
library(magrittr)
library(tidyr)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(rgdal)
library(rgeos)
library(geojsonio)
library(cshapes)
library(sp)
library(leaflet.minicharts)
library(ggplot2)

# load in data
df <- read.csv("ged171.csv")

# change lat and long into numeric
df$longitude <- as.numeric(df$longitude)
df$latitude <- as.numeric(df$latitude)


df$random <- as.Date(paste(df$year, 1, 1, sep="-"))

# Shiny UI
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  
  # slider panel
  absolutePanel(
    id = 'controls',
    class = 'panel panel-default',
    fixed = T,
    height = 'auto',
    top=50,
    right = 50,
    left = 'auto',
    bottom = 'auto',
    width = 'auto', 
    draggable = T,
    selectInput('map', "Map Type", choices = c( 
      "CartoDB.Positron",
      "Esri.WorldImagery"), 
      selected = "CartoDB.Positron"),
    sliderInput(
      "range",
      "year Range",
      min=min(df$year),
      max = max(df$year),
      value = range(df$year),
      step=1
    )
  )
  
)


# SHiny server
server <- function(input, output, session) {
  filteredData <- reactive({
    df[df$year >= input$range[1] & df$year <= input$range[2],]
  })
  
  # Map output
  output$map <- renderLeaflet({
    leaflet(df) %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude)) %>%
      addProviderTiles(input$map)
  })
  
  
  # plot points on the map
  observe({
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>% clearPopups() %>% clearMarkers() %>% clearMarkerClusters() %>% 
      addCircleMarkers(lng=~longitude, lat=~latitude, popup = ~paste(paste("<h3>", filteredData()$conflict_name, "</h3>"), 
                                                                     paste("<b>Side A:</b>", side_a, "<br>", "<b>Side B:</b>", side_b, "<br>",
                                                                           "<b>Date:</b>", paste(date_start, date_end, sep = " - "), '<br>', "<b>Casualties:</b>", best , sep = " ")),
                       clusterOptions = markerClusterOptions())
  })
  
}

# RUn app
shinyApp(ui, server)
