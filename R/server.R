
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

source('externalAPI.R')

trash <- read.csv('../Data/output.csv')


  ## EXAMPLE GOOGLE REQUEST
  #test <- head(trash, 1)
  #googleData <- googlePlaces(test$latitude, test$longitude, radius = 500, types = NULL, name = NULL)

shinyServer(function(input, output) {

  trash <- filter(trash, latitude != 0 & latitude != 1 & longitude != 0 & longitude != 1)
  
  points <- eventReactive(input$recalc, {
    cbind(trash$longitude, trash$latitude)
  }, ignoreNULL = FALSE)

  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("Stamen.TonerLite", 
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(clusterOptions = markerClusterOptions(), data = points())
  })
  
  observe({
    click <- input$map_marker_click
    if(is.null(click))
      return()
    output$text <- renderText(paste("Lattitude ", click$lat, "Longtitude ", click$lng))
  })
  
  observe({
    click <- input$map_marker_mouseout
    if(is.null(click))
      return()
    output$text <- renderText("mouseout")
  })
  
  observe({
    click <- input$clusterclick
    if(is.null(click))
      return()
    output$text <- renderText(click)
  })
})
