  
  ##
  # This is the server logic for a Shiny web application.
  # You can find out more about building applications with Shiny here:
  #
  # http://shiny.rstudio.com
  #

source('externalAPI.R')

trash <- read.csv('../Data/output.csv')
trash <- filter(trash, latitude != 0 & latitude != 1 & longitude != 0 & longitude != 1)
types <- distinct(trash, type)

shinyServer(function(input, output) {

  ##
  # Rendered points
  
  points <- eventReactive(input$recalc, {
    cbind(trash$longitude, trash$latitude)
  }, ignoreNULL = FALSE)
  
  ##
  # Leaflet map

  output$map <- renderLeaflet({
    leaflet() %>%
      setView(5, 52, 7) %>%
      addProviderTiles("Stamen.TonerLite", 
        options = providerTileOptions(
          noWrap = TRUE, 
          maxZoom = 18,
          minZoom = 1
        )
      ) %>%
      addMarkers(clusterOptions = markerClusterOptions(), data = points())
  })
  
  ##
  # Observers
  #
  # Events, kind of like "input$map_object_event".
  # Possible objects: marker, map, shape
  # Possible events:  click, mouseover, mouseout, bounds, zoom

  # Should show a popover with detailed information
  observe({
    click <- input$map_marker_click
    if(is.null(click))
      return()
    output$text <- renderText(paste("Marker: Lattitude ", click$lat, "Longtitude ", click$lng))
  })
  
  # Should show all possible information related to this location using API?
  # EXAMPLE GOOGLE REQUEST
  #test <- head(trash, 1)
  #googleData <- googlePlaces(test$latitude, test$longitude, radius = 500, types = NULL, name = NULL)
  observe({
    click <- input$map_click
    if(is.null(click))
      return()
    output$text <- renderText(paste("Map: Lattitude ", click$lat, "Longtitude ", click$lng))
  })
  
  # Should show relevant info at zoom level
  observe({
    e <- input$map_zoom
    if(is.null(e))
      return()
    output$text <- renderText(paste("Zoom: ", e))
  })
})
