  
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
  # Leaflet map

  output$map <- renderLeaflet({
    leaflet(trash) %>%
      setView(5, 52, 7) %>%
      addProviderTiles("Stamen.TonerLite", 
        options = providerTileOptions(
          noWrap = TRUE, 
          maxZoom = 18,
          minZoom = 1
        )
      ) %>%
      addMarkers(clusterOptions = markerClusterOptions())
  })
  
  ##
  # Observers
  #
  # Events, kind of like "input$map_object_event".
  # Possible objects: marker, map, shape
  # Possible events:  click, mouseover, mouseout, bounds, zoom

  # Should determine wether location is accurate enough to request google places information
  observe({
    e <- input$map_zoom
    if(is.null(e))
      return()
    output$text <- renderText(paste("Zoom: ", e))
  })
  
  # Should output usefull information about the information and trash at that location
  observe({
    click <- input$map_click
    if(is.null(click))
      return()
    output$text <- renderText(paste("Map: Lattitude ", click$lat, "Longtitude ", click$lng))
  })
  
  # Should show information about the trash
  observe({
    click <- input$map_marker_click
    if(is.null(click))
      return()
    output$text <- renderText(paste("Marker: Lattitude ", click$lat, "Longtitude ", click$lng))
  })
  
  # Button press should show graphs and additional information
  observeEvent(input$showGraphs, {
    output$text <- renderText('Button pressed: yes')
  })
})


#nearby <- nearbySearch(52.743, 5.221, 1000)
#text <- textSearch(52.743, 5.221, 1000)
#radar <- radarSearch(52.743, 5.221, 1000, type = 'food')
