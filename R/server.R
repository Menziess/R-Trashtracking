  
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
      addMarkers(
        clusterOptions = markerClusterOptions(), 
        popup = ~as.character(paste(type, brand))
      )
  })
  
  output$table <- renderDataTable({trash})
  
  ##
  # Observers
  #
  # Events, kind of like "input$map_object_event".
  # Possible objects: marker, map, shape
  # Possible events:  click, mouseover, mouseout, bounds, zoom

  observe({
    e <- input$map_zoom
    if(is.null(e))
      return()
    output$text <- renderText(paste("Zoom: ", e))
  })
  
  observe({
    click <- input$map_click
    if(is.null(click))
      return()
    places <- radarSearch(click$lat, click$lng, 1000, type = 'food')
    output$text <- renderText(paste("Map: Lat ", click$lat, "Lng ", click$lng))
    output$text <- renderText(paste("Google Places: ", length(places$results)))
  })
  
  observe({
    click <- input$map_marker_click
    if(is.null(click))
      return()
    output$text <- renderText(paste("Marker: Lat ", click$lat, "Lng ", click$lng))
  })
  
  observeEvent(input$showGraphs, {
    output$text <- renderText('Button pressed: yes')
  })
})
