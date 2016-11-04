  
  ##
  # This is the server logic for a Shiny web application.
  # You can find out more about building applications with Shiny here:
  #
  # http://shiny.rstudio.com
  #

source('externalAPI.R')

trash <- read.csv('../Data/output.csv') 
trash <- filter(trash, latitude != 0 & latitude != 1 & longitude != 0 & longitude != 1)

shinyServer(function(input, output) {
  
  filteredData <- reactive({
    trash[trash$type == input$type, ]
  })
  
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
  
  ## 
  # Trash types
  
  output$type = renderUI({
    selectInput("type", NULL, distinct(trash, type))
  })
  
  ##
  # Observers
  #
  # Events, kind of like "input$map_object_event".
  # Possible objects: marker, map, shape
  # Possible events:  click, mouseover, mouseout, bounds, zoom
  
  observe({
    input$type
    leafletProxy("map", data = filteredData()) %>%
      clearMarkerClusters() %>%
      addMarkers(
        clusterOptions = markerClusterOptions(), 
        popup = ~as.character(paste(type, brand))
      )
  })
  
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
    
    analysis <- analyse(trash, places) # Hier moet de analyse worden uitgevoerd (google places VS trash data)
    
    # update output
    output$table <- renderDataTable({analysis})
    output$text <- renderText(paste("Map: Lat ", click$lat, "Lng ", click$lng, "Google Places: ", length(places$results)))
    
    # Alternate icon
    greenLeafIcon <- makeIcon(
      iconUrl = "https://lh4.ggpht.com/Tr5sntMif9qOPrKV_UVl7K8A_V3xQDgA7Sw_qweLUFlg76d_vGFA7q1xIKZ6IcmeGqg=w300",
      iconWidth = 38, iconHeight = 40,
    )    
    
    leafletProxy("map") %>%
      addMarkers("map", lat = 52.745, lng = 5.221, icon = greenLeafIcon)
  })
  
  
  observe({
    click <- input$map_marker_click
    if(is.null(click))
      return()
    output$text <- renderText(paste("Marker: Lat ", click$lat, "Lng ", click$lng))
  })
  
  observeEvent(input$type, {
    output$text <- renderText(paste("Input: ", input$type))
  })
  
})
