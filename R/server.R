  
  ##
  # This is the server logic for a Shiny web application.
  # You can find out more about building applications with Shiny here:
  #
  # http://shiny.rstudio.com
  #

source('externalAPI.R')

trash <- read.csv('../Data/output.csv') 
trash <- filter(trash, latitude != 0 & latitude != 1 & longitude != 0 & longitude != 1)

shinyServer(function(input, output, session) {
  
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
        clusterId = 'trash',
        clusterOptions = markerClusterOptions(), 
        popup = ~as.character(paste(type, brand))
      )
  })
  
  filteredData <- reactive({
    trash[trash$type == input$trashType, ]
    trash[trash$brand == input$trashBrand, ]
  })

  ## 
  # Trash types input
  
  output$trashTypeInput = renderUI({
    selectInput("trashType", NULL, distinct(trash, type))
  })
  

  output$trashBrandInput = renderUI({
    selectInput("trashBrand", NULL, distinct(trash, brand))
  })
  
  output$locationTypeInput = renderUI({
    locationTypes <-  c("School" = "school",
                        "Bar" = "bar",
                        "Bakkerij" = "bakery",
                        "Cafe" = "cafe",
                        "Convenience Store" = "convenience_store")
    selectInput("locationType", NULL, locationTypes)
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
      removeMarkerCluster('trash') %>%
      addMarkers(
        clusterId = 'trash',
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
    
    places <- radarSearch(click$lat, click$lng, input$distanceSlider, input$locationType)
    places <- do.call(rbind, lapply(places$results, data.frame, stringsAsFactors=FALSE))
    trash <- filter(trash, latitude > click$lat - 0.1 & latitude < click$lat + 0.1
                    & longitude > click$lng - 0.1 & longitude < click$lng + 0.1)
    
    # update output
    output$table <- renderDataTable({
      analyse(trash, places)
    })
    
    output$text <- renderText(paste("Map: Lat ", click$lat, "Lng ", click$lng, "Google Places: ", length(places$results)))
    
    # Alternate icon
    greenLeafIcon <- makeIcon(
      iconUrl = "https://lh4.ggpht.com/Tr5sntMif9qOPrKV_UVl7K8A_V3xQDgA7Sw_qweLUFlg76d_vGFA7q1xIKZ6IcmeGqg=w300",
      iconWidth = 38, iconHeight = 40
    )
    
    # Adds google search locations to the map
    leafletProxy("map", data = places) %>%
      clearGroup('analysis') %>%
      addMarkers(
        group = 'analysis',
        places$geometry.location.lng, places$geometry.location.lat,
        popup = 'food',
        icon = greenLeafIcon
      )
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
  
  observeEvent(input$showDetails, {
    updateNavbarPage(session, "Trashtracking", "Details")
  })
})
