  
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
  
  ########################
  # Initial Leaflet Map  #
  ########################

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
  
  ########################
  # Reactive Data Filter #
  ########################
  
  filteredData <- reactive({
    if (!is.null(input$trashType) && input$trashType != 'All') {
      trash <- subset(trash, trash$type == input$trashType)
    }
    if (!is.null(input$trashBrand) && input$trashBrand != 'All') {
      trash <- subset(trash, trash$brand == input$trashBrand)
    }
    
    return (trash)
  })

  #######################
  #       Inputs        #
  #######################
  
  # date
  #trash["dates"] <- NA
  #trahs$dates <- as.POSIXct(trashdataset$taken, format="%Y-%m-%d")
  #trash$dates <- as.Date(trashdataset$dates, "%d-%m-%y")
  
  # Trash type  
  output$trashTypeInput = renderUI({
    names <- distinct(trash, type)
    selectInput("trashType", NULL, c("All", as.character(names$type)))
  })
  
  # Trash brand
  output$trashBrandInput = renderUI({
    names <- distinct(trash, brand)
    selectInput("trashBrand", NULL, c('All', as.character(names$brand)))
  })
  
  output$locationTypeInput = renderUI({
    locationTypes <-  c("Afhaalrestaurants" = "meal_takeaway",
                        "Bakkerij" = "bakery",
                        "Bar" = "bar",
                        "Bioscoop" = "movie_theater",
                        "Cafe" = "cafe",
                        "Casino" = "casino",
                        "Dierentuin" = "zoo",
                        "Kampeerplek" = "campground",
                        "Museum" = "museum",
                        "Nachtclub" = "night_club",
                        "Pretpark" = "amusement_park",
                        "School" = "school",
                        "Stadion" = "stadium",
                        "Tankstation" = "gas_station",
                        "Universiteit" = "university",
                        "Warenhuis" = "department_store",
                        "Winkel" = "store")
    selectInput("locationType", NULL, locationTypes)
  })
  
  #######################
  #      Observers      #
  #######################
  
  map <- leafletProxy("map")
  
  # Input type changes
  observe({
    input$type
    map <- leafletProxy("map", data = filteredData()) %>%
      removeMarkerCluster('trash') %>%
      addMarkers(
        clusterId = 'trash',
        clusterOptions = markerClusterOptions(), 
        popup = ~as.character(paste(type, brand))
      )
  })
  
  # Map zoom
  observe({
    e <- input$map_zoom
    if(is.null(e))
      return()
    output$text <- renderText(paste("Zoom: ", e))
  })
  
  # Map click
  observe({
    click <- input$map_click
    if(is.null(click))
      return()

    places <- radarSearch(click$lat, click$lng, input$distanceSlider, input$locationType)
    places2 <- radarSearch(click$lat, click$lng, input$distanceSlider, 'cafe')
    
    nrResults <- length(places$results)
    analyzation <- NULL
    
    if(nrResults == 0) {
      output$text <- renderText(paste("No places found in this area."))
    } else {
      
      # Flatten places
      places <- do.call(rbind, lapply(places$results, data.frame, stringsAsFactors=FALSE))
      places2 <- do.call(rbind, lapply(places2$results, data.frame, stringsAsFactors=FALSE))
      
      # Preperation
      distanceInLatLng <- metersToLatLng(click$lat, click$lng, input$distanceSlider)
      trash <- filter(isolate(filteredData()), latitude > click$lat - distanceInLatLng[[1]] & latitude < click$lat + distanceInLatLng[[1]]
                      & longitude > click$lng - distanceInLatLng[[2]] & longitude < click$lng + distanceInLatLng[[2]])

      # Analyzation
      analyzation <- analyse(trash, places)
      analyzation2 <- analyse(trash, places2)
      analyzation$Place <- paste(input$locationType ,seq.int(nrow(analyzation)))
      analyzation2$Place <- paste('analyse2' ,seq.int(nrow(analyzation2)))
      analyzation <- merge(analyzation, analyzation2, all=T)
      
      # Alternate icon
      greenLeafIcon <- makeIcon(
        iconUrl = "https://lh4.ggpht.com/Tr5sntMif9qOPrKV_UVl7K8A_V3xQDgA7Sw_qweLUFlg76d_vGFA7q1xIKZ6IcmeGqg=w300",
        iconWidth = 38, iconHeight = 40
      )
      
      # Adds google search locations to the map
      map %>% 
        clearGroup('analysis') %>%
        addMarkers(
          data = places,
          group = 'analysis',
          lng = places$geometry.location.lng, 
          lat = places$geometry.location.lat,
          popup = input$locationType,
          icon = greenLeafIcon
        )
    } 
    
    # Update table
    output$table <- renderDataTable({
      if(!is.null(analyzation)) {
        analyzation
      }
    })
    
    output$plot <- renderPlotly({
      plot_ly(analyzation,
        x = ~Place,
        y = ~Amount,
        name = "Plot",
        type = "bar"
      )
    })
    
    # Informative text
    output$text <- renderText(paste("Distance: ", input$distanceSlider, " meter. ", nrResults, "locations found."))
    output$story <- renderText(paste(
      input$locationType, 
      input$trashType, 
      input$trashBrand, 
      input$distanceSlider
    ))
    
    # Add distance circle
    map %>% 
      clearGroup('circles') %>%
      addCircles(lat = click$lat, lng = click$lng, radius = input$distanceSlider, group = "circles")
    
    # Hide markers button
    if (!input$checkboxLocationInput) {
      map %>% hideGroup("analysis")
    } else {
      map %>% showGroup("analysis")
    }
  })
  
  # Marker click
  observe({
    click <- input$map_marker_click
    if(is.null(click))
      return()
    output$text <- renderText(paste("Marker: Lat ", click$lat, "Lng ", click$lng))
  })
  
  # Shape click
  observe({
    click <- input$map_shape_click
    if(is.null(click))
      return()
    map %>% clearGroup('circles')
  })
  
  # Button Detail Page
  observeEvent(input$showDetails, {
    updateNavbarPage(session, "Trashtracking", "Details")
  })
})
