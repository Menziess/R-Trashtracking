  
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
  
  updateNavbarPage(session, "Trashtracking", "Overview")

  output$map <- renderLeaflet({
      leaflet() %>%
      setView(5, 52, 7) %>%
      addProviderTiles("Stamen.TonerLite", 
        options = providerTileOptions(
          noWrap = TRUE, 
          maxZoom = 18,
          minZoom = 1
        )
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
    if (!is.null(input$daterange)) {
      trash <- subset(trash, as.Date(trash$dates) >= as.Date(input$daterange[1]) 
                           & as.Date(trash$dates) <= as.Date(input$daterange[2]))
    }
    return (trash)
  })

  #######################
  #       Inputs        #
  #######################
  
  # Scatterplot
  output$scatterplot <- renderPlot({
    # TODO: Zehra
    plot(ChickWeight)
  })
  
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
  
  # Input type changes
  observe({
    input$type
    map <<- leafletProxy("map", data = filteredData()) %>%
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

    withProgress(message = "Getting Google Places", value = 0, {
      places <- radarSearch(click$lat, click$lng, input$distanceSlider, input$locationType)
      nrResults <- length(places$results)
      
      if(nrResults == 0) {
        output$text <- renderText(paste("No places found in this area."))
      } else {
        
        # Flatten places
        places <- do.call(rbind, lapply(places$results, data.frame, stringsAsFactors=FALSE))
        
        # Preperation
        incProgress(1/4, detail = "Filtering Trash")
        distanceInLatLng <- metersToLatLng(click$lat, click$lng, input$distanceSlider)
        trash <- filter(isolate(filteredData()), latitude > click$lat - distanceInLatLng[[1]] & latitude < click$lat + distanceInLatLng[[1]]
                        & longitude > click$lng - distanceInLatLng[[2]] & longitude < click$lng + distanceInLatLng[[2]])
  
        # Analyzation
        incProgress(2/4, detail = "Distance between Trash and Places")
        analyzation <<- analyse(trash, places)
        browser()
        analyzation$Place <- paste(input$locationType ,seq.int(nrow(analyzation)))
        
        # Alternate icon
        greenLeafIcon <- makeIcon(
          iconUrl = "http://icons.iconarchive.com/icons/paomedia/small-n-flat/1024/map-marker-icon.png",
          iconWidth = 38, iconHeight = 40
        )
        
        # Adds google search locations to the map
        incProgress(3/4, detail = "Drawing Places on the map")
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
      incProgress(4/4, detail = "Baking piecharts")
      output$table <- renderDataTable({
        if(!is.data.frame(analyzation))
          return()
        analyzation
      })
      
      # Update plot
      output$plot <- renderPlotly({
        if(!is.data.frame(analyzation))
          return()
        plot_ly(analyzation,
                x = ~Place,
                y = ~Amount,
                name = "Top places with trash",
                type = "bar"
        ) %>% 
          config(p = ., staticPlot = FALSE, displayModeBar = FALSE, workspace = FALSE, 
                 editable = FALSE, sendData = FALSE, displaylogo = FALSE
        ) %>%
        layout(title = 'Trash connected to nearby Google Places',
               xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(title = "Shows Google Places within the blue circle", showgrid = FALSE, 
                            zeroline = FALSE, showticklabels = FALSE))
      })
      
      # Update pie
      output$pie_trash_type <- renderPlotly({
        if(!is.data.frame(analyzation))
          return()
        plot_ly(head(trash %>% count(type, sort = T), 10),
                labels = ~type,
                values = ~n,
                name = "Trash distribution",
                type = "pie"
        ) %>% 
        config(p = ., staticPlot = FALSE, displayModeBar = FALSE, workspace = FALSE, 
               editable = FALSE, sendData = FALSE, displaylogo = FALSE
        ) %>%
        layout(title = 'Trash types within selected area',
               legend = list(x = 100, y = 0.5),
               xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(title = "Shows trash types within the blue circle", showgrid = FALSE, 
                            zeroline = FALSE, showticklabels = FALSE))
      })
      
      # Update pie
      output$pie_trash_brand <- renderPlotly({
        if(!is.data.frame(analyzation))
          return()
        plot_ly(head(trash %>% count(brand, sort = T), 10),
                labels = ~brand,
                values = ~n,
                name = "Trash distribution",
                type = "pie"
        ) %>% 
          config(p = ., staticPlot = FALSE, displayModeBar = FALSE, workspace = FALSE, 
                 editable = FALSE, sendData = FALSE, displaylogo = FALSE
        ) %>%
        layout(title = 'Trash brands within selected area',
               legend = list(x = 100, y = 0.5),
               xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(title = "Shows trash brands within the blue circle", showgrid = FALSE, 
                            zeroline = FALSE, showticklabels = FALSE))
      })
      
      # Informative text
      output$text <- renderText(paste0("Distance: ", input$distanceSlider, " meter. Locations found: ", nrResults, "."))
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
      
      output$graphButton <- renderUI({
        if(!is.data.frame(analyzation))
          return()
        column(8, align="center",
          actionButton("showStatistics", "Show Statistics", class = "btn-success btn-lg")
        )
      })
      
      # # Hide markers button
      # if (!input$checkboxLocationInput) {
      #   map %>% hideGroup("analysis")
      # } else {
      #   map %>% showGroup("analysis")
      # }
    })
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
  
  # Barchart click
  observe({
    click <- event_data("plotly_click")
    if(is.null(click) || !is.data.frame(analyzation))
      return()
    output$LocationName <- renderPrint({
      # TODO: Jeffrey
      analyzation[click$pointNumber + 1,]$Place
    })
    updateNavbarPage(session, "Trashtracking", "Details")
  })
  
  
  ########################
  #       Buttons        #
  ########################
  
  # Button Map Page
  observeEvent(input$showMap, {
    updateNavbarPage(session, "Trashtracking", "Map")
  })
  
  # Button Statistics Page
  observeEvent(input$showStatistics, {
    updateNavbarPage(session, "Trashtracking", "Statistics")
  })
  
  # Button Location Detail Page
  observeEvent(input$showLocationDetails, {
    locationDetails <- locationSearch("ChIJv_eH87sJxkcRufIWAPR3ro8")
    
    output$LocationName <- renderText(paste(locationDetails$result$name))
    output$LocationAdress <- renderText(paste("Adress: " + locationDetails$result$formatted_address))
    
    updateNavbarPage(session, "Trashtracking", "Details")
  })
})
