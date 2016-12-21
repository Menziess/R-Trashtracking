  
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
  
  #######################
  #       Outputs       #
  #######################

  output$overview <- renderPlot({ 
    Mdata <- reactive({
      tabletype = table(trash$brand, trash$type)
      types = as.data.frame(tabletype)
      names(types)[1] = 'brand'
      names(types)[2] = 'type'
      names(types)[3] = 'amount'
      types <- types[order(-types$amount),]
      types <- filter(types,grepl('Red Bull|Heineken|Coca Cola|AH|AA|Spa|Amstel|Slammers',brand))
      df = ddply(types, .(brand), transform, percent = round((amount/sum(amount) * 100),1))
    })
    
    output$M3 <- renderDataTable({
      Mdata()
    })
    
    ggplot(Mdata(), aes(x=reorder(brand,amount,function(x)+sum(x)), y=percent, fill=type))+
      geom_bar(position = "fill", stat='identity',  width = .7)+
      geom_text(aes(label=percent, ymax=100, ymin=0), vjust=0, hjust=2, color = "white",  position=position_fill())+
      coord_flip() +
      scale_y_continuous(labels = percent_format())+
      ylab("")+
      xlab("")
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
      school <- radarSearch(click$lat, click$lng, input$distanceSlider, "school")
      cafe <- radarSearch(click$lat, click$lng, input$distanceSlider, "cafe")
      takeaway <- radarSearch(click$lat, click$lng, input$distanceSlider, "meal_takeaway")
      university <- radarSearch(click$lat, click$lng, input$distanceSlider, "university")
      schools <- c(school$results, university$results)
      restaurants <- c(cafe$results, takeaway$results)
      places <- c(school$results, cafe$results, takeaway$results, university$results)
      nrResults <- length(places)
      
      if(nrResults == 0) {
        output$text <- renderText(paste("No places found in this area."))
      } else {
        
        # Flatten places
        schools <- do.call(rbind, lapply(schools, data.frame, stringsAsFactors=FALSE))
        restaurants <- do.call(rbind, lapply(restaurants, data.frame, stringsAsFactors=FALSE))
        places <- do.call(rbind, lapply(places, data.frame, stringsAsFactors=FALSE))
        # places <- do.call(rbind, lapply(places$results, data.frame, stringsAsFactors=FALSE))
        
        # Preperation
        incProgress(1/4, detail = "Filtering Trash")
        distanceInLatLng <- metersToLatLng(click$lat, click$lng, input$distanceSlider)
        trash <- filter(isolate(filteredData()), latitude > click$lat - distanceInLatLng[[1]] & latitude < click$lat + distanceInLatLng[[1]]
                        & longitude > click$lng - distanceInLatLng[[2]] & longitude < click$lng + distanceInLatLng[[2]])
        
        # Analyzation
        incProgress(2/4, detail = "Distance between Trash and Places")
        analyzation <<- analyse(trash, places)
        
        # Alternate icon
        # greenLeafIcon <- makeIcon(
        #   iconUrl = "http://icons.iconarchive.com/icons/paomedia/small-n-flat/1024/map-marker-icon.png",
        #   iconWidth = 38, iconHeight = 40
        # )
        # blueLeafIcon <- makeIcon(
        #   iconUrl = "http://i.imgur.com/heDEpTw.png",
        #   iconWidth = 38, iconHeight = 40
        # )
        # 
        # # Adds google search locations to the map
        # incProgress(3/4, detail = "Drawing Places on the map")
        # if (length(schools) > 0){
        #   map %>% 
        #     clearGroup('schoolgroup') %>%
        #     addMarkers(
        #       data = schools,
        #       group = 'schoolgroup',
        #       lng = schools$geometry.location.lng, 
        #       lat = schools$geometry.location.lat,
        #       popup = "School",
        #       icon = greenLeafIcon
        #     )
        # } else {
        #   map %>%
        #     clearGroup('schoolgroup')
        # }
        # if (length(restaurants) > 0){
        #   map %>%
        #     clearGroup('restaurantgroup') %>%
        #     addMarkers(
        #       data = restaurants,
        #       group = 'restaurantgroup',
        #       lng = restaurants$geometry.location.lng, 
        #       lat = restaurants$geometry.location.lat,
        #       popup = "Restaurant",
        #       icon = blueLeafIcon
        #     )
        # } else {
        #   map %>%
        #     clearGroup('restaurantgroup')
        # }
      } 
      
      # Update Table
      incProgress(4/4, detail = "Baking piecharts")
      output$table <- renderDataTable({
        if(!is.data.frame(analyzation))
          return()
        analyzation
      })
      
      # Update Places Barchart
      output$plot <- renderPlotly({
        if(!is.data.frame(analyzation))
          return()
        plot_ly(analyzation,
                x = ~Place,
                y = ~Amount,
                name = "Top places with trash",
                type = "bar",
                hoverinfo = "text",
                text = "Click for more info"
        ) %>% 
        config(p = ., staticPlot = FALSE, displayModeBar = FALSE, workspace = FALSE, 
               editable = FALSE, sendData = FALSE, displaylogo = FALSE
        ) %>%
        layout(title = 'Trash connected to nearby Google Places',
               xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(title = "Shows Google Places within the blue circle", showgrid = FALSE,
                            zeroline = FALSE, showticklabels = FALSE))
      })
      
      # Update Type Piechart
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
      
      # Update Brand Piechart
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
      
      # Add distance circle
      map %>% 
        clearGroup('circles') %>%
        addCircles(lat = click$lat, lng = click$lng, radius = input$distanceSlider, group = "circles", fill = FALSE)
      
      output$graphButton <- renderUI({
        if(!is.data.frame(analyzation))
          return()
          actionButton("showStatistics", HTML("&#10064;"), class = "btn btn-success", style="height: 2.8em")
      })
  
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
    map %>% clearGroup('circles') %>% clearGroup('placemarkers')
    
    # Remove everything else
    #@TODO 2
  })
  
  # Barchart click
  observe({
    click <- event_data("plotly_click")
    
    if(is.null(click) || !is.data.frame(analyzation))
      return()
    response <- locationSearch(click$x)
    output$details <- renderUI({
      HTML(paste0(
        '<h3>', response$result$name, '</h3>',
        '<img src="', response$result$icon, '" style="margin:1em;" />',
        '<p><strong>Address:&nbsp </strong>', response$result$formatted_address, '</p>',
        '<p><strong>Phone:&nbsp </strong>', response$result$formatted_phone_number, '</p>',
        '<p><strong>Website:&nbsp </strong><a href="', response$result$website, '">', response$result$website, '</a></p>',
        '<p><strong>Rating:&nbsp </strong>', response$result$rating, '</p>',
        '<hr/>'
      ))
    })
    
    map %>%
      addMarkers(
        group = 'placemarkers',
        lng = response$result$geometry$location$lng,
        lat = response$result$geometry$location$lat,
        popup = response$result$name,
        icon = makeIcon(
          iconUrl = response$result$icon,
          iconWidth = 38, iconHeight = 40
        )
      )
    
  })
  
  # Barchart hover
  #@TODO 1
  
  
  ########################
  #       Buttons        #
  ########################
  
  # Button Explore
  observeEvent(input$explore, {
    updateNavbarPage(session, "Trashtracking", "Map")
  })

})
