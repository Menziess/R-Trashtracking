  
  ##
  # This is the server logic for a Shiny web application.
  # You can find out more about building applications with Shiny here:
  #
  # http://shiny.rstudio.com
  #

source('externalAPI.R')

# Load data
tryCatch({trash <- load("../Data/trash.RData")}, 
         warning = function(w) {print(w)},
         error = function(e) {print(e)},
         finally = {
           trash <- read.csv('../Data/output.csv')
           trash <- filter(trash, latitude != 0 & latitude != 1 & longitude != 0 & longitude != 1)
           save(trash, file = "../Data/trash.RData")
         })


# Shiny server
shinyServer(function(input, output, session) {
  
  
  ########################
  # Initial Leaflet Map  #
  ########################

  # Empty leaflet map
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

  # Reactive data filter
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
  trashbrands <- filter(trash,grepl('Red Bull|Heineken|Coca Cola|AH|AA|Spa|Amstel|Slammers',brand))
  output$trashBrandInput = renderUI({
    names <- distinct(trashbrands, brand)
    selectInput("trashBrand", NULL, c('All', as.character(names$brand)))
  })
  
  # Google places filter
  output$locationTypeInput = renderUI({
    locationTypes <-  c("All" = "?",
                        "Afhaalrestaurants" = "meal_takeaway",
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
  #   Default outputs   #
  #######################
  
  # Plot overview
  output$overview <- renderPlotly({
    
    Mdata <- reactive({
      tabletype = table(trash$brand, trash$type)
      types = as.data.frame(tabletype)
      names(types)[1] = 'brand'
      names(types)[2] = 'type'
      names(types)[3] = 'amount'
      types <- types[order(-types$amount),]
      types <- filter(types, grepl('Red Bull|Heineken|Coca Cola|AH|AA|Spa|Amstel|Slammers',brand))
      df = ddply(types, .(brand), transform, percent = round((amount/sum(amount) * 100),1))
    })
    
    colors <- colorRampPalette(brewer.pal(4,"Reds"))(10)
    ggplot(Mdata(), aes(x=reorder(brand,amount,function(x)+sum(x)), y=percent, fill=type))+
      geom_bar(position = "fill", stat='identity',  width = .7)+
      geom_text(aes(label=percent, ymax=100, ymin=0), vjust=0, hjust=2, color = "white",  position=position_fill())+
      coord_flip() +
      scale_y_continuous(labels = percent_format())+
      ylab("")+
      xlab("")
    
    #######################
    #    Dezelfde data    #
    #######################
    
    # df <- trash %>%
    #   filter(grepl('Red Bull|Heineken|Coca Cola|AH|AA|Spa|Amstel|Slammers',brand)) %>%
    #   group_by(brand, type) %>%
    #   summarise(n = n()) %>%
    #   mutate(percentage = round(n / sum(n) * 100, 1))
    # 
    # 
    # plot_ly(df, r = ~percentage, t = ~type) %>% add_area(color = ~brand)
    
    
    #######################
    #  ggplot2 in plotly  #
    #######################
    
    # ggplotly(ggplot(df, aes(x=reorder(brand,n,function(x)+sum(x)), y=percentage, fill=type))+
    #   geom_bar(position = "fill", stat='identity',  width = .7)+
    #   geom_text(aes(label=percentage, ymax=100, ymin=0), vjust=0, hjust=2, color = "white",  position=position_fill())+
    #   coord_flip() +
    #   scale_y_continuous(labels = percent_format())+
    #   ylab("")+
    #   xlab("")
    # ) %>%
    # config(p = ., staticPlot = FALSE, displayModeBar = FALSE, workspace = FALSE,
    #        editable = FALSE, sendData = FALSE, displaylogo = FALSE
    # ) %>%
    # layout(width = 500, legend = list(x = 0.1, y = 0.9),
    #        xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
    #        yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

  })
  
  tabledate = table(trash$dates)
  dateinfo = as.data.frame(tabledate)
  names(dateinfo)[1] = 'date'
  names(dateinfo)[2] = 'amount'
  
  dateinformation <- setDT(dateinfo)[, date := as.IDate(date)][, .(mn_amt = round(mean(amount))), by = .(yr = year(date), mon = month(date))]
  dateinformation <- transform(dateinformation, month = month.abb[mon])
  dateinformation <- dateinformation[,c(3,4)]
  dateinformation <- dateinformation[2:11,]
  
  output$datePlot <- renderPlot({
    
    barplot(height=dateinformation$mn_amt,names.arg=dateinformation$month,
            col = 'navajowhite', border = 'navajowhite3',    
            main= paste("Trash by month"),
            ylab="Number of trash produced",
            xlab="months",
            ylim = c(0,50)
    )
  })
  
  # Modal content
  output$modal <- renderUI({
    HTML(paste0(
      '<div id="myModal" class="modal fade" style="top:1em;" role="dialog">',
      '<div class="modal-dialog">',
      '<div class="modal-content">',
      '<div class="modal-header">',
      '<button type="button" class="close" data-dismiss="modal">&times;</button>',
      '<h4 class="modal-title">Hello there!</h4>',
      '</div>',
      '<div class="modal-body text-center">',
      '<p>Start by clicking on a blank space on the map.</p>',
      '<img src="instruction1.png" />',
      '<p style="margin-top: 1em;">The system will locate interesting places that may a correlation with the trash in that area.</p>',
      '<img src="instruction2.png" />',
      '</div>',
      '<div class="modal-footer">',
      '<button type="button" class="btn btn-default" data-dismiss="modal">Got it</button>',
      '</div>',
      '</div>',
      '</div>',
      '</div>'
    ))
  })
  
    
  #######################
  #      Observers      #
  #######################
  
  # Input type changes
  observe({
    input$type
    icon <- makeIcon(
      iconUrl = "marker.png",
      iconWidth = 40, iconHeight = 40
    )
    map <<- leafletProxy("map", data = filteredData()) %>%
      removeMarkerCluster('trash') %>%
      addMarkers(
        clusterId = 'trash',
        clusterOptions = markerClusterOptions(), 
        popup = ~as.character(paste(type, brand)),
        icon = icon
      )
  })
  
  # Url parameters
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(is.null(query$lat) && is.null(query$lng))
      return()
    if(query$lat == "" || query$lng == "")
      return()
    if(is.double(lat <- as.double(query$lat)) && is.double(lng <- as.double(query$lng)))
      performMapSearch(lat, lng, query$distance, query$type)
  })
  
  # Map zoom
  observeEvent(input$map_zoom, {
    e <- input$map_zoom
    if(is.null(e))
      return()
    output$text <- renderText(paste("Zoom: ", e))
  })
  
  # Map click
  observeEvent(input$map_click, {
    click <- input$map_click
    if(is.null(click))
      return()
    performMapSearch(click$lat, click$lng)
  })
  
  # Marker click
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if(is.null(click))
      return()
    output$text <- renderText(paste("Marker at Lat:", round(click$lat, 4), "- Lng:", round(click$lng, 4)))
  })
  
  # Shape click
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if(is.null(click))
      return()
    map %>% clearGroup('circles') %>% clearGroup('placemarkers')
    shinyjs::hide("toggle-analysis")
    shinyjs::show("toggle-overview")
  })
  
  # Barchart click
  observeEvent(event_data("plotly_click"), {
    click <- event_data("plotly_click")
    if(is.null(click) || !is.data.frame(googleData))
      return()
    output$details <- renderUI({
      HTML(paste0(
        '<hr/>',
        '<h3 display: inline-block><img src="', googleData[click$z, 4], '" height=40, width=40" />', googleData[click$z, 1], '</h3>',
        '<p><strong>Address:&nbsp </strong>', googleData[click$z, 5], '</p>',
        '<p><strong>Phone:&nbsp </strong>', googleData[click$z, 6], '</p>',
        '<p><strong>Website:&nbsp </strong><a href="', googleData[click$z, 7], '" target="blank">', googleData[click$z, 7], '</a></p>',
        '<hr/>'
      ))
    })
    
    map %>% 
      setView(googleData[click$z, 2], googleData[click$z, 3], getViewport(isolate(input$distanceSlider)) + 1)
  })
  
  
  ########################
  #       Buttons        #
  ########################
  
  # Dev actionbutton
  observeEvent(input$latlng, {
    if(input$lat == "" || input$lng == "")
      return()
    if((lat <- as.double(input$lat)) && (lng <- as.double(input$lng)))
      performMapSearch(lat, lng)
    updateNavbarPage(session, "Trashtracking", "Map")
  })
  
  # Button Explore
  observeEvent(input$explore, {
    updateNavbarPage(session, "Trashtracking", "Map")
  })
  
  # Reset actionbutton
  observeEvent(input$reset, {
    updateSliderInput(session, "distanceSlider", value=200)
    updateDateRangeInput(session, "daterange", start = "2015-05-31", end = "2016-03-15")
    updateSelectInput(session, "trashBrand", selected = "All")
    updateSelectInput(session, "trashType", selected = "All")
    updateSelectInput(session, "locationType", selected = "?")
    map %>% 
      setView(4.8999, 52.3724, 8) %>%  
      clearGroup('circles') %>%
      clearGroup('placemarkers')
  })
  
  
  ########################
  #       Map Search     #
  ########################
  
  performMapSearch <- function(lat, lng, distance = NULL, type = NULL) {

    # Add progress bar
    withProgress(message = "Getting Google Places", value = 0, {

    # Perform google radar search call
    if (!is.null(isolate(input$locationType)) || !is.null(type))
      type <- ifelse(is.null(type), isolate(input$locationType), as.character(type))
    if (!is.null(isolate(input$distanceSlider)) || !is.null(distance))
      distance  <- ifelse(is.null(distance), isolate(input$distanceSlider), as.integer(distance))
    places <- radarSearch(lat, lng, distance, type)
    nrResults <- length(places)
    
    if(nrResults == 0) {
      output$text <- renderText(paste("No places found in this area."))
    } else {
      
      # Flatten places from list to dataframe
      places <- do.call(rbind, lapply(places$results, data.frame, stringsAsFactors=FALSE))
      
      # Preperation as in filtering trash based on lat lng location with radius of distanceInLatLng
      incProgress(1/4, detail = "Filtering Trash")
      distanceInLatLng <- metersToLatLng(lat, lng, distance)
      trash <- filter(isolate(filteredData()), latitude > lat - distanceInLatLng[[1]] & latitude < lat + distanceInLatLng[[1]]
                      & longitude > lng - distanceInLatLng[[2]] & longitude < lng + distanceInLatLng[[2]])
      
      # Analyzation joining, counting distinct trash and places
      incProgress(2/4, detail = "Distance between Trash and Places")
      analyzation <- analyse(trash, places)
      googleData <<- retrievePlacesDetails(analyzation)
    } 
      
    # Add distance circle on the map
    map %>% 
      setView(lng, lat, getViewport(distance)) %>%
      clearGroup('circles') %>%
      clearGroup('placemarkers') %>%
      addCircles(lat = lat, lng = lng, radius = distance, group = "circles", fillOpacity = 0.05)
    
    # Retrieve detailed places information
    if (!is.null(googleData)) {
      map %>%
        addMarkers(
          data = googleData,
          group = 'placemarkers',
          lng = ~Lng,
          lat = ~Lat,
          popup = paste0(
            '<h3 display: inline-block><img src="', googleData$Icon, '" height=40, width=40" style="margin:0.5em 0.5em 0.5em 0em;" />', googleData$Name, '</h3>',
            '<p><strong>Address:&nbsp </strong>', googleData$Address, '</p>',
            '<p><strong>Phone:&nbsp </strong>', googleData$Phone, '</p>',
            '<p><strong>Website:&nbsp </strong><a href="', googleData$Website, '" target="blank">', googleData$Website, '</a></p>'
          ),
          icon = makeIcon(
            iconUrl = googleData$Icon,
            iconWidth = 38, iconHeight = 40
          )
        )
      
        shinyjs::hide("toggle-overview")
        shinyjs::show("toggle-analysis")
      } else {
        shinyjs::show("toggle-overview")
        shinyjs::hide("toggle-analysis")
      }
  
      # # Update Streetimage
      # output$streetImage <- renderUI({
      #   tags$img(src= streetImage(lat, lng))
      # })
      
      # Update Streetview
      # output$streetView <- renderUI({
      #   tags$iframe(src=streetView(lat, lng), width=600, height=400, frameborder = 0, style = "border:0;")
      # })
    
      # Update Places Barchart
      incProgress(3/4, detail = "Render graphs")
      output$plot <- renderPlotly({
        if(!is.data.frame(googleData))
          return()
        colors <- colorRampPalette(brewer.pal(4,"Greens"))(10)
        plot_ly(googleData,
                x = ~Name,
                y = ~Amount,
                z = ~Place,
                name = "Top 10 trash found at google places",
                colors = colors,
                type = "bar",
                hoverinfo = "text",
                text = ~paste(Amount, "trash found at", Name)
        ) %>% 
        config(p = ., staticPlot = FALSE, displayModeBar = FALSE, workspace = FALSE, 
               editable = FALSE, sendData = FALSE, displaylogo = FALSE
        ) %>%
        layout(title = paste(sum(googleData$Amount), "trash near", nrow(googleData), "places."),
               xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
               yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      })
      
      # Update Type Piechart
      output$pie_trash_type <- renderPlotly({
        if(!is.data.frame(googleData))
          return()
        colors <- colorRampPalette(brewer.pal(4,"Reds"))(10)
        plot_ly(head(trash %>% count(type, sort = T), 10),
                labels = ~type,
                values = ~n,
                marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)),
                name = "Trash distribution",
                type = "pie"
        ) %>% 
        config(p = ., staticPlot = FALSE, displayModeBar = FALSE, workspace = FALSE, 
               editable = FALSE, sendData = FALSE, displaylogo = FALSE
        ) %>%
        layout(title = 'Trash types within selected area',
               legend = list(x = 100, y = 0.5),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      })
      
      # Update Brand Piechart
      output$pie_trash_brand <- renderPlotly({
        if(!is.data.frame(googleData)) 
          return ()
        colors <- colorRampPalette(brewer.pal(9, "Purples"))(10)
        plot_ly(head(trash %>% count(brand, sort = T), 10),
                labels = ~brand,
                values = ~n,
                marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)), 
                name = "Trash distribution",
                type = "pie"
        ) %>% 
        config(p = ., staticPlot = FALSE, displayModeBar = FALSE, workspace = FALSE, 
               editable = FALSE, sendData = FALSE, displaylogo = FALSE
        ) %>%
        layout(title = 'Trash brands within selected area',
               legend = list(x = 100, y = 0.5),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      })
    })
  } 
  
  # Gets proper map viewport from distance input
  getViewport <- function(distance) {
    return(ifelse(distance<125, 18,
    ifelse(distance<250, 17,
    ifelse(distance<500, 16,
    ifelse(distance<1000, 15,
    ifelse(distance<2000, 14, 13))))))
  }
})

