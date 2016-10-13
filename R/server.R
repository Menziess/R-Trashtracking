
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

source('externalAPI.R')

trash <- read.csv('../Data/output.csv')
trash <- filter(trash, latitude != 0 & latitude != 1 & longitude != 0 & longitude != 1)

  ## EXAMPLE GOOGLE REQUEST
  #test <- head(trash, 1)
  #googleData <- googlePlaces(test$latitude, test$longitude, radius = 500, types = NULL, name = NULL)

shinyServer(function(input, output) {

  points <- eventReactive(input$recalc, {
    cbind(trash$longitude, trash$latitude)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(clusterOptions = markerClusterOptions(), data = points())
  })
})
