
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

trash <- read.csv('../Data/output.csv')

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

# Clustering and externalAPI logic should be static methods assuming a trash data set

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
