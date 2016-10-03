
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

source('require.R')

trash <- read.csv('../Data/output.csv')
trash <- head(trash, 10)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

shinyServer(function(input, output) {

  points <- eventReactive(input$recalc, {
    cbind(trash$longitude, trash$latitude)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())
  })

})
