
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

source('require.R')

shinyUI(navbarPage("Trashtracking",
  tabPanel("Map", 
           
    div(class="map",
        
      # Include custom CSS
      tags$head(
        includeCSS("styles.css")
        # includeScript("script.js")
      ),
    
      # Leaflet map
      leafletOutput("map", width = "100%", height = "100%"),
      absolutePanel(class = "panel panel-primary", bottom = -150, right = -40, draggable = T,
        div(class = "panel-heading", "Controls"),
        div(class = "panel-body",
          selectInput("type", NULL, choices = c("All")),
          dateRangeInput("daterange1", "Datum",
                         start = "2015-05-01",
                         end   = "2016-03-31"),
         selectInput("variable", "Type afval:",
                      c("aluminium blikjes" = "cyl",
                        "Kartonnen pakje" = "am",
                        "PET blikje" = "gear",
                        "Plastic fles" = "fles")),
          selectInput("variable", "Merk: ",
                      c("Fernandes" = "cyl",
                        "Coca Cole" = "am",
                        "Fanta" = "gear")),
          actionButton("showGraphs", "Weergeef resultaat")
        ),
        div(class = "panel-footer", 
          # Feedback for the user
          textOutput("text")
        )
      )
    )
  ),
  tabPanel("Details",
    div(class="tabel",
      dataTableOutput("table")
    )
  )
))