
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

source('require.R')

shinyUI(navbarPage("Trashtracking", 
  id = "Trashtracking",
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

         
            # input user  
          dateRangeInput("daterange1", "About the Trash..",
                         start = "2015-05-01",
                         end   = "2016-03-31"),
          helpText(" Choose a type or brand"),
          uiOutput("trashTypeInput"),
          uiOutput("trashBrandInput"),
          hr(),
          uiOutput("locationTypeInput"),
          checkboxInput("checkboxLocationInput", "Show locations on map", value = FALSE),
          sliderInput("distanceSlider", "Afstand",
                      min = 100, max = 2500, value = 1000),
          hr(),
          actionButton("showDetails", "Show Details")
        ),
        div(class = "panel-footer", 
          # Feedback for the user
          textOutput("locaties"),
          textOutput("text")
        )
      )
    )
  ),
  tabPanel("Details",
    div(class="tabel",
      dataTableOutput("table")
    )
  ),
  tabPanel("Graph",
    div(class="graph",
      plotOutput("graph")
    )
  )
))