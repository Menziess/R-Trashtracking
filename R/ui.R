
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
        
      ########################
      #   Links and Scripts  #
      ########################
        
      tags$head(
        includeCSS("styles.css")
        # includeScript("script.js")
      ),
    
      ########################
      #      Leaflet Map     #
      ########################
      
      leafletOutput("map", width = "100%", height = "100%"),
      absolutePanel(class = "panel panel-primary", bottom = 0, left = 10, draggable = T,
        div(class = "panel-heading", "Controls"),
        div(class = "panel-body",
         
          # Trash  
          h4("Trash"),
          dateRangeInput("daterange", NULL, start = "2015-05-31", end = "2016-03-15"),
          helpText("Choose type or brand"),
          uiOutput("trashTypeInput"),
          uiOutput("trashBrandInput"),
          hr(), 
          
          # Places
          h4("Google Places"),
          uiOutput("locationTypeInput"),
          helpText("Distance in meters"),
          sliderInput("distanceSlider", NULL,
                      min = 100, max = 2500, value = 1000),
          checkboxInput("checkboxLocationInput", "Show Places on the map", value = T),
          hr(),
          
          # Button
          actionButton("showDetails", "Show Details"),
          actionButton("showLocationDetails", "Show Location")
        ),
        div(class = "panel-footer", 
          textOutput("text")
        )
      ),
      absolutePanel(class = "panel panel-primary", bottom = 0, right = 10, draggable = T,
        div(class="pie",
          plotlyOutput("pie")
        )
      )
    )
  ),
  
  ########################
  #         Table        #
  ########################
  
  tabPanel("Details",
    h2(textOutput("story")),
    div(class="tabel",
      dataTableOutput("table")
    )
  ),
  
  ########################
  #     Details Page     #
  ########################
  
  tabPanel("LocationDetails",
   div(class = "panel-body",
       h3(textOutput("LocationName")),
       hr(),
       textOutput("LocationAdress")
   )
  ),
  
  ########################
  #        Graph         #
  ########################
  
  tabPanel("Plot",
    div(class="plot",
      plotlyOutput("plot")
    )
  )
))