
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

source('require.R')

shinyUI(navbarPage("Trashtracking",
  tabPanel("Map", 
           
    div(class="outer",
        
      # Include custom CSS
      tags$head(
        includeCSS("styles.css")
      ),
    
      # Leaflet map
      leafletOutput("map", width = "100%", height = "100%"),
      absolutePanel(class = "panel panel-primary", bottom = 20, right = 20, draggable = T,
        div(class = "panel-heading", "Controls"),
        div(class = "panel-body",
          selectInput("type", NULL, choices = c("cola")),
          
          # Feedback developer text
          textOutput("text")
        ),
        div(class = "panel-footer", "Footer")
      )
    )
  )
))