
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

source('require.R')

shinyUI(navbarPage("Trashtracking", 
  theme = shinytheme("flatly"),
  selected = "Map",
  id = "Trashtracking",
  
  ########################
  #      Scatterplot     #
  ########################
  
  tabPanel("Overview",
    div(class="scatterplot",
      column(12, align = "center",
        h1("Scatterplot TODO"),
        plotOutput("scatterplot"),
        actionButton("showMap", "Show Map", class = "btn-success btn-lg", style = "margin-top: 2em;")
      )
    )
  ),
  
  ########################
  #         Map          #
  ########################
  
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
      #       Leaflet        #
      ########################
      
      leafletOutput("map", width = "100%", height = "100%"),
      absolutePanel(class = "panel panel-primary", style = "width: 20em;", bottom = 0, left = 10, draggable = F,
        div(class = "panel-heading", "Controls", 
            HTML('<button class="btn btn-danger pull-right" data-toggle="collapse" data-target="#collapse">filter</button>')
        ),
        div(class = "panel-body",
          div(id = "collapse", class = "collapse",
         
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
            # checkboxInput("checkboxLocationInput", "Show Places on the map", value = T),
            uiOutput("graphButton")
          )
        ),
        div(class = "panel-footer", 
            textOutput("text")
        )
      )
    )
  ),
  ########################
  #      Statistics      #
  ########################
  
  tabPanel("Statistics",
    textOutput("print"),
    column(12, align = "center",
      div(class="plot",
        h4("Trash information of the selected area"),
        conditionalPanel(
          condition="!output.plot",
          helpText("Oops! Sorry we can not show statistics if you don't select an area first.
          Please go to back to the map-tab and click on the place you would 
                   like to see statistics about, then click on the Stats button"),
          actionButton("showMap", "Show me the trash on a map",
                       class = "btn-success btn-lg", style = "margin-top: 2em;")
        ), 
        plotlyOutput("plot")
      )
    ),
    column(6,
      div(class="pie",
        plotlyOutput("pie_trash_type")
      )
    ),
    column(6,
      div(class="pie",
        plotlyOutput("pie_trash_brand")
      )
    )
  ),
  
  ########################
  #     Details Page     #
  ########################
  
  tabPanel("Details",
    column(12, align = "center",
      h4("Place details")
    ,
    conditionalPanel(
      condition="!output.plot",
      helpText("Oops! Sorry we can not show any details if you don't select an area first.
          Please go back to the map-tab and click on the
               place you would like to know more details about."),
      actionButton("showMap", "Show me the trash on a map",
                   class = "btn-success btn-lg", style = "margin-top: 2em;"))
      , 
    div(class = "panel-body",
      h3(textOutput("LocationName")),
      hr(),
      textOutput("LocationAdress")
    ))
  ),
  
  ########################
  #         Table        #
  ########################
  
  tabPanel("</>",
    column(12, align = "center",
    h1("Places with trash"),
      div(class="tabel",
         dataTableOutput("table")
      )
    )
  )
))
