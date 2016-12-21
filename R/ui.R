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
  #       Overview      #
  ########################
  
  tabPanel("Overview",
    div(class="scatterplot",
      column(12, align = "center",
             h4("Thanks to the trash hunters we know.."),
             helpText("To become a hunter download the Trash Hunters app for free, 
                      now available in the App and Playstore!"),
             plotOutput("overview"),
             helpText("This graph shows the all time top 10 brands that are found"),
             actionButton("explore", "Explore", class = "btn-success btn-lg", style = "margin-top: 2em;")
             ),
      column(12, align = "center",
             dataTableOutput(outputId="M3")
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
        includeCSS("styles.css"),
        includeScript("script.js")
      ),
    
      ########################
      #       Leaflet        #
      ########################
      
      leafletOutput("map", width = "100%", height = "100%"),
      absolutePanel(class = "panel panel-primary", style = "width: 20em;", left = 10, bottom = 0, draggable = F,
        div(class = "panel-heading", "Controls",
          div(style="margin-top: 0px;",
            HTML('<button class="btn btn-primary" style="top:0;right:0;position:absolute;height:2.7em;" 
                  data-toggle="collapse" data-target="#collapse_controls">
                  &#10010;</button>')
          )
        ),
        div(class = "panel-body",
          div(id = "collapse_controls", class = "collapse in",
         
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
            sliderInput("distanceSlider", NULL, min = 100, max = 2500, value = 1000)
          )
        ),
        # Info footer
        div(class = "panel-footer", 
          textOutput("text")
        )
      )
    ),
    
    # Sidebar
    absolutePanel(id="collapse_sidebar", class = "collapsed-sidebar panel panel-primary", top = "3.7em", right = 0, style = "width: 35em; height: 100%; position: fixed;",
      div(class="panel-body", style="height:100%; display: block",
        div(bottom=0,
          HTML('<button class="btn btn-primary" onClick="openSidebar()" style="height:2.7em;">
                &#10010;</button>')
        ),
        conditionalPanel(
          condition = "!output.plot",
          helpText("")
        ),
        htmlOutput("details"),
        div(
          plotlyOutput("plot"),
          HTML('<a href="#" data-toggle="popover" title="Popover Header" data-content="Some content inside the popover">Toggle popover</a>')
        ),
        div(id="pie_type",
            plotlyOutput("pie_trash_type")   
        ),
        div(id="pie_brand_type",
            plotlyOutput("pie_trash_brand")
        )
      )
    )
  )
  
)
)


