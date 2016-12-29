# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

source('require.R')

shinyUI(
  navbarPage("Trashtracking",
  
             
    ########################
    #         Map          #
    ########################
    
    tabPanel("Map", 
      htmlOutput("modal"),
      div(class="leaflet-bar", style="position:absolute;z-index:1;transform:translate(2em, -1em);",
        HTML('<button class="btn btn-info" data-toggle="modal" data-target="#myModal" style="height:2.7em;width:3.1em;">
              ?</button>')
      ),
      div(class="map",
          
      
        ########################
        #       Leaflet        #
        ########################
        
        leafletOutput("map", width = "100%", height = "100%"),
        
        # Filter panel
        absolutePanel(class = "panel panel-primary", style = "width: 20em;", left = 10, bottom = 0, draggable = F,
          div(class = "panel-heading", "Filters",
            div(style="margin-top: 0px;",
              HTML('<button class="btn btn-primary" 
                    style="top:0;right:0;position:absolute;height:2.7em;"
                    data-toggle="collapse" data-target="#collapse_controls">
                    &#10010;</button>'),
              HTML('<button id="reset" class="btn btn-primary action-button" 
                    style="top:0;right:3.2em;position:absolute;height:2.7em;"">
                    clear</button>')
            )
          ),
          div(class = "panel-body", style="padding:0 15px 0 15px;",
            div(id = "collapse_controls", class = "collapse in",
           
              # Trash  
              helpText("Filter dates"),
              dateRangeInput("daterange", NULL, start = "2015-05-31", end = "2016-03-15"),
              helpText("Choose type"),
              uiOutput("trashTypeInput"),
              helpText("Choose brand"),
              uiOutput("trashBrandInput"),
              hr(), 
              
              # Places
              helpText("What are you searching for?"),
              uiOutput("locationTypeInput"),
              helpText("Distance in meters"),
              sliderInput("distanceSlider", NULL, min = 100, max = 2500, value = 200)
            )
          ),
          # Info footer
          div(class = "panel-footer", 
            textOutput("text")
          )
        )
      ),
  
      # Sidebar panel
      absolutePanel(id="collapse_sidebar", class = "collapsed-sidebar panel panel-primary", top = "3.7em", right = 0, style = "width: 35em; height: 100%; position: fixed;",
        div(class="panel-body", style="height:100%; display: block",
          div(style="position:fixed;z-index:1;-webkit-transform: translateZ(0);width:32.5em;",
            HTML('<button class="btn btn-primary" onClick="openSidebar()" style="height:2.7em;">
                 &#10010;</button>')
          ),
          
          # Show overview of all data
          div(id="toggle-overview",
            h3("Click near trash clusters", class="text-center text-success"),
            div(
              plotlyOutput("overview")
            )
          ),
          
          # Show analysis
          div(id="toggle-analysis", hidden = "hidden",
            h3("Analysis", class="text-center text-success"),
            # div(
              # uiOutput("streetImage"),
              # htmlOutput("streetView")
            # ),
            div(
              plotlyOutput("plot")
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
    ),
    
    
    ########################
    #         More         #
    ########################
    
    navbarMenu("More",
      tabPanel("About",
        column(12, align = "center",
               h4("Thanks to the trashhunters we know."),
               helpText("To become a hunter download the Trash Hunters app for free, 
                        now available in the App and Playstore!"),
               actionButton("explore", "Explore", class = "btn-success btn-lg", style = "margin-top: 2em;")
        )
      ),
      "----",
      tabPanel("Search", align = "center",
        h3("Search", class="text-center text-success"),
        div(class="form-inline",
          div(class="form-group",
            textInput("lat", NULL, NULL, 200, "lat")
          ),
          div(class="form-group",
            textInput("lng", NULL, NULL, 200, "lng")
          ),
          div(class="form-group", style="display:inline-block;",
            actionButton("latlng", "Go", NULL)
          )
        )
      )
    ),
    
    
    ########################
    #   Links and Scripts  #
    ########################
    
    tags$head(
      includeCSS("styles.css"),
      includeScript("script.js")
    ),
    
    theme = shinytheme("flatly"),
    useShinyjs(),
    selected = "Map",
    id = "Trashtracking"
  )
)


