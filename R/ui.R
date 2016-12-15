
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
            # checkboxInput("checkboxLocationInput", "Show Places on the map", value = T),
          )
        ),
        # Info footer
        div(class = "panel-footer", 
          textOutput("text")
        )
      )
    ),
    
    # Sidebar
    absolutePanel(id="collapse_sidebar", class = "panel panel-primary", top = "3.7em", right = 0, style = "width: 5em; height: 100%; position: fixed;",
      div(class="panel-body", style="height:100%; display: block",
        div(bottom=0,
          HTML('<button class="btn btn-primary" onClick="openSidebar()" style="height:2.7em;">
                &#10010;</button>'),
          uiOutput("graphButton")
        ),
        div(id="sidebar_content", style="display: none",
          helpText("Lorem ipsum dolor sit amet, per eius ullum ne, per ea eligendi voluptatum. Ad suas menandri eum, at per movet utinam, at iriure omittam mel. Novum dolor ocurreret mea ad, modo augue feugait est ne. Per ad error tritani. Ius ignota legendos disputando et. Ad cum corpora luptatum hendrerit, qui cu saperet percipit persequeris. Per inani dicam utroque ea. Ei agam libris cum, te esse laboramus nec, ex quo eripuit maiorum. Te has delectus periculis, eu assum populo commune eos. Sit no nonumy aliquip scripserit, ex eripuit blandit facilisi duo. Ut usu repudiare intellegat, te sit augue dicta equidem. In mel libris impedit deserunt. Vix maiorum splendide at, has et error movet qualisque. Qui eu sanctus minimum placerat, facer verterem intellegat id mel. Munere dissentiet consequuntur nec eu, et aeque iusto ocurreret sit, eam error molestie maluisset ut. An vitae referrentur usu. In tibique postulant repudiandae qui, ea vis utamur salutatus urbanitas. His dolores copiosae inciderint in, eam legimus honestatis mediocritatem at. Audiam impedit quo an, pri duis aperiam moderatius cu, nec exerci cetero corpora te. Est mucius mandamus ne."),
          plotlyOutput("plot")
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
          condition="!output.large_plot",
          helpText("Oops! Sorry we can not show statistics if you don't select an area first.
                   Please go to back to the map-tab and click on the place you would
                   like to see statistics about, then click on the Stats button."),
          actionButton("showMap", "Show me the trash on a map", class = "btn-success btn-lg", style = "margin-top: 2em;")
        ),
        plotlyOutput("large_plot")
      ),
      conditionalPanel(
        condition="output.large_plot && !output.details",
        helpText("Click on a bar to see the details.")
      ),
      column(6, align = "center", style = "margin-left: 32%;", 
        div(class = "panel-body text-left",
          htmlOutput("details")
        )
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

