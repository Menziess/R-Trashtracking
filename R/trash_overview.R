library(shiny)
library(shinydashboard)
library(plyr)
library(ggplot2)
library(scales)

# trash <- read.csv(file="C:/Users/somaye/Documents/output.csv",head=TRUE,sep=",")
trash <- read.csv('../Data/output.csv', head = T, sep = ",") 

# Header -----------------------------------------------------------

header <- dashboardHeader(title= "OverView")


# Sidebar --------------------------------------------------------------

sm <- sidebarMenu(
  
  menuItem(
    text="stacked bar chart",
    tabName="chart",
    icon=icon("eye")
  )  
  
)

sidebar <- dashboardSidebar(sm)

# Body --------------------------------------------------

body <- dashboardBody(
  
  # Layout  --------------------------------------------  
  
  tabItems(
    tabItem(
      tabName="chart",
      fluidPage(
        
        fluidRow(
          
          title = "Inputs", status = "warning", width = 2, solidHeader = TRUE, collapsible = TRUE,
          plotOutput("M1"),
          dataTableOutput(outputId="M3")
          
          
        )
      )
    )
  )
)

# Setup Shiny app UI components -------------------------------------------

ui <- dashboardPage(header, sidebar, body)

# Setup Shiny app back-end components -------------------------------------

server <- function(input, output) {

  # ----------------------------------------------------------------------------- 
  #reproducable data generation
  Mdata <- reactive({
    
    tabletype = table(trash$brand, trash$type)
    types = as.data.frame(tabletype)
    names(types)[1] = 'brand'
    names(types)[2] = 'type'
    names(types)[3] = 'amount'
    types <- types[order(-types$amount),]
    types <- filter(types,grepl('Red Bull|Heineken|Coca Cola|AH|AA|Spa|Amstel|Slammers',brand))
    # Calculate the percentages
    df = ddply(types, .(brand), transform, percent = round((amount/sum(amount) * 100),1))
    
    #create nice labes
    #df$label = paste0(sprintf("%.0f", df$percent), "%")
    #return(df)
  })
  
  
  
  output$M1 <- renderPlot({ 
    
    ggplot(Mdata(), aes(x=reorder(brand,amount,function(x)+sum(x)), y=percent, fill=type))+
      geom_bar(position = "fill", stat='identity',  width = .7)+
      geom_text(aes(label=percent, ymax=100, ymin=0), vjust=0, hjust=2, color = "white",  position=position_fill())+
      coord_flip() +
      scale_y_continuous(labels = percent_format())+
      ylab("")+
      xlab("")
    
  })  
  
  output$M3 <- renderDataTable({
    Mdata()
  })  
  
  
  # -----------------------------------------------------------------------------
  
  
}

# Render Shiny app --------------------------------------------------------

shinyApp(ui, server)
