# Install required packages

options(shiny.trace = T)
# options(error=recover)

list.of.packages <- c("shiny", "XML", "dplyr", "leaflet", "httr", "xml2", "geosphere", "plotly", "shinythemes",
                      "ggplot2", "scales", "shinydashboard", "plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
for (i in list.of.packages) {
  do.call("library", list(i))
}