# Install required packages

options(shiny.trace = T)
# options(error=recover)

list.of.packages <- c("shiny", "shinyjs", "XML", "plyr", "ggplot2", "scales", "dplyr", "leaflet", "curl", "httr", "xml2", "geosphere", "plotly", "shinythemes", "RColorBrewer", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
for (i in list.of.packages) {
  do.call("library", list(i))
}
