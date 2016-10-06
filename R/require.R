# Install required packages

list.of.packages <- c("shiny", "XML", "dplyr", "mlxR", "leaflet", "httr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(XML)
library(dplyr)
library(mlxR)
library(leaflet)
library(httr)