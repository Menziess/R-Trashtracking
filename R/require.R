# Install required packages

options(shiny.trace = T)

list.of.packages <- c("shiny", "XML", "dplyr", "mlxR", "leaflet", "httr", "xml2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(XML)
library(dplyr)
library(mlxR)
library(leaflet)
library(httr)
library(xml2)