# Install required packages

options(shiny.trace = T)

list.of.packages <- c("shiny", "XML", "dplyr", "leaflet", "httr", "xml2", "foreach", "doParallel", "geosphere", "methods")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(XML)
library(dplyr)
library(leaflet)
library(httr)
library(xml2)
library(foreach)
library(doParallel)
library(geosphere)
library(methods)