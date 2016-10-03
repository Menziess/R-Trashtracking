# Install required packages

list.of.packages <- c("shiny", "XML", "dplyr", "ggplot2", "httr", "shinydashboard", "mlxR")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(XML)
library(dplyr)
library(ggplot2)
library(httr)
library(shinydashboard)
library(mlxR)