rm(list = ls()) 

# Packages ----
library(shiny)
library(shinydashboard)
library(data.table)
library(DT)
library(plotly)
library(readxl)

dashboard.naam <- "Ranking the outcomes"

csvFile <- "Quickscan VBHC.csv"

domains <- c("Relevantie", "Impact", "Volume")

colors <- c("#4DB6AC", "#4FC3F7", "#9575CD")
