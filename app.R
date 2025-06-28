# app.R

# Load libraries
library(shiny)
library(ggplot2)
library(scales)
library(dplyr)
library(xts)
library(zoo)

# Source your logic and UI
source("1.Pairwise.R")      # contains data loading or calculations
source("2.aPairwiseDark.R") # contains ui and server

# Run the app
shinyApp(ui = ui, server = server)
