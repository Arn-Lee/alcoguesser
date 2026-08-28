# Import libs

library(tidyverse)
library(shiny)
library(shinyjs)
library(rhandsontable)
library(MASS)

# Define global vars
# NOTE: specifying non-int numbers for time (otherwise it gets stuck on int)
init_df = data.frame(time = c(0.0, 1.0, 2.0, 3.0, 4.0), 
                     sg = c(1.078, 1.074, 1.07, 1.065, 1.062)) # initial df with example data