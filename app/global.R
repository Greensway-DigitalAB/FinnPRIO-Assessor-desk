library(shiny)
library(shinyjs)
library(shinyFiles)
library(shinythemes)
library(shinyalert)
library(shinyWidgets)

library(DT)
library(DBI)
library(RSQLite) # swap with RPostgres/MySQL if needed
library(tidyverse)
library(glue)
library(jsonlite)

# For monte carlo simulations
library(mc2d) 

# For .docx reports
library(officer)
library(flextable)

## Set options
op <- options(digits.secs = 0)
