# production <- FALSE
library(shiny)
library(shinyjs)
library(shinyFiles)
library(shinyalert)
library(shinyWidgets)

library(DT)
library(DBI)
library(RSQLite) # swap with RPostgres/MySQL if needed
library(tidyverse)
library(glue)
library(jsonlite)

library(mc2d) # for monte carlo simulations

op <- options(digits.secs = 0)
