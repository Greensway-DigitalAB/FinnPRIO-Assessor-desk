# production <- FALSE
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

library(mc2d) # for monte carlo simulations

### For .docx reports
library(officer)
# library(crosstable)
# options(crosstable_style_list_unordered="unordered_list")
library(flextable)

## Set options
op <- options(digits.secs = 0)
