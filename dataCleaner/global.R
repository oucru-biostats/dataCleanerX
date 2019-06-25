# This is an application to detect errorneous data in the dataset.

library(shiny)
library(shinyWidgets)
library(V8)
library(shinyjs)
library(tidyverse)
library(DT)
# library(DTedits) #folked, not load
library(rhandsontable)
library(readxl)
library(dataMaid)
library(hunspell)
library(stringr)
library(jsonlite)
library(tools)
library(rlist)
library(vroom)
library(promises)
library(future)
plan(multiprocess)

# dev <- TRUE