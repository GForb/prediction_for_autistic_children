# Setting paths ----
# To set paths the global option "pfac_path" must be set with the path to the data and outputs folder. 
# This can be done with the command options(pfac_path = "*path to data*"). This only needs to be run once.

data_and_outputs <-  Sys.getenv("pfac_path")
raw_data <- file.path(data_and_outputs, "Raw data")
derived_data <- file.path(data_and_outputs, "Derived data")
outputs <- file.path(data_and_outputs, "Outputs")

# Loading required libraries
library(here)
library(testthat)
library(haven) 
library(gridExtra) 
library(ggdist) 
library(ggthemes) 
library(gifski) 
library(magick) 
library(gt) 
library(gtExtras)
library(tidyverse)
filter <- dplyr::filter

# Loading variable metadata ----
var_metadata <- utils::read.csv(here::here("variable_metadata.csv"))



# Sourcing functions ----
source_functions <- function() {
  function_dir <- "R scripts/Functions"
  for (fun in list.files(here::here(function_dir), pattern="\\.R$")) {
    source(here::here(function_dir, fun))
  }
  
}
source_functions()

# Test function ----
test <- function(filename = NULL) {
  if (is.null(filename)) {
    test_dir <- "R scripts/Functions/Tests"
    for (test in list.files(here::here(test_dir), pattern="\\.R$")) {
      print(paste("Running tests in", test))
      source(here::here(test_dir, test))
    }
  } else {
    source(here::here(test_dir, filename))
  }
}


