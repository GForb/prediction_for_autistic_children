# Setting paths ----
# To set paths the global option "pfac_path" must be set with the path to the data and outputs folder. 
# This can be done with the command options(pfac_path = "*path to data*"). This only needs to be run once.

data_and_outputs <-  Sys.getenv("pfac_path")
raw_data <- file.path(data_and_outputs, "Raw data")
derived_data <- file.path(data_and_outputs, "Derived data")
outputs <- file.path(data_and_outputs, "Outputs")

# Loading required libraries
library(here)
library(tidyverse)
library(haven) 
library(gridExtra) 
library(ggdist) 
library(ggthemes) 
library(gifski) 
library(magick) 
library(gt) 
library(gtExtras)
library(testthat)

# Loading variable metadata ----
var_metadata <- utils::read.csv(here::here("variable_metadata.csv"))



# Sourcing functions ----
functions_dir <-"R scripts/Functions"
functions <- list.files(here::here(functions_dir))
ignore_fuctions <- c("Tests")
for (fun in functions) {
  if (!(fun %in% ignore_fuctions)) {
    source(here::here(functions_dir, fun))
  }
}
