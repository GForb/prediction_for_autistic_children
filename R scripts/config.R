# Setting paths ----
# To set paths the global option "pfac_path" must be set with the path to the data and outputs folder. 
# If using Rstudio, this can be done by adding  Sys.setenv(pfac_path = "*path to data*") to the .Renviron file. To edit the .Renviron file you can use  usethis::edit_r_environ().

# Data folders
data_and_outputs <-  Sys.getenv("pfac_path")
raw_data <- here::here(data_and_outputs, "Raw data")
derived_data <- here::here(data_and_outputs, "Derived data")

# Output folders
outputs <- here::here(data_and_outputs, "Outputs")
thesis_tables <- file.path(outputs, "Thesis Tables")
thesis_plots <- file.path(outputs, "Thesis Plots")

latex_folder <-  file.path(thesis_tables, "LatexTables")

# R scripts folders
data_processing_scripts <- here::here("R scripts", "Data processing")
modelling_scripts <- here::here("R scripts", "Modelling")
reporting_scripts <- here::here("R scripts", "Reporting")
thesis_reporting <- here::here("R scripts", "Thesis plots and tables")



#devtools::install_github("GForb/IPDPredictR")


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
library(qwraps2)
library(cowplot)
library(patchwork)
library(grid)
library(flextable)
library(rms)

filter <- dplyr::filter

# Loading metadata ----
var_metadata <- utils::read.csv(here::here("variable_metadata.csv"))
study_labels <- utils::read.csv(here::here("study_labels.csv"))
coef_mapping <- utils::read.csv(here::here("coef_mapping.csv"))



# Sourcing functions ----
source_functions <- function() {
  function_dir <- "R scripts/Functions"
  for (fun in list.files(here::here(function_dir), pattern="\\.R$")) {
    print(paste("sourcing", fun))
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

options("RStata.StataVersion" = 18)
options("RStata.StataPath" = "/Applications/Stata/StataMP.app/Contents/MacOs/stata-mp")

