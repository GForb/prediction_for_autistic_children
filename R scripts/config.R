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

results_folder_sdq <- here::here(data_and_outputs, "Results", "SDQ", "Thesis")
results_folder_cbcl <- here::here(data_and_outputs, "Results", "CBCL", "Thesis")
results_folder_vabs <- here::here(data_and_outputs, "Results", "VABS", "Thesis")


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
study_metadata <- utils::read.csv(here::here("study_labels.csv"))
coef_mapping <- utils::read.csv(here::here("coef_mapping.csv"))

sdq_cutoffs <- var_metadata |> 
  filter(variable_name %in% c("sdq_emot_p", "sdq_cond_p", "sdq_hyp_p", "sdq_peer_p", "sdq_pro_p")) |> 
  select(outcome = variable_name,
         outcome_label = label3,
         cutoff,
         min, 
         max)

cbcl_cutoffs <- var_metadata |> 
  filter(variable_name %in% c(
    "cbcl_aff",
    "cbcl_anx",
    "cbcl_som",
    "cbcl_adhd",
    "cbcl_odd",
    "cbcl_con"
  )) |> 
  select(outcome = variable_name,
         outcome_label = label3,
         cutoff,
         min, 
         max,
         n_items)


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

