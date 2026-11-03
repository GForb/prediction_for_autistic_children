# This config file is for use with controller v4.
# It uses the same derived data as config v2 (publication), but writes model results to a "Revised" subfolder
# so that the publication results in "Thesis" are not overwritten.


# Setting paths ----
# To set paths the global option "pfac_path" must be set with the path to the data and outputs folder.
# If using Rstudio, this can be done by adding  Sys.setenv(pfac_path = "*path to data*") to the .Renviron file. To edit the .Renviron file you can use  usethis::edit_r_environ().

print("running config v4 for revised results")

# Data folders
data_and_outputs <-  Sys.getenv("pfac_path")
raw_data <- here::here(data_and_outputs, "Raw data")
derived_data <- here::here(data_and_outputs, "Publication", "Derived Data")
# Prediction interval data is created from revised results, so is saved separately from the publication derived data
pi_data_folder <- here::here(derived_data, "Revised")
dir.create(pi_data_folder, showWarnings = FALSE)

# Output folders
outputs <- here::here(data_and_outputs, "Publication", "Outputs", "Revised")
thesis_tables <- file.path(outputs, "Thesis Tables")
thesis_plots <- file.path(outputs, "Thesis Plots")

latex_folder <-  file.path(thesis_tables, "LatexTables")

# Creating output folders, mirroring the publication outputs folder structure
for (folder in c("Paper", "Results", "Dataset summaries", "Plots",
                 file.path("Thesis Plots", c("SDQ Post diag", "Main Results")),
                 file.path("Thesis Tables", c("LatexTables", "Main Results")))) {
  dir.create(file.path(outputs, folder), recursive = TRUE, showWarnings = FALSE)
}

# Results folders
results_folder_main <- here::here(data_and_outputs, "Publication", "Results")

results_folder_sdq <- here::here(results_folder_main, "SDQ", "Revised")
results_folder_cbcl <- here::here(results_folder_main, "CBCL", "Revised")
results_folder_vabs <- here::here(results_folder_main, "VABS", "Revised")

# Creating results folders (Stata logs and do files fail to write if folders do not exist)
for (folder in c(results_folder_sdq, results_folder_cbcl, results_folder_vabs)) {
  for (sub_folder in c("Logs", "Do Files")) {
    dir.create(file.path(folder, sub_folder), recursive = TRUE, showWarnings = FALSE)
  }
}




# R scripts folders
data_processing_scripts <- here::here("R scripts", "Data processing")
modelling_scripts <- here::here("R scripts", "Modelling")
reporting_scripts <- here::here("R scripts", "Reporting")
thesis_reporting <- here::here("R scripts", "Thesis plots and tables")
reporting_scripts <- here::here("R scripts", "Reporting")
paper_scripts <- here::here("R scripts", "Paper")



devtools::install_github("GForb/IPDPredictR")

# Loading required libraries
library(metafor)
library(huxtable)
library(smcfcs)
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
#library(micemd)

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
options("RStata.StataPath" = "/Applications/StataNow/StataMP.app/Contents/MacOs/stata-mp")


print("config v4 loaded")


