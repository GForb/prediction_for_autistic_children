# To set paths the global option "pfac_path" must be set with the path to the data and outputs folder. 
# This can be done with the command options(pfac_path = "*path to data*"). This only needs to be run once.

data_and_outputs <-  Sys.getenv("pfac_path")
raw_data <- file.path(data_and_outputs, "Raw data")
derived_data <- file.path(data_and_outputs, "Derived data")
outputs <- file.path(data_and_outputs, "Outputs")

var_metadata <- read.csv(here::here("variable_metadata.csv"))

source(here::here("R scripts/Functions/check_values.R"))