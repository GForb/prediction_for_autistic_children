# Prediction for autistic chilren [TO BE UPDATED]



The Prediction for autistic children project aims to develop statsitical models that describe the uncertainty of outcomes based on an autistic child's individual characteristics.

Find more information on our [study page](https://www.kcl.ac.uk/research/predictions-of-outcomes-for-autistic-children)

We are bringing together data from multiple longitudinal studies of autistic children and will develop prediction models using an individual participant data meta-analysis.

# Organisation of this repository

## 1. Documentation
This folder contains notes on the dataset, instructions on how to set up file paths to use the repo, and a style guide (pointing at the tidyverse style guide) for contributing code. 

**File paths environment variables must be set up prior to running any scripts**

see setting_up_paths.md

## 2. Rscripts
### Top level files:
  - config.R: Sets up paths and loads libraries and functions. This needs to be run first prior to other scripts.
  - 0_controller.R: Will run the whole analysis from start to finish
  - install_IPDPredictR.R A script to install the package IPDPredictR from github. This is a package authored by me that contains functions used in the analysis.

### Data Processing: 
Scripts to inport data from each study plus any helper scripts used.

### EDA: 
Exploratory data analysis, any scripts that run exploratory analysis on the data.

### Functions: 
This contains scripts of r functions that are used in the analysis
  - Within the funcitons folder there is a Tests folder, this contains tests for the fucntions.

### Modelling: 
Scripts to implement models
- run_model_only_* scripts run the analysis models on the full dataset for reporting model parameter results
- run_model_* scripts run the analysis models and carry out internal-external cross-validation 
- *_mi_* scripts run multiple impuation 
- single_study_analysis scripts are sensitivity analysis run on a single study

#### Note on implementation of modelling

### Thesis plots and reporting
Scripts to produce the figures and tables used in PhD thesis. Figures are outputted as .png and tables as .tex files.

## 3. Rmarkdown
Contains required rmarkdown files. These are knitted by the run_desciprtive_reports function

## 4. Stata
This contians stata files used in the analysis. The file prediction_progs.do defines a series of stata progrmas that are used to implement each model and conduct validation of model performance. 

## Other files
study_meta_data.csv contains data at a study level that can be used for reporting or checking
variable_metadata.csv contains data at a variable level that can be used for reporting or checking.
coef_mapping.csv contains mapping of model parameter names to predictor names and is used in reporting models
study_labels.csv contains labels for studies used in reporing