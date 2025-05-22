# Prediction for autistic chilren [TO BE UPDATED]



The Prediction for autistic children project aims to develop statsitical models that describe the uncertainty of outcomes based on an autistic child's individual characteristics.

Find more information on our [study page](https://www.kcl.ac.uk/research/predictions-of-outcomes-for-autistic-children)

We are bringing together data from multiple longitudinal studies of autistic children and will develop prediction models using an individual participant data meta-analysis.

# Organisation of this repository

## 1. Documentation
This folder contains notes on the dataset, instructions on how to set up file paths to use the repo, and a style guide (pointing at the tidyverse style guide) for contributing code. 

***File paths environment variables must be set up prior to running any scripts***

see setting_up_paths.md

## 2. Rscripts
### Top level r scripts:
  - config.R: Sets up paths relative to path environment variables and loads libraries and functions. This needs to be run first prior to other scripts.
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
- study_meta_data.csv contains data at a study level that can be used for reporting or checking
- variable_metadata.csv contains data at a variable level that can be used for reporting or checking.
- coef_mapping.csv contains mapping of model parameter names to predictor names and is used in reporting models
- study_labels.csv contains labels for studies used in reportingg

# The modelling pipeline
Models are estimted, and internal-external cross validation is carried out in Stata. Data processing, meta-analysis of results, and reporting is carried out in R. The whole modelling process is controlled via R.

## Modelling
Prior to modelling, multiple imputaiotn is carried out in the mi_ scripts and the multiply imputed datasets are saved.
For each *outcome* the script run_*outcome*_models.R creates a grid which charactersits of the analysis including a model function, which predictors, and which dataset to use. This script then passes the grid to the run_many_models function which loops trough the rows and calls the mdoel function for each row.

The model functions are defined in the script Rscripts/Functions/model_factories.R. They are named model_pred_reg... or model_pred_gsem... indicating whether mdoelling is carried out using linear regression or Stata's gsem command. The model functions set up the arguments required for the stata code then call run_stat_code. 
This fucntion creates a .do file for the analysis, and runs it in Stata via the Rstata package. The .do file for each analysis is saved with the results and not included in this repository. Each analysis calls the stata program Stata/prediction_progs.do wchih implents the analysis and carries out internal-external cross validation.

The Stata code saves the predicted values and outcomes for each cross validation fold. Performance metrics and meta-analysis of performance metrics are carried out by the funciton 'create_full_results_table' saved in R scripts/Functions/create_full_results_table.R. Ultimately the outcomes and predicted values are passed to functions from the package IPDPredictR which calcualtes model performance and implements the meta-analysis of results. 

A similar process is fullowed in the scripts run_model_only_*outcome*, but the only the models are estimated and no cross-validation is carried out.

