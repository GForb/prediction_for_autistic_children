# Prediction for autistic chilren [TO BE UPDATED]



The Prediction for autistic children project aims to develop statsitical models that describe the uncertainty of outcomes based on an autistic child's individual characteristics.

Find more information on our [study page](https://www.kcl.ac.uk/research/predictions-of-outcomes-for-autistic-children)

We are bringing together data from multiple longitudinal studies of autistic children and will develop prediction models using an individual participant data meta-analysis.


The code is currently under developmet.

# Organisation of this repository

## 1. Documentation
This folder contains notes on the dataset, instructions on how to set up file paths to use the repo, and a style guide (pointing at the tidyverse style guide) for contributing code.

## 2. Rscripts
### Top level files:
  - config.R: Sets up paths and loads libraries and functions. Some of this could be taken out if fucntions were in a package
  - 0_controller.R: Will run the whole analysis from start to finish
  - run_descriptive_reports.R: Generates rmarkdown reports for each study containing descriptive graphs and summary tables. 

### Data Processing: 
There should be one import script per study. This should do all the merging and data manipulation required to get the data into the required form. At the end of each script check functions are called before saving to catch any out of range values or duplicate observations.
### EDA: 
Exploratory data analysis, any scripts that run exploratory analysis on the data.

### Functions: 
This contains scripts of r functions that are used in the analysis
  - Within the funcitons folder there is a Tests folder, this contains tests for the fucntions.

### Modelling: 
Scripts to implement models

### Thesis plots and reporting
Scripts to produce the figures and tables used in PhD thesis. Figures are outputted as .png and tables as .tex files.

## 3. Rmarkdown
Contains required rmarkdown files. These are knitted by the run_desciprtive_reports function

## Other files
study_meta_data.csv contains data at a study level that can be used for reporting or checking
variable_metadata.csv contains data at a variable level that can be used for reporting or checking.
