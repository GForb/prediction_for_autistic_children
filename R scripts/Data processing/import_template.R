library(tydyverse)
library(here)

# see variables.txt for variable names and study names
# have one import script per study, name import_study_name
# load data

# If sample is not all autistic - create tibble indicating autism status
  # check to see if autism diagnosis is asked at different waves
  # It may be the case that a diagnosis may only need to be recorded once, once recorded 
  # this will be a dataframe with two columns: 
    # 1: participant ID
    # 2: Autistic TRUE/FLASE/NA


# for each wave:
  # merge all questionnaire data so there is one row per participant
  # select needed variables
  # generate a variable indicating which wave
  # generate an ID relating to each particpant
  # rename variables to match variable name list 

# stack (use bind_rows) data from different rows
  # this will give a tibble with one row per participant, per wave

# merge stacked dataset with autism diagnosis tibble, 

# keep only autistic particpants - drop others

# add a variable indicating which study the data is from