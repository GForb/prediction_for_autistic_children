# add code which loads clean data
library(tidyverse)
library(here)

source(here("R scripts", "set_paths.R" ))
source(here("R scripts", "Functions","summary_functions.R" ))
source(here("R scripts", "Functions","helper_functions.R" ))

make_summary_by_wave(gui_data)
make_summary_by_wave(data)
