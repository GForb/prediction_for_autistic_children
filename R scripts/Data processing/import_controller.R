import_script_folder = here("R scripts", "Data processing")

# General population cohorts
source(here(import_script_folder, "import_lsac_k.R"))
rm(list=ls(pattern="^lsac"))
gc() 

source(here(import_script_folder, "import_lsac_b.R"))
rm(list=ls(pattern="^lsac"))
gc() 

source(here(import_script_folder, "import_gui.R"))
rm(list=ls(pattern="^gui"))
gc() 

source(here(import_script_folder, "import_mcs.R"))
rm(list=ls(pattern="^mcs"))
gc() 

source(here(import_script_folder, "import_ssc.R"))
source(here(import_script_folder, "import_HR.R"))