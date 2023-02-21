library(haven)
library(tidyverse)
lsac_b_wave_8 <- read_sas(file.path(raw_data, "LSAC/General Release/Survey data/SAS/lsacgrb17.sas7bdat"))
is_there_sdq <- grep("sdq", lsac_b_wave_8)
is_there_sdq
x <- sum(is_there_sdq)
if(x == 0){
  print("There is no SDQ in this wave")
}else{
  print("Found SDQ!")
}