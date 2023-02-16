library(tidyverse)
library(here)

correct_values <- read.csv("variable_metadata.csv")
correct_values[3, 1] <- "sdq_tot"
correct_values[6, 1] <- "sdq_pro"

check_values <- function(data, metadata) {
  test_passed <- TRUE
  failed_test <- "The following variables had values out of range: "
  failed_the_test <- list()
  for (i in 1:nrow(metadata)) {
    string <- as.character(metadata[i, 1])
    tibble <- data |> 
      select(starts_with(string))
    for (j in 1:ncol(tibble)) {
      if(max(tibble[,j], na.rm = TRUE) > metadata[i,3]){
        test_passed <- FALSE
        failed_the_test[i] <- colnames(tibble[,j])
      }else{
        if(min(tibble[,j], na.rm = TRUE) < metadata[i, 2]){
          test_passed <- FALSE
          failed_the_test[i] <- colnames(tibble[,j])
        }
      }
    }
  }
  if(test_passed){
    return(print("Successfully passed test"))
  }else{
    return(print(paste0(failed_test, failed_the_test)))
  }
}
check_values(gui_data, correct_values)
