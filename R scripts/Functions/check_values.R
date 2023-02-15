library(tidyverse)
library(here)

correct_values <- read.csv("variable_metadata.csv")

check_values <- function(data, metadata) {
  test_passed <- TRUE
  failed_test <- "The following variables had values out of range: "
  failed_the_test <- list()
  for (i in 1:nrow(metadata)) {
    string <- as.character(metadata[i, 1])
    print(string)
    tibble <- data |> 
      select(starts_with(string))
    for (j in 1:ncol(tibble)) {
      if(max(tibble[,j]) > metadata[i,3]){
        test_passed <- FALSE
        append(failed_the_test, colnames(tibble[j]))
      }else{
        if(min(tibble[,j]) < metadata[i, 2]){
          test_passed <- FALSE
          append(failed_the_test, colnames(tibble[j]))
        }
      }
    }
  }
  if(test_passed){
    print("Successfully passed test")
  }else{
    print(paste0(failed_test, failed_the_test))
  }
}
