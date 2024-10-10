nrow_analysis_data <- function(analysis_data) {
  if(is.data.frame(analysis_data)) {
    return(nrow(analysis_data))
  } else {
    return(nrow(analysis_data[[1]]))
  }
}

nids_analysis_data <- function(analysis_data) {
  if(is.data.frame(analysis_data)) {
    data <- analysis_data
  } else {
    data <- analysis_data[[1]]
  }
  return(data$ID |> unique() |> length())
}