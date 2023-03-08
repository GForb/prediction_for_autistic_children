divide_by_wave <- function(data){
  wave_ids <- data$wave |> unique()
  data_wave <- list()
  for (i in wave_ids) {
    subseted <- data |> 
      filter(wave == i)
    data_wave[i] <- list(subseted)
  }
  return(data_wave)
}

find_min_max <- function(variable_name, var_metadata) {
  metadata_row <- var_metadata[startsWith(variable_name, var_metadata[,1]),]
  ylims <- c(metadata_row[1,2], metadata_row[1,3])
  if (any(is.na(ylims))) {
    ylims <- NULL
  }
  return(ylims)
}
