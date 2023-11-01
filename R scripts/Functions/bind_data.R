stack_datasets <- function(data_list) {
  # Combine datasets into one data frame
  data <- dplyr::bind_rows(data_list)
  
  # Check for duplicate IDs within and across studies
  data$UID <- NA
  uid_counter <- 1
  
  for (id in unique(data$ID)) {
    id_data <- data[data$ID == id, ]
    if (nrow(id_data) > 1) {
      # Check if ID appears in multiple studies
      study_ids <- unique(id_data$study)
      if (length(study_ids) > 1) {
        # Assign unique ID within each study
        for (study_id in study_ids) {
          study_data <- id_data[id_data$study == study_id, ]
          study_data$UID <- paste0(study_data$study, "_", uid_counter)
          data[data$ID == id & data$study == study_id, "UID"] <- study_data$UID
          uid_counter <- uid_counter + 1
        }
      } else {
        # Assign sequential UID within study
        id_data$UID <- paste0(id_data$study, "_", seq_along(id_data$ID))
        data[data$ID == id, "UID"] <- id_data$UID
      }
    } else {
      # Assign single UID to participant with only one observation
      data[data$ID == id, "UID"] <- paste0(id_data$study, "_1")
    }
  }
  
  return(data)
}

