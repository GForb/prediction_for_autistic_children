data <- readRDS(here(derived_data , "pooled_sdq_wide.Rds"))

summarise_hv <- function(data) {
  data |> 
    summarise(N = n(),
              n_hearing_data = sum(!is.na(base_hearing_impairment)),
              n_hearing_impariment = sum(base_hearing_impairment == 1, na.rm = TRUE),
              n_vision_data = sum(!is.na(base_visual_impairment)),
              n_visual_impariment = sum(base_visual_impairment == 1, na.rm = TRUE),
    ) |> 
    mutate(prop_hearing = n_hearing_impariment/n_hearing_data,
           prop_vision = n_visual_impariment/n_vision_data)
  
}

by_study <- data |> 
  group_by(study) |> 
  summarise_hv() |> 
  ungroup()

overall <- data |> 
  summarise_hv() |> 
  mutate(study = "Overall")

bind_rows(by_study, overall)