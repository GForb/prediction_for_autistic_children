analysis_data <- readRDS(here(derived_data, "pooled_sdq.Rds")) |>
  filter(base_all_complete, out_all_complete, autism != "post baseline")

analysis_data_base <- analysis_data |> filter(wave == base_wave)

analysis_data_base |> group_by(study) |> summarise(n = n(),
                                                   min_age = min(age))