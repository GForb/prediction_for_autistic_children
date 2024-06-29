data_file <- "elena_vabs.Rds"

data <- readRDS(file.path(derived_data, data_file))

data |> 
  dplyr::filter(wave == base_wave) |> 
  select(starts_with("vabs")) |> 
  describe_all(metadata = var_metadata)

predictor_names <- var_metadata |> filter(cbcl_predictor == 1) |> pull(variable_name)
data |> 
  dplyr::filter(wave == base_wave) |> 
  select(any_of(predictor_names)) |>
  describe_all(metadata = var_metadata)

data |> 
  dplyr::filter(wave == base_wave) |> 
  select(starts_with("vabs")) |> 
  describe_df(metadata = var_metadata)