# Load required packages
library(ggplot2)
library(stringr)
results_folder <- here::here(data_and_outputs, "Results", "SDQ", "Prelim")
full_data <- readRDS(here::here(results_folder, "results_meta_analysis_long.rds")) 

processed_data <- full_data |>
  filter(intercept_est_method == "estimate_cv",
         predictor_set != "pred_init") |> 
  filter(!(model == "st_fi_study_mi" & predictor_set == "pred1")) |> 
  mutate(label = case_when(
      predictor_set == "pred1" ~ "Base predictors",
      predictor_set == "pred2" ~ "Base + Individual",
      predictor_set == "pred3" ~ "Base + Individual + Contextual",
    ),
    position =  label |>  factor() |>  as.numeric())
   
# Print the processed data
outcomes <- processed_data |> pull(outcome) |> unique()


processed_data_by_outcome <- processed_data |> 
  filter(model == "mt_ri_study_rs",
         predictor_set == "pred1_mt"
         !(metric %in% c("calib_itl", "r_squared"))) |> 
  mutate( 
    label = get_label(outcome), 
    position =  label |>  factor() |>  as.numeric()
  )

processed_data_by_outcome |> 
  plot_many_ma_by_metric(diamond_height = 0.1) +
  ggtitle("Predictor set 3")

# By outcome multi-timepoint
processed_data_by_outcome <- processed_data |> 
  filter(model == "st_ri_study",
         predictor_set == "pred1",
         !(metric %in% c("calib_itl", "r_squared"))) |> 
  mutate( 
    label = get_label(outcome), 
    position =  label |>  factor() |>  as.numeric()
  )

processed_data_by_outcome |> 
  plot_many_ma_by_metric() +
  ggtitle("Single timepoint, predictor set 1")


plots <- map(outcomes, ~plot_many_ma_by_metric(data = processed_data, outcome= .x))
