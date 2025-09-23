# Load required packages
library(ggplot2)
library(stringr)
results_folder <- here::here(data_and_outputs, "Results", "VABS", "Prelim")
full_data <- readRDS(here::here(results_folder, "results_meta_analysis_long.rds")) 

processed_data <- full_data |>
  filter(intercept_est_method == "estimate_cv",
         predictor_set != "pred_init") |> 
  filter(!(model == "st_fi_study_mi" & predictor_set == "pred1")) |> 
  mutate(single_multi = case_when(
    grepl("^st", analysis_name)  ~ "Single-timepoint, ",
    grepl("^mt", analysis_name) & grepl("ri$", model)   ~ "Multi-timepoint, random intercept, ",
    grepl("^mt", analysis_name) & grepl("rs$", model) ~ "Multi-timepoint, random slope, "),
         predictor_set_label = case_when(predictor_set == "pred1" ~ "Base predictors",
                                         predictor_set == "pred2" ~ "Base + Individual",
                                         predictor_set == "pred3" ~ "Base + Individual + Contextual"),
    single_multi = case_when( 
      grepl("^mt", analysis_name) & grepl("3pred$", analysis_name) ~ paste0(single_multi, "3tp, "),
      TRUE ~ single_multi),
    label = paste(single_multi, predictor_set_label, sep = "")) |> 
         arrange(single_multi, predictor_set) |> 
    mutate(
             position =  
               label |> 
               factor(
                 levels = c(
                   "Multi-timepoint, random slope, 3tp, Base predictors",    
                   "Multi-timepoint, random slope, Base predictors",         
                   "Multi-timepoint, random intercept, 3tp, Base predictors",
                   "Multi-timepoint, random intercept, Base predictors",     
                   "Single-timepoint, Base + Individual + Contextual", 
                   "Single-timepoint, Base + Individual"  , 
                   "Single-timepoint, Base predictors"   
               )) |>   
               as.numeric())
           

# Print the processed data

processed_data |> 
  plot_many_ma_by_metric(outcome = "vabs_dls_ae", diamond_height = 0.1)

processed_data |> 
  plot_many_ma_by_metric(outcome = "vabs_soc_ae", diamond_height = 0.1)

processed_data |> 
  plot_many_ma_by_metric(outcome = "vabs_com_ae", diamond_height = 0.1)

processed_data_by_outcome <- processed_data |> 
  filter(model == "st_fi_study") |> 
  mutate( 
    label = get_label(outcome), 
    position =  label |>  factor() |>  as.numeric()
  )

processed_data_by_outcome |> 
  plot_many_ma_by_metric(diamond_height = 0.1)


myOutcome = processed_data_by_outcome |> pull(outcome) |> unique()

processed_data_by_outcome
   