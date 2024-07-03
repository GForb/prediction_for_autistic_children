# Load required packages
library(ggplot2)
library(stringr)
results_folder <- here::here(data_and_outputs, "Results", "VABS", "Prelim")
full_data <- readRDS(here::here(results_folder, "results_meta_analysis_long.rds")) 

processed_data <- full_data |>
  filter(intercept_est_method == "average",
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
plot_data <- processed_data |> filter(outcome == "vabs_dls_ae") 

vline_data <- tibble(metric = full_data$metric |> unique(),
                     vline_x = c(1, 
                                 0, 
                                 plot_data |> filter(metric == "r_squared") |> pull(est) |> max(na.rm  = TRUE),
                                 plot_data |> filter(metric == "rmse") |> pull(est) |> min(na.rm  = TRUE)))

plot_data |> filter(outcome == "vabs_dls_ae") |>  plot_many_ma(diamond_height = 0.1) + facet_grid(cols = vars(metric), scales = "free_x") + 
  geom_vline(data = vline_data, aes(xintercept = vline_x), linetype = "dashed", color = "red")

plot_data <- processed_data |> filter(outcome == "vabs_com_ae") 

vline_data <- tibble(metric = full_data$metric |> unique(),
                     vline_x = c(1, 
                                 0, 
                                 plot_data |> filter(metric == "r_squared") |> pull(est) |> max(na.rm  = TRUE),
                                 plot_data |> filter(metric == "rmse") |> pull(est) |> min(na.rm  = TRUE)))

plot_data |> filter(outcome == "vabs_com_ae") |>  plot_many_ma(diamond_height = 0.1) + facet_grid(cols = vars(metric), scales = "free_x") + 
  geom_vline(data = vline_data, aes(xintercept = vline_x), linetype = "dashed", color = "red")



plot_data <- processed_data |> filter(outcome == "vabs_soc_ae") 

vline_data <- tibble(metric = full_data$metric |> unique(),
                     vline_x = c(1, 
                                 0, 
                                 plot_data |> filter(metric == "r_squared") |> pull(est) |> max(na.rm  = TRUE),
                                 plot_data |> filter(metric == "rmse") |> pull(est) |> min(na.rm  = TRUE)))

plot_data |> filter(outcome == "vabs_soc_ae") |>  plot_many_ma(diamond_height = 0.1) + facet_grid(cols = vars(metric), scales = "free_x") + 
  geom_vline(data = vline_data, aes(xintercept = vline_x), linetype = "dashed", color = "red")


# Many models

# By outcome
processed_data <- full_data |>
  filter(intercept_est_method == "estimate",
         metric == "r_squared",
         outcome = "vabs_dls_ae") |> 
  select(outcome, est, ci.lb, ci.ub, pi.lb, pi.ub) |>
  mutate(label = get_label(outcome , label_no = 1),
         position = factor(label) |> as.numeric() )