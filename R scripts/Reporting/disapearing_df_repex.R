devtools::install_github("GForb/IPDPredictR")
library(IPDPredictR)
detach("package:IPDPredictR", unload = TRUE, character.only = TRUE)

results_folder <- here::here(data_and_outputs, "Results", "VABS", "Prelim")
model_names <- c("results_reg_init", "results_reg", "results_ri_int", "results_ri")
intercept_est_methods <- c("estimate", "average")
outcome <- "vabs_dls_ae"


model_name_spec = expand_grid(model_names, intercept_est_methods, outcome) |>
  mutate(analysis_name = paste0(model_names, "_", outcome, "_int_", intercept_est_methods),
         file_name = paste0(analysis_name,".rds"))



analysis_name <- model_name_spec |> slice(1) |> pull(file_name) 
result <- readRDS(here::here(results_folder,analysis_name))

result |> IPDPredictR:::meta_analyse_predictions_cont(study_var_name = "study")


