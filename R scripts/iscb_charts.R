data <- readRDS(here(derived_data, "pooled_sdq_wide.Rds")) |> filter(autism != "post baseline")


outcome_names <- var_metadata |> filter(outcome ==1) |> pull(variable_name)
predictor_metadata <- var_metadata |> filter(sdq_predictor == 1, outcome ==0)
predictor_names <- predictor_metadata |> pull(variable_name)

overall_n <- length(data$ID |> unique())

results_folder <- here::here(data_and_outputs, "Results", "sdq", "Prelim")

outcomes <- c("sdq_cond_p", "sdq_emot_p", "sdq_hyp_p", "sdq_peer_p", "sdq_pro_p") # 

model_names1 <- tibble(outcomes = outcomes) |> 
  mutate(model_name =  paste0("st_ri_study_", outcomes, "_pred1_estimate_cv")
  )

model_names2 <- tibble(outcomes = outcomes) |> 
  mutate(model_name =  paste0("st_ri_study_", outcomes, "_pred1_average")
  )

results_folder <- here::here(data_and_outputs, "Results", "sdq", "Prelim")
results <- readRDS(here(results_folder, "results_meta_analysis.rds"))

result <- results |> filter(
  outcome == "sdq_emot_p", 
  intercept_est_method == "average",
  model == "st_fi_study",
  predictor_set == "pred1"
  )
ma <- result|> 
  pull(meta_analysis_calib_itl)
ma <- ma[[1]]
my_tau <- result |> pull(tau_calib_itl)

tau_formatted <- my_tau |>  format(scientific=F, digits = 2)
tau_text = glue::glue("tau = {tau_formatted}")

save_folder <- "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/Conferences/ISCB 2024"

png(file= file.path(save_folder, "forrest_plot_calib_itl.png"),
    width = 14, 
    height = 14, 
    units = "cm",
    res = 300)
metafor::forest(ma,
                annotate = FALSE,
                addpred = TRUE, 
                refline = 0 , 
                xlab = "Calibration in the large", 
                header= "Study", 
                mlab = tau_text)
dev.off()
