
results_folder <- here::here(data_and_outputs, "Results", "sdq", "Prelim")


outcomes <- c("sdq_cond_p", "sdq_emot_p", "sdq_hyp_p", "sdq_peer_p", "sdq_pro_p") # 

model_names <- tibble(outcomes = outcomes) |> 
  mutate(model_name =  paste0("st_ri_study_", outcomes, "_pred3_mi_estimate_cv")
)

outcome <- "sdq_hyp_p"
model_full_name <- model_names |> filter(outcomes == outcome) |> pull(model_name)
hype_plots <- report_model(model_full_name, results_folder, outcome = "SDQ")

hype_plots |> ggExtra::ggMarginal()

outcome <- "sdq_emot_p"
model_full_name <- model_names |> filter(outcomes == outcome) |> pull(model_name)
report_model(model_full_name, results_folder, outcome = "SDQ")

outcome <- "sdq_peer_p"
model_full_name <- model_names |> filter(outcomes == outcome) |> pull(model_name)
report_model(model_full_name, results_folder, outcome = "SDQ")