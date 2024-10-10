
process_results_for_outcome <- function(outcome, p_value_stars = TRUE) {
 uppercase_outcome <- toupper(outcome)
 results_folder <- here::here(data_and_outputs, "Results", uppercase_outcome, "Thesis")
 analysis_spec <- readRDS(here::here(results_folder, "analysis_spec.rds"))
 
 model_results_name <- paste0(outcome, "_model_only.rds")
 model_results <- readRDS <- readRDS(here::here(results_folder, model_results_name))
 
 results <- map2(analysis_spec$analysis_name, 
                 model_results,
                 \(x,y) process_model_results(x, y, results_folder, p_value_stars = TRUE)) |> 
   bind_rows() |> 
   tibble() |>
   left_join(analysis_spec |> select(analysis_name, outcome, predictor_set, model_name, suffix)) |> 
   select(-analysis_name) |> 
   pivot_wider(names_from = outcome, values_from = summary) |> 
   filter(coef != "n")
 return(results)
}

# SDQ
sdq_results <- process_results_for_outcome("sdq")
main_analysis_sdq <- sdq_results |> 
  filter(predictor_set  == "pred3_mt") |>  
  left_join(coef_mapping) |> # Coef mapping is loaded in config and is an externally managed csv with labels for model coeficients.
  arrange(order) |> 
  select(Coefficient = label,sdq_hyp_p, sdq_emot_p ,sdq_cond_p, sdq_peer_p, sdq_pro_p) 

 
  sdq_table <- bind_rows(colnames(main_analysis_sdq) |> get_label(label_no = 3), main_analysis_sdq) |> 
              huxtable::hux(add_colnames = FALSE) |> 
   huxtable::set_bottom_border(row = c(1,10, 15, 20, 25), value = 0.5)  
  sdq_table |>  save_hux_table(caption = "Model coefficients for the primary analysis models of the SDQ. Columns show coefficient estimates and 95\\% confidence intervals. *p<0.05, **p<0.01, ***p<0.001.",
                               label = "tab:sdq_primary_model",
                               file_name = "sdq_primary_model.tex",
                               font_size = 10)
  
# VABS
sdq_results <- process_results_for_outcome("vabs")
main_analysis_sdq <- sdq_results |> 
  filter(predictor_set  == "pred3_mt") |>  
  left_join(coef_mapping) |> # Coef mapping is loaded in config and is an externally managed csv with labels for model coeficients.
  arrange(order) |> 
  select(Coefficient = label,sdq_hyp_p, sdq_emot_p ,sdq_cond_p, sdq_peer_p, sdq_pro_p) 


sdq_table <- bind_rows(colnames(main_analysis_sdq) |> get_label(label_no = 3), main_analysis_sdq) |> 
  huxtable::hux(add_colnames = FALSE) |> 
  huxtable::set_bottom_border(row = c(1,10, 15, 20, 25), value = 0.5)  
sdq_table |>  save_hux_table(caption = "Model coefficients for the primary analysis models of the SDQ. Columns show coefficient estimates and 95\\% confidence intervals. *p<0.05, **p<0.01, ***p<0.001.",
                             label = "tab:sdq_primary_model",
                             file_name = "sdq_primary_model.tex",
                             font_size = 10)


# CBCL
sdq_results <- process_results_for_outcome("cbcl")
main_analysis_sdq <- sdq_results |> 
  filter(predictor_set  == "pred3_mt") |>  
  left_join(coef_mapping) |> # Coef mapping is loaded in config and is an externally managed csv with labels for model coeficients.
  arrange(order) |> 
  select(Coefficient = label,sdq_hyp_p, sdq_emot_p ,sdq_cond_p, sdq_peer_p, sdq_pro_p) 


sdq_table <- bind_rows(colnames(main_analysis_sdq) |> get_label(label_no = 3), main_analysis_sdq) |> 
  huxtable::hux(add_colnames = FALSE) |> 
  huxtable::set_bottom_border(row = c(1,10, 15, 20, 25), value = 0.5)  
sdq_table |>  save_hux_table(caption = "Model coefficients for the primary analysis models of the SDQ. Columns show coefficient estimates and 95\\% confidence intervals. *p<0.05, **p<0.01, ***p<0.001.",
                             label = "tab:sdq_primary_model",
                             file_name = "sdq_primary_model.tex",
                             font_size = 10)