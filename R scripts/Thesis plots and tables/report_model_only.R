coef_mapping <- utils::read.csv(here::here("coef_mapping.csv"))

process_results_for_outcome <- function(outcome, p_value_stars = TRUE, sqrt_var = FALSE) {
 uppercase_outcome <- toupper(outcome)
 results_folder <- here::here(data_and_outputs, "Results", uppercase_outcome, "Thesis")
 analysis_spec <- readRDS(here::here(results_folder, "analysis_spec.rds"))
 
 model_results_name <- paste0(outcome, "_model_only.rds")
 model_results <- readRDS <- readRDS(here::here(results_folder, model_results_name))
 
 results <- map2(analysis_spec$analysis_name, 
                 model_results,
                 \(x,y) process_model_results(x, y, results_folder, p_value_stars = TRUE, sqrt_var = sqrt_var)) |> 
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
  select(Coefficient = label, starts_with("sdq")) 

 
  sdq_table <- bind_rows(colnames(main_analysis_sdq) |> get_label(label_no = 3), main_analysis_sdq) |> 
              huxtable::hux(add_colnames = FALSE) |> 
   huxtable::set_bottom_border(row = c(1,10, 15, 20, 25), value = 0.5)  
  sdq_table |>  save_hux_table(caption = "Model coefficients for the primary analysis models of the SDQ. Columns show coefficient estimates and 95\\% confidence intervals. *p<0.05, **p<0.01, ***p<0.001.",
                               label = "sdq_primary_model",
                               file_name = "sdq_primary_model.tex",
                               font_size = 10)
  
  st_sdq <- sdq_results |> 
    filter(model_name  == "st_fi_study",  predictor_set == "pred1") |>  
    left_join(coef_mapping) |> # Coef mapping is loaded in config and is an externally managed csv with labels for model coeficients.
    arrange(order) |> 
    select(Coefficient = label, starts_with("sdq")) 
  
  
  sdq_table_st <- bind_rows(colnames(st_sdq) |> get_label(label_no = 3), st_sdq) |> 
    huxtable::hux(add_colnames = FALSE) |> 
    huxtable::set_bottom_border(row = c(1,10, 17), value = 0.5)  
  sdq_table_st |>  save_hux_table(caption = "Model coefficients for the single timepoint model for the SDQ, estimated using study, age, sex and baseline domains of outcome only. Columns show coefficient estimates and 95\\% confidence intervals. *p<0.05, **p<0.01, ***p<0.001.",
                               label = "sdq_st_model",
                               file_name = "sdq_st_model.tex",
                               font_size = 10)
  
  
# VABS
vabs_results <- process_results_for_outcome("vabs", sqrt_var = TRUE)
main_analysis_vabs <- vabs_results |> 
  filter(predictor_set  == "pred3_mt") |>  
  left_join(coef_mapping) |> # Coef mapping is loaded in config and is an externally managed csv with labels for model coeficients.
  arrange(order) |> 
  select(Coefficient = label, starts_with("vabs")) 


vabs_table <- bind_rows(colnames(main_analysis_vabs) |> get_label(label_no = 3), main_analysis_vabs) |> 
  huxtable::hux(add_colnames = FALSE) |> 
  huxtable::set_bottom_border(row = c(1,5, 13, 16, 24), value = 0.5)  
vabs_table |>  save_hux_table(caption = "Model coefficients for the primary analysis models of the VABS Columns show coefficient estimates and 95\\% confidence intervals. *p<0.05, **p<0.01, ***p<0.001.",
                             label = "vabs_primary_model",
                             file_name = "vabs_primary_model.tex",
                             font_size = 10)

st_vabs <- vabs_results |> 
  filter(model_name  == "st_fi_study", predictor_set == "pred1") |>  
  left_join(coef_mapping) |> # Coef mapping is loaded in config and is an externally managed csv with labels for model coeficients.
  arrange(order) |> 
  select(Coefficient = label, starts_with("vabs")) 


vabs_table_st <- bind_rows(colnames(st_vabs) |> get_label(label_no = 3), st_vabs) |> 
  huxtable::hux(add_colnames = FALSE) |> 
  huxtable::set_bottom_border(row = c(1,5, 11, 15), value = 0.5)  
vabs_table_st |>  save_hux_table(caption = "Model coefficients for the single timepoint model for the VABS, estimated using study, age, sex and baseline domains of outcome only. Columns show coefficient estimates and 95\\% confidence intervals. *p<0.05, **p<0.01, ***p<0.001.",
                                label = "vabs_st_model",
                                file_name = "vabs_st_model.tex",
                                font_size = 10)



# CBCL
cbcl_results_raw <- process_results_for_outcome("cbcl", sqrt_var = TRUE)

cbcl_results <- cbcl_results_raw|> 
  pivot_longer(cols = starts_with("cbcl"), names_to = "outcome", values_to = "summary") |>
  filter(predictor_set  == "pred3_mt", (model_name == "mt_fi_study_rs" & outcome != "cbcl_som") |
           (model_name == "mt_fi_study_ri" & outcome == "cbcl_som")) |> 
  select(coef, outcome, summary) |> 
  left_join(coef_mapping) |>   
  select(Coefficient = label,  outcome, summary, order) |> # Coef mapping is loaded in config and is an externally managed csv with labels for model coeficients.
  pivot_wider(names_from = outcome, values_from = summary) |> 
  arrange(order, Coefficient) |> 
  select(-order) |> 
  select(Coefficient, cbcl_adhd, cbcl_aff, cbcl_anx, cbcl_con, cbcl_odd, cbcl_som)
  


cbcl_table <- bind_rows(colnames(cbcl_results) |> get_label(label_no = 3), cbcl_results) |> 
  huxtable::hux(add_colnames = FALSE) |> 
  huxtable::set_bottom_border(row = c(1,7, 12, 18, 27), value = 0.5)  
cbcl_table |>  save_hux_table(caption = "Model coefficients for the primary analysis models of the CBCL. Columns show coefficient estimates and 95\\% confidence intervals. *p<0.05, **p<0.01, ***p<0.001.",
                             label = "cbcl_primary_model",
                             file_name = "cbcl_primary_model.tex",
                             font_size = 10)



st_cbcl <- cbcl_results_raw |> 
  filter(model_name  == "st_fi_study", , predictor_set == "pred1") |>  
  left_join(coef_mapping) |> # Coef mapping is loaded in config and is an externally managed csv with labels for model coeficients.
  arrange(order) |> 
  select(Coefficient = label, cbcl_adhd, cbcl_aff, cbcl_anx, cbcl_con, cbcl_odd, cbcl_som)


cbcl_table_st <- bind_rows(colnames(st_cbcl) |> get_label(label_no = 3), st_cbcl) |> 
  huxtable::hux(add_colnames = FALSE) |> 
  huxtable::set_bottom_border(row = c(1,7, 14), value = 0.5)  
cbcl_table_st |>  save_hux_table(caption = "Model coefficients for the single timepoint model for the CBCL, estimated using study, age, sex and baseline domains of outcome only. Columns show coefficient estimates and 95\\% confidence intervals. *p<0.05, **p<0.01, ***p<0.001.",
                                label = "cbcl_st_model",
                                file_name = "cbcl_st_model.tex",
                                font_size = 10)