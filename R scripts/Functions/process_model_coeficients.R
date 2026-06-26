
process_results_for_outcome <- function(outcome, p_value_stars = TRUE, sqrt_var = FALSE) {
  uppercase_outcome <- toupper(outcome)
  results_folders <- list(cbcl = results_folder_cbcl, 
                          sdq = results_folder_sdq, 
                          vabs = results_folder_vabs)
  results_folder <- results_folders[[outcome]]
  analysis_spec <- readRDS(here::here(results_folder, "analysis_spec.rds"))
  
  model_results_name <- paste0(outcome, "_model_only.rds")
  model_results <- readRDS <- readRDS(here::here(results_folder, model_results_name))
  
  print(analysis_spec$analysis_name)
  length(model_results) |> print()
  print(model_results[[1]])
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