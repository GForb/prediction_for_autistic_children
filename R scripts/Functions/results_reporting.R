# get_meta_analysis_df <- function(model_name_spec) {
#   if(is.null(model_name_spec$multiple_imputed_data)) {
#     list_results <- lapply(model_name_spec$file_name, run_meta_analysis)
#   } else {
#     list_results <- map2(model_name_spec$file_name, model_name_spec$multiple_imputed_data, run_meta_analysis)
#   }
#   
#   results_df <- bind_rows(list_results)
# 
#   model_info <- model_name_spec |> select(model = model_name, intercept_est_method  = intercept_est, outcome, predictor_set, file_name, analysis_name)
#   
#   # Extract df
#   long_results <- results_df |> 
#     left_join(model_info, by = "file_name") |>
#     select(-file_name) |>
#     mutate(tau2 = sqrt(tau2)) # not changing name as it is used compress columns, name correcte in select statement below
#   
#   wide_results <- long_results |> 
#     IPDPredictR:::compress_columns() |> 
#     mutate(summary = paste0(round(est, 2), " ", ci, " [", round(tau2, 2), "]")) |> 
#     select(outcome, model,predictor_set, intercept_est_method, metric, summary, meta_analysis, tau = tau2, analysis_name) |> 
#     pivot_wider(names_from = metric, values_from = all_of(c("summary", "meta_analysis", "tau")))  |> 
#     arrange(outcome) |> 
#     select(outcome  ,   model ,  predictor_set,    intercept_est_method,  everything() ) |> 
#     rename_with(.fn = ~ str_remove(.x, "summary_"), .cols = starts_with("summary")) 
#   
#   return(list(wide_results = wide_results, long_results = long_results))
# }
# 
# #     pivot_wider(names_from = metric, values_from = all_of(c("summary", "meta_analysis"))) |>
# 
# 
# run_meta_analysis <- function(results_name, multiple_imputed_data = NULL) {
#   print(results_name)
#   meta_analysis_df <- NULL
#   try({results <- readRDS(here::here(results_folder, results_name))
#     if(is.null(multiple_imputed_data) | is.na(multiple_imputed_data)) {
#       multiple_imputed_data <- FALSE
#     }
#     if(multiple_imputed_data){
#       meta_analysis_df <- run_meta_analysis_mi(results)
#     } else {
#       meta_analysis_df <- run_meta_analysis_single_data(results)
#     }
#     
#     meta_analysis_df <- meta_analysis_df |> mutate(file_name = results_name) 
#   })
#   return(meta_analysis_df)
# }
# 
# run_meta_analysis_mi <- function(results) {
#   n_imp = max(results$imp_no)
#   
#   # 1. Meta-analyse each imputed dataset separately
#   analse_imp_rep <- function(my_imp_no) {
#     results |> filter(imp_no == my_imp_no) |> run_meta_analysis_single_data() |> mutate(imp_no = my_imp_no)
#   }
#   results_list <- map(1:n_imp, analse_imp_rep) 
#   bind_rows(results_list) |> pool_est_all_metrics()
#   
# }
# 
# run_meta_analysis_single_data <- function(results) {
#   if(!check_results(results)){
#     meta_analysis_df <- tibble(metric = c("calib_slope", "calib_itl", "r_squared" , "rmse"))
#   } else {
#     meta_analysis_list <- IPDPredictR:::meta_analyse_predictions_cont(predictions = results, study_var_name = "study") 
#     meta_analysis_df <- meta_analysis_list$results_df |> tibble()
#     meta_analysis <- tibble(meta_analysis = meta_analysis_list$results_list)
#     
#     by_study <- tibble(by_study = list(meta_analysis_list$by_study))
#     
#     meta_analysis_df <- meta_analysis_df |> bind_cols(meta_analysis) |> bind_cols(by_study)
#   }
# 
#     return(meta_analysis_df)
# }
# 
# 
# 
# check_results <- function(results) {
#   if(is.null(results$pred) | is.null(results$actual)) {
#     return(FALSE)
#   } else if(any(is.na(results$pred)) | any(is.na(results$actual))) {
#       return(FALSE)
#   } else {
#     return(TRUE)
#   }
# 
# }
# 
# process_results_df  <- function(model_names, intercept_est_methods, outcomes, predictor_set, df) {
#   if(is.null(df)) {
#     tibble(metric = c("r-squared", "calib_itl", "calib_slope"),
#            outcome = outcomes, 
#            model = model_names, 
#            predictor_set = predictor_set,
#            intercept_est_method = intercept_est_methods,
#            summary = NA) |> 
#       pivot_wider(names_from = metric, values_from = summary) 
#     
#   } else {
#     df |> mutate(
#       outcome = outcomes, 
#       model = model_names, 
#       predictor_set = predictor_set,
#       intercept_est_method = intercept_est_methods) |> 
#       IPDPredictR:::compress_columns() |> 
#       mutate(summary = paste0(round(est, 2), " ", ci, " ", pi, " [", round(tau2, 2), "] ")) |> 
#       select(outcome, model,predictor_set, intercept_est_method, metric, summary) |> 
#       pivot_wider(names_from = metric, values_from = summary) 
#   }
# }
# 
