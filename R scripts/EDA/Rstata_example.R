library(RStata)
library(glue)
data_folder <-  here::here(derived_data, "SDQ_gen_pop")

data <- readRDS(file = here(data_folder, "sdq_gen_pop_non_autistic.rds")) |> 
  select(ID, studyid, wave, out_wave, age, starts_with("base"), starts_with("sdq"), relative_wave, train_data, test_data) |> na.omit() |> data.frame()

data_stp <- data |> filter(relative_wave == 1)

get_stata_predictions <- function(data, stata_model_string, gsem = FALSE) {
  data_pre_processing <-  "
    encode studyid, gen(studyid2)
    drop studyid
    rename studyid2 studyid
"
  predict <-  "
    keep if test_data
     predict pred_regress 
     keep studyid sdq_emot_p pred
     keep if relative_wave ==1
"
  
  reg_pred <- stata(paste(data_pre_processing, stata_model_string, predict, sep = "\n"),  data.in = data, data.out = TRUE)
  colnames(reg_pred) <- c("actual", "study" ,"pred")
  
  return(reg_pred)
}



outcome <- "sdq_emot_p"

model <- glue::glue("
  regress {outcome}  i.studyid age  base_sdq_emot_p   base_sdq_cond_p  base_sdq_hyp_p  base_sdq_peer_p base_sdq_pro_p base_age base_sex if relative_wave ==1 & train_data
")
results <- get_stata_predictions(stata_model_string = model, data_stp)

IPDPredictR:::meta_analyse_predictions(results, IPDPredictR:::evaluate_performance_cont_obs_pred)
  

results_uri1 <- stata_univariate_random_intercept(data = data, 
                                                 outcome = "sdq_emot_p", 
                                                 predictors =  "i.studyid   base_sdq_cond_p  base_sdq_hyp_p  base_sdq_peer_p base_sdq_pro_p base_age base_sex",
                                                 n_timepoints = 1)


results_uri2 <- stata_univariate_random_intercept(data = data, 
                                                 outcome = "sdq_emot_p", 
                                                 predictors =  "i.studyid   base_sdq_cond_p  base_sdq_hyp_p  base_sdq_peer_p base_sdq_pro_p base_age base_sex",
                                                 n_timepoints = 2)


results_uri3 <- stata_univariate_random_intercept(data = data, 
                                                 outcome = "sdq_emot_p", 
                                                 predictors =  "i.studyid   base_sdq_cond_p  base_sdq_hyp_p  base_sdq_peer_p base_sdq_pro_p base_age base_sex",
                                                 n_timepoints = 3)

univariate_random_int_1tp <- function(n_timepoints, data) {
  results <- stata_univariate_random_intercept(data = data, 
                                                    outcome = "sdq_emot_p", 
                                                    predictors =  "i.studyid   base_sdq_cond_p  base_sdq_hyp_p  base_sdq_peer_p base_sdq_pro_p base_age base_sex",
                                                    n_timepoints = n_timepoints) |> 
    IPDPredictR:::meta_analyse_predictions(IPDPredictR:::evaluate_performance_cont_obs_pred)
}

univariate_random_int <- function(timepoints, data) {
  results_list <- lapply(timepoints, univariate_random_int_1tp, data=data)
  results_list |> dplyr::bind_rows(.id = "timepoints") |> select(timepoints, everything())
  
}

stata_results_many_timpoints(outcome = "sdq_emot_p", timepoints = 1:3, data = data, stata_model_fun = stata_univariate_random_intercept)

stata_results_many_outcomes(outcomes = c("sdq_emot_p", "sdq_cond_p", "sdq_hyp_p"), timepoints = 1:3, data = data, stata_model_fun = stata_univariate_random_intercept)

results_uri1 |> IPDPredictR:::meta_analyse_predictions(IPDPredictR:::evaluate_performance_cont_obs_pred)
results_uri2 |> IPDPredictR:::meta_analyse_predictions(IPDPredictR:::evaluate_performance_cont_obs_pred)
results_uri3 |> IPDPredictR:::meta_analyse_predictions(IPDPredictR:::evaluate_performance_cont_obs_pred)


results_uri1[1:10,]
results_uri2[1:10,]
results_uri3[1:10,]
