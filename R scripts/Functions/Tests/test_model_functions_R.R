predictors <-  "base_age out_age base_sdq_pro_p base_sdq_hyp_p base_sdq_emot_p base_sdq_cond_p base_sdq_peer_p base_sex"

data <- analysis_data_wide <- readRDS(here(derived_data, "pooled_sdq_wide.Rds")) |> filter(base_all_complete, out_all_complete)
outcome <- "sdq_pro_p"

predictors_vector <- get_predictors_fixed_study(data, predictors)
hold_out_study <- "MCS"
train_data <- data |> filter(study != hold_out_study)
test_data <- data |> filter(study == hold_out_study)

model_formula = as.formula(paste0("out_", outcome, " ~ ", paste(predictors_vector, collapse = "+")))
print(model_formula)

model_function <- function(data){
  lm(formula = model_formula, data = data)
}
model <- model_function(train_data)

# only 
summary(model) |> print()
fixed_pred <- IPDPredictR:::predict_fixed(model, test_data)

# int_est = "average"
int_estimates <- model |> broom::tidy() |> filter(ter) # here!!

# int_est = "estimate"
inttercepts <- IPDPredictR:::predict_intercepts(model, newdata = test_data |> as.data.frame(), cluster_var = "study")


# int_est = estimate_cv


abc_data <- data.frame(a = 1:100, b = 1:100, c = 1:100)
fold_data <- abc_data |> add_fold(n_folds = 5)

fold_data |> count(fold)

