vabs_wide_data <- readRDS(here(derived_data, "pooled_vabs_wide.Rds"))
vabs_long_data <- readRDS(here(derived_data, "pooled_vabs.Rds"))

colnames(vabs_wide_data)

# Linear Models
vabs_model1 <- function(data, outcome){
  formula <- as.formula(paste0("out_", outcome, " ~ study + base_age + out_age + base_vabs_com_ae + base_vabs_soc_ae+ base_vabs_dls_ae + base_sex"))
  rms::ols(data = data, formula = formula)
}

vabs_model2 <- function(data, outcome){
  out_var = paste0("out_", outcome)
  base_var = paste0("base_", outcome)
  additional_predictors = sub(base_var, "", " + study + base_age + out_age + base_vabs_com_ae + base_vabs_soc_ae+ base_vabs_dls_ae + base_sex")
  formula <- as.formula(paste(out_var, "~ rcs(", base_var, ", 3)", additional_predictors))
  print(formula)
  model_rms <- rms::ols(formula, data=data)

}

vabs_model3 <- function(data, outcome){
  out_var = paste0("out_", outcome)
  base_var = paste0("base_", outcome)
  additional_predictors = sub(base_var, "", " + study + base_age + out_age + base_vabs_com_ae + base_vabs_soc_ae+ base_vabs_dls_ae + base_sex")
  formula <- as.formula(paste(out_var, "~ rcs(", base_var, ", 3)*base_sex", additional_predictors))
  print(formula)
  model_rms <- rms::ols(formula, data=data)
  
}

for(outcome in c("vabs_com_ae", "vabs_soc_ae", "vabs_dls_ae")){
  print(outcome)
  model1 <- vabs_model1(data = vabs_wide_data, outcome = outcome) 
  model1 |> print()
  model2 <- vabs_model2(data = vabs_wide_data, outcome = outcome) 
  model2 |> print()
  model3 <- vabs_model3(data = vabs_wide_data, outcome = outcome) 
  model3 |> print()
}


# Multilevel models

vabs_ml1 <- function(data, outcome){
  data$study <- factor(data$study)
  additional_predictors = sub(paste0("base_", outcome), "", "  i.study base_vabs_com_ae  base_vabs_soc_ae base_vabs_dls_ae")
  model_command <- glue::glue("
    mkspline age_spline = age, nknots(3) cubic
    gsem ({outcome} <- c.age_spline1##i.base_sex c.age_spline2##i.base_sex {additional_predictors} M1[ID]@1)
   ")
  RStata::stata(model_command, data.in = data)
}

vabs_ml2 <- function(data, outcome){
  data$study <- factor(data$study)
  additional_predictors = sub(paste0("base_", outcome), "", "  i.study base_vabs_com_ae  base_vabs_soc_ae base_vabs_dls_ae")
  model_command <- glue::glue("
    mkspline age_spline = age, nknots(3) cubic
    gsem (outcome <- c.age_spline1##i.base_sex c.age_spline2##i.base_sex {additional_predictors} M1[ID]@1  c.age#M2[ID]@1)
   ")
  RStata::stata(model_command, data.in = data)
}

vabs_ml3 <- function(data){
  data$study <- factor(data$study)
  additional_predictors <- "i.study"
  lp <- glue::glue("c.age_spline1##i.base_sex c.age_spline2##i.base_sex {additional_predictors}")
  model_command <- glue::glue("
    mkspline age_spline = age, nknots(3) cubic
    gsem (vabs_dls_ae <- {lp}  M1[ID]@1) ///
        (vabs_soc_ae <- {lp}  M2[ID]@1) ///
        (vabs_com_ae <- {lp}  M3[ID]@1) ///
        , cov( ///
            e.vabs_dls_ae*e.vabs_soc_ae ///
            e.vabs_dls_ae*e.vabs_com_ae ///
            e.vabs_soc_ae*e.vabs_com_ae ///
          )
      estat sd
   ")
  RStata::stata(model_command, data.in = data)
}



vabs_ml1(vabs_long_data, "vabs_dls_ae")

vabs_ml2(vabs_long_data, "vabs_dls_ae")

tictoc::tic()
vabs_ml3(vabs_long_data)
tictoc::toc()

# For sensitivity analysis 
vabs_wide_data |> filter(fu_length < 5) |> count(study)

vabs_long_data |> filter(wave == base_wave) |> count(n_obs) 
vabs_long_data |> filter(n_obs > 2, wave == base_wave) |> count(study) 

vabs_model1_dls <- function(data) {
  vabs_model1(data, "vabs_dls_ae")
}

vabs_model1_factory <- function(outcome){
  function(data){
    vabs_model1(data, outcome)
  }
}

IPDPredictR::ipdma_prediction_pipeline(
  data = vabs_wide_data,
  model_function = vabs_model1_factory("vabs_dls_ae"),
  study_var_name = "study",
  out_var_name = "out_vabs_dls_ae",
  evaluate_performance = IPDPredictR:::evaluate_performance_continuous_new_studies
)  
