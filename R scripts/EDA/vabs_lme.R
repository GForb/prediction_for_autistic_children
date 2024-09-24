library(lme4)
library(numDeriv)
library(RCurl) 

vabs_long_data <- readRDS(here(derived_data, "pooled_vabs.Rds")) |> filter(base_all_complete, out_all_complete, all_complete) 

spline_stata_code <- "
    mkspline age_spline = age_c , nknots(3) cubic
    su age_spline*  
"

analysis_data <- RStata::stata(spline_stata_code, data.in = vabs_long_data, data.out = TRUE) 

analysis_data <- analysis_data|> 
  mutate(age_spline1Xsex = age_spline1*base_sex,
         age_spline2Xsex = age_spline2*base_sex,
         age_spline1Xdq = age_spline1*base_vabs_dq/10,
         age_spline2Xdq = age_spline2*base_vabs_dq/10)

id_and_study <- analysis_data |> 
  select(ID, study, base_sex, starts_with("study_")) 

continusous_variables <- analysis_data |> 
  select(starts_with("vabs"), starts_with("base_vabs"), starts_with("age_spline"), age_c ) 

continuous_variables_scaled <- continusous_variables |> scale() |> as.data.frame()

analysis_data_scaled <- bind_cols(id_and_study, continuous_variables_scaled)





analysis_spec <- tibble(outcome = c("vabs_com_ae", "vabs_dls_ae", "vabs_soc_ae")) |> 
  rowwise() |> 
  mutate(non_outcome_baseline  = stringr::str_remove_all(string = "base_vabs_com_ae base_vabs_soc_ae base_vabs_dls_ae", pattern = paste0("base_", outcome)),
         predictors = glue::glue("age_spline1 age_spline2 base_vabs_dq base_sex age_spline1Xsex age_spline2Xsex age_spline1Xdq age_spline2Xdq  {non_outcome_baseline}"),
         predictor_formula = get_predictors_fixed_study(analysis_data, predictors),
         my_formula = glue::glue("{outcome} ~ {predictor_formula} + (age_c|ID)") 
  )

models <- map(analysis_spec$my_formula, \(my_formula) lme4::lmer(my_formula |> as.formula(), data = analysis_data, REML = TRUE))
models_scaled <- map(analysis_spec$my_formula, \(my_formula) lme4::lmer(my_formula |> as.formula(), data = analysis_data_scaled, REML = FALSE))

model1 <- models_scaled[[1]]
pred_re <- lme4::ranef(model1)$ID 
my_re <- IPDPredictR:::eb(model1, newdata = analysis_data_scaled)[,-1]

pred_re - my_re

pred_re[1,]
my_re[1,]



analysis_data_scaled_no_pathways <- analysis_data_scaled |> filter(study != "Pathways") |> select(-study_Pathways)

analysis_spec_np <- tibble(outcome = c("vabs_com_ae", "vabs_dls_ae", "vabs_soc_ae")) |> 
  rowwise() |> 
  mutate(non_outcome_baseline  = stringr::str_remove_all(string = "base_vabs_com_ae base_vabs_soc_ae base_vabs_dls_ae", pattern = paste0("base_", outcome)),
         predictors = glue::glue("age_spline1 age_spline2 base_vabs_dq base_sex age_spline1Xsex age_spline2Xsex age_spline1Xdq age_spline2Xdq  {non_outcome_baseline}"),
         predictor_formula = get_predictors_fixed_study(analysis_data_scaled_no_pathways, predictors),
         my_formula = glue::glue("{outcome} ~ {predictor_formula} + (age_c|ID)") 
  )

models_np <- map(analysis_spec_np$my_formula, \(my_formula) lme4::lmer(my_formula |> as.formula(), data = analysis_data_scaled_no_pathways, REML = FALSE))

model3 <- models_np[[3]]
model2 <- models_np[[2]]


pred_re <- lme4::ranef(model3)$ID 
my_re <- IPDPredictR:::eb(model3, newdata = analysis_data_scaled_no_pathways)[,-1]

pred_re - my_re

pred_re[1,]
my_re[1,]



# How to make predictions for a new study

# Steps: 
# 1. predict fixed effect (excluding study term)
# 2. Predict study term
# 3. Predict random terms based on fixed part of the model.

