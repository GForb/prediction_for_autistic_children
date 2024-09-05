analysis_data_long<- readRDS(here(derived_data, "pooled_vabs.Rds")) |> filter(base_all_complete, out_all_complete) 

IDs<- analysis_data_long |> pull(ID) |> unique() 
id_nums <- IDs |> tibble() |> 
  mutate(id_num = 1:length(IDs)) |> 
  rename(ID = IDs)

analysis_data_long <- analysis_data_long |> left_join(id_nums)

train_data <- analysis_data_long |> 
  filter(study != "Pathways") |> 
  select(-study_Pathways)

predictors <- get_predictors_fixed_study(data = train_data, predictors = "age_c  base_sex")
mixed_formula <- glue::glue("vabs_dls_ae ~ {predictors} + (1 + age_c| id_num) ") |> as.formula()

mixed_model <- lme4::lmer(mixed_formula, data = train_data, REML = FALSE)

lme4::isSingular(mixed_model, tol = 1e-5)
lme4::isSingular(mixed_model, tol = 1e-6)
lme4::isSingular(mixed_model, tol = 1e-7)
lme4::isSingular(mixed_model, tol = 1e-8)




# how to predit a study intercept for fixed study
  
# Options: Implement my own post estimation predictor for mixed in stata
# This shouldn't be too hard.
  
# Use gsem to estimate intercepts - that is hard.