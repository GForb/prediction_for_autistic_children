analysis_data_long <- readRDS(here(derived_data, "pooled_sdq.Rds"))

# Missing data is at level 2: The cluster level (cluster = indivudal)


plots_folder <- here::here(data_and_outputs, "Results", "SDQ", "Imputation Plots")


analysis_data_long |> 
  pivot_longer(cols = where(is.numeric), names_to = "variable", values_to = "value") |>  
  group_by(variable) |> 
  sum_detail("value") |> 
  print(n = 50)

analysis_data <- analysis_data_long |> 
  select(starts_with("sdq"), age_c, 
         base_sex,,
         base_maternal_education, base_imd_decile, base_maternal_mh, base_ethnicity,
         base_ld,
         starts_with("study_")) |> 
  mutate(across(where(is.numeric), as.numeric)) |> 
  mutate(base_maternal_education = as.factor(base_maternal_education),
         base_ld = as.factor(base_ld),
         base_ethnicity = as.factor(base_ethnicity),
         base_maternal_education = as.factor(base_maternal_education))

set.seed(1234)

non_imputed_vars <- analysis_data_wide |> select(ID, study, wave)
tictoc::tic()
imputations <- mice::mice(analysis_data, maxit = 100, print=FALSE, m = 50)
tictoc::toc()
imputed_data <- mice::complete(imputations, action = "all", mild = FALSE) |> 
  map(~bind_cols(., non_imputed_vars))

saveRDS(imputed_data, here::here(derived_data, "sdq_imputed_wide.Rds"))



imp_plot <- plot(imputations)
imp_plot
imp_strip_plot <-  mice::stripplot(imputations)
imp_strip_plot

#imputation_plots <- list(imp_plot, imp_strip_plot)

# saveRDS(imputation_plots, here::here(imputation_plots, "imp_plots.RDS"))


