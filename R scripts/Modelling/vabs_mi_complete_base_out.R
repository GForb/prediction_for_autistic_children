analysis_data_wide <- readRDS(here(derived_data, "pooled_vabs_wide.Rds")) |> 
  filter(base_all_complete, out_all_complete)

plots_folder <- here::here(data_and_outputs, "Results", "VABS", "Imputation Plots")


analysis_data_wide |> 
  pivot_longer(cols = where(is.numeric), names_to = "variable", values_to = "value") |>  
  group_by(variable) |> 
  sum_detail("value") |> 
  print(n = 50)

analysis_data <- analysis_data_wide |> 
  select(starts_with("out_vabs"), out_age, starts_with("base_vabs"), 
         base_age, base_sex, -starts_with("base_vabs_abc"),
         base_maternal_education, base_adi_65, base_ados_css_rrb, base_ados_css_sa,
         base_iq_standard, base_iq_full_scale, base_iq_perceptual,
         base_ethnicity,
         starts_with("study_")) |> 
  mutate(base_iq_full_scale = as.numeric(base_iq_full_scale),
         base_maternal_education = as.factor(base_maternal_education),
         base_iq_standard = as.factor(base_iq_standard),
         base_iq_perceptual = as.numeric(base_iq_perceptual),
         base_adi_65 = as.factor(base_adi_65),
         base_ethnicity = as.factor(base_ethnicity),
         base_maternal_education = as.factor(base_maternal_education))

set.seed(1234)

non_imputed_vars <- analysis_data_wide |> select(ID, study, wave)
tictoc::tic()
imputations <- mice::mice(analysis_data, maxit = 100, print=FALSE, m = 50)
tictoc::toc()
imputed_data <- mice::complete(imputations, action = "all", mild = FALSE) |> 
  map(~bind_cols(., non_imputed_vars))

saveRDS(imputed_data, here::here(derived_data, "vabs_imputed_wide_complete_base_out.Rds"))



imp_plot <- plot(imputations)
imp_plot
imp_strip_plot <-  mice::stripplot(imputations)
imp_strip_plot

imputation_plots <- list(imp_plot, imp_strip_plot)

saveRDS(imputation_plots, here::here(imputation_plots, "imp_plots_complete_base_out.RDS"))
