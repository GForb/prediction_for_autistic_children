analysis_data_long <- readRDS(here(derived_data, "pooled_sdq.Rds")) |> 
  filter(base_all_complete, out_all_complete, autism != "post baseline", all_complete) # exclude people missing a single value in one outcome.

# Missing data is at level 2: The cluster level (cluster = individual) - this is the script to use to investigate how to impute missing data.
# 1% of data are partially complete - to drop or not to drop? - wont introduce bias

plots_folder <- here::here(data_and_outputs, "Results", "SDQ", "Imputation Plots")

id_nums <- analysis_data_long |> 
  select(ID) |> 
  unique() |> 
  mutate(ID_num = row_number()) 

analysis_data_long |> 
  pivot_longer(cols = where(is.numeric), names_to = "variable", values_to = "value") |>  
  group_by(variable) |> 
  sum_detail("value") |> 
  print(n = 50)

analysis_data <- analysis_data_long |> 
  select(ID, study,  starts_with("sdq"), age_c,
         base_sex, starts_with("base_sdq"),
         base_maternal_education, base_imd_decile, base_maternal_mh, base_ethnicity,
         base_ld) |> 
  mutate(across(where(is.numeric), as.numeric)) |> 
  mutate(base_maternal_education = as.factor(base_maternal_education),
         base_ld = as.factor(base_ld),
         base_ethnicity = as.factor(base_ethnicity),
         base_maternal_education = as.factor(base_maternal_education),
         study = as.factor(study)) |> 
  left_join(id_nums) |> 
  select(ID_num, study, everything(), -ID, )


spline_stata_code <- "
    mkspline age_spline = age_c , nknots(3) cubic
    gen age_spline1Xsex = age_spline1*base_sex
    gen age_spline2Xsex = age_spline2*base_sex
    su age_spline*  
"
analysis_data <- RStata::stata(spline_stata_code, data.in = analysis_data, data.out = TRUE) |> 
  select(-age_c) # dropping age_c as will be equal to first spline term

# Setting up model: Following guide in this chapter - https://bookdown.org/mwheymans/bookmi/multiple-imputation-models-for-multilevel-data.html#multilevel-data---example-datasets

## Set imputation method for all variables (except study)
ind.clust<- 1
methods <- analysis_data |> micemd::find.defaultMethod( ind.clust)
methods[c("base_maternal_education",
          "base_imd_decile",
          "base_maternal_mh",
          "base_ethnicity",
          "base_ld")] <- "2lonly.pmm"

# For details if 2lonly imputation function see https://stefvanbuuren.name/fimd/sec-level2pred.html

"age_spline1 age_spline2 ///
  base_sex ///
   c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
  {non_outcome_baseline}"

# Define appropriate methoods
# Define appropriate predictor matrix
# - what if missing data is only at level 2?
# Define approprieate predictor matrix
# Run once and see :)


predictor.matrix <- mice::make.predictorMatrix(analysis_data)
predictor.matrix[-ind.clust,ind.clust]<- -2
predictor.matrix[-ind.clust,ind.clust]<- -2

# Include cluster means of all lvl 1 variables
# Include cluster means of all lvl 1 interactions
# Include all lvl 2 variables
# include all lvl 2 interactions

# Run imputations
set.seed(1234)


tictoc::tic()
imputations <- mice::mice(analysis_data, predictorMatrix = predictor.matrix , method = methods , m = 50, maxit = 20)
tictoc::toc()

non_imputed_vars <- analysis_data_long |> select(ID, starts_with("study_"), age_c, wave)

imputed_data <- mice::complete(imputations, action = "all", mild = FALSE) |> 
  map(~bind_cols(., non_imputed_vars) |> mutate(study = as.character(study)))

saveRDS(imputed_data, here::here(derived_data, "sdq_imputed_ml.Rds"))


# 
# imp_plot <- plot(imputations)
# 
# png(here::here(imputation_plots, "imp_plot_sdq_ml.png"))
# imp_plot
# dev.off()
# 
# imp_strip_plot <-  mice::stripplot(imputations, 
#                                    base_imd_decile +
#                                      base_maternal_mh +
#                                      base_maternal_education + 
#                                      base_ld + 
#                                      base_ethnicity  ~ .imp)
# imp_strip_plot
# 
# png(here::here(imputation_plots, "imp_strip_plot_sdq_ml.png"))
# imp_strip_plot
# dev.off()

