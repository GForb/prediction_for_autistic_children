analysis_data_long <- readRDS(here(derived_data, "pooled_vabs.Rds")) |> 
  filter(base_all_complete, out_all_complete, all_complete) # exclude people missing a single value in one outcome.

# Missing data is at level 2: The cluster level (cluster = individual) - this is the script to use to investigate how to impute missing data.
# 1% of data are partially complete - to drop or not to drop? - wont introduce bias

plots_folder <- here::here(data_and_outputs, "Results", "VABS", "Imputation Plots")

id_nums <- analysis_data_long |> 
  select(ID) |> 
  unique() |> 
  mutate(ID_num = row_number()) 

analysis_data_long |> 
  pivot_longer(cols = where(is.numeric), names_to = "variable", values_to = "value") |>  
  group_by(variable) |> 
  sum_detail("value") |> 
  print(n = 50)


spline_stata_code <- "
    mkspline age_spline = age_c , nknots(3) cubic
    gen age_spline1Xsex = age_spline1*base_sex
    gen age_spline2Xsex = age_spline2*base_sex
    gen age_spline1Xdq = age_spline1*base_vabs_dq/10,
    gen age_spline2Xdq = age_spline2*base_vabs_dq/10
    su age_spline*  
"

analysis_data_long <- RStata::stata(spline_stata_code, data.in = analysis_data_long, data.out = TRUE) 

saveRDS(analysis_data_long, here::here(derived_data, "pooled_vabs_spline.Rds"))

analysis_data <- analysis_data_long |> 
  select(ID, study,  starts_with("vabs"), starts_with("age_spline"), age_c,
         base_sex, starts_with("base_vabs"), - base_vabs_abc_ss,
         base_adi_65, base_ados_css_rrb, base_ados_css_sa, base_iq_full_scale,
         base_iq_standard, base_maternal_education, base_ethnicity) |> 
  mutate(across(where(is.numeric), as.numeric)) |> 
  mutate(base_maternal_education = as.factor(base_maternal_education),
         base_iq_standard = as.factor(base_iq_standard),
         base_ethnicity = as.factor(base_ethnicity),
         study = as.factor(study),
         base_iqXmethod = as.numeric(base_iq_standard) * base_iq_full_scale) |> 
  left_join(id_nums) |> 
  select(ID_num, study, everything(), -ID, -age_c) # dropping age_c as will be equal to first spline term


# Setting up model: Following guide in this chapter - https://bookdown.org/mwheymans/bookmi/multiple-imputation-models-for-multilevel-data.html#multilevel-data---example-datasets

## Set imputation method for all variables (except study)
ind.clust<- 1
methods <- analysis_data |> micemd::find.defaultMethod( ind.clust)
methods[methods != ""] <- "2lonly.pmm"

# For details if 2lonly imputation function see https://stefvanbuuren.name/fimd/sec-level2pred.html



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

# Run imputations separately in those with and without standard tests - what if there is missing data in the interaction term?



imputed_data <- mice::complete(imputations, action = "all", mild = FALSE) |> 
  map(~bind_cols(., non_imputed_vars) |> mutate(study = as.character(study)))

saveRDS(imputed_data, here::here(derived_data, "vabs_imputed_ml.Rds"))

plot(imputations)


# 
mice::stripplot(imputations, 
               base_adi_65 +
                 base_ados_css_rrb +
                 base_ados_css_sa +
                 base_iq_full_scale +
                 base_iq_standard +
                 base_maternal_education +
                 base_ethnicity + 
                 base_iqXmethod
                 ~ .imp)

