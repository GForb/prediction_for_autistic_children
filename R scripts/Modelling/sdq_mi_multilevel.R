analysis_data_long <- readRDS(here(derived_data, "pooled_sdq.Rds"))

# Missing data is at level 2: The cluster level (cluster = indivudal)


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
  select(ID, starts_with("sdq"), age_c,
         base_sex,,
         base_maternal_education, base_imd_decile, base_maternal_mh, base_ethnicity,
         base_ld,
         starts_with("study_")) |> 
  mutate(across(where(is.numeric), as.numeric)) |> 
  mutate(base_maternal_education = as.factor(base_maternal_education),
         base_ld = as.factor(base_ld),
         base_ethnicity = as.factor(base_ethnicity),
         base_maternal_education = as.factor(base_maternal_education)) |> 
  left_join(id_nums) |> 
  select(ID_num, everything(), -ID)




# Setting up model: Following guide in this chapter - https://bookdown.org/mwheymans/bookmi/multiple-imputation-models-for-multilevel-data.html#multilevel-data---example-datasets

## Set imputation method for all variables (except study)

continuous_to_be_imputed <- analysis_data |> select(-ID_num, -starts_with("study_"), -age_c, -base_ld, -base_maternal_education, -base_sex, -base_ethnicity)
binary_to_be_imputed <- analysis_data |> select(base_ld, base_maternal_education, base_sex, base_ethnicity)
data_not_to_be_imputed <- analysis_data |> select(ID_num, starts_with("study_"), age_c)


impmethod_cont <- rep("2l.lmer", ncol(continuous_to_be_imputed))
names(impmethod_cont) <- colnames(continuous_to_be_imputed)

imp_method_bin <- rep("2l.bin", ncol(binary_to_be_imputed))
names(imp_method_bin) <- colnames(binary_to_be_imputed)

impmethod_blank <- character(ncol(data_not_to_be_imputed))
names(impmethod_blank) <- colnames(data_not_to_be_imputed)

impmethod <- c(impmethod_cont, imp_method_bin, impmethod_blank)

impmethod

impmethod_test <- character(ncol(analysis_data))
names(impmethod_test) <- colnames(analysis_data)

impmethod_test["sdq_hyp_p"] <- "2l.lmer"

impmethod_test

## Making the prediction matrix
# see: https://bookdown.org/mwheymans/bookmi/multiple-imputation.html#customizing-the-imputation-model-1
# The variables in the columns are used to impute the row variables
# I don't have restricted cuvic splines in the model! AHHHH. At least I am creating splines in complete data - am I - mostly... Uh oh.

pm <- mice::make.predictorMatrix(analysis_data)
pm1 <- pm
pm1[, "ID_num"] <- -2
pm2 <- pm1
pm2[, "age_c"] <- 2


pm
pm1
pm2


# Run imputations
set.seed(1234)

length(impmethod_test)
nrow(pm)
ncol(pm)

colnames(pm) %in% names(impmethod_test)
rownames(pm) %in% names(impmethod_test)

names(impmethod_test) %in% colnames(pm)
names(impmethod_test) %in% rownames(pm)


tictoc::tic()
imputations <- mice::mice(analysis_data, predictorMatrix = pm1, method = impmethod_test , m = 1, maxit = 5)
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


