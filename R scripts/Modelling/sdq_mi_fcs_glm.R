library(micemd) # necessary to load library so calls to micemd imputation methods work.


analysis_data_wide <- readRDS(here(derived_data, "pooled_sdq_wide.Rds")) |> filter(base_all_complete, out_all_complete, autism != "post baseline") 

# Add spline variables for SDQ - think about how this is done with the MI models - do I include in every imputation?

plots_folder <- here::here(data_and_outputs, "Results", "SDQ", "Imputation Plots")

# Adding additional model terms:
# Spline for each sdq domain using Stata mkspline
# Interaction term between sex and each model term.

# adding base_splines
# This will cause a problem for the analysis - if basesplines are already created...

spline_stata_code <- "
  foreach domain in emot cond hyp peer pro {
    mkspline base_sdq_`domain'_p_spline = base_sdq_`domain'_p, nknots(3) cubic
    gen base_sdq_`domain'_p_spline1Xsex = base_sdq_`domain'_p_spline1*base_sex
    gen base_sdq_`domain'_p_spline2Xsex = base_sdq_`domain'_p_spline2*base_sex
    
    su base_sdq_`domain'_p_spline*
  }
"

analysis_data_spline <- RStata::stata(spline_stata_code, data.in = analysis_data_wide, data.out = TRUE)



# Check which analysis model this will be used for and make appropriate spline.
# Wide data: For single timepoint analysis


analysis_data <- analysis_data_spline |> 
  mutate(study_no = factor(study) |> as.numeric()) |> 
  select(study_no, starts_with("out_sdq"), out_age, starts_with("base_sdq"), 
         base_age, base_sex,
         base_maternal_education, base_imd_decile, base_maternal_mh, base_ethnicity,
         base_ld) |> 
  select(-base_sdq_cond_p, -base_sdq_emot_p, -base_sdq_peer_p, -base_sdq_pro_p, -base_sdq_hyp_p) |> # removing main terms for sdq as spline terms will be used instead
  mutate(across(where(is.numeric), as.numeric)) |> 
  mutate(base_maternal_education = as.factor(base_maternal_education),
         base_ld = as.factor(base_ld),
         base_ethnicity = as.factor(base_ethnicity),
         base_maternal_education = as.factor(base_maternal_education))

ind.clust <- 1
predictor.matrix <- mice::mice(analysis_data,m=1,maxit=0)$pred
predictor.matrix[ind.clust,ind.clust] <- 0

method<-micemd::find.defaultMethod(analysis_data, ind.clust)
method["base_ethnicity"] <-  "2l.glm.bin"
set.seed(1234)

non_imputed_vars <- analysis_data_wide |> select(ID, study, wave, base_sdq_cond_p, base_sdq_emot_p, base_sdq_peer_p, base_sdq_pro_p, base_sdq_hyp_p)
tictoc::tic()
imputations <- mice::mice(analysis_data, 
                                predictorMatrix = predictor.matrix,
                                method = method,
                                maxit = 20, print=TRUE, m = 50) 
tictoc::toc()
imputed_data <- mice::complete(imputations, action = "all", mild = FALSE) |> 
  map(~bind_cols(., non_imputed_vars))

saveRDS(imputed_data, here::here(derived_data, "sdq_imputed_wide_fcs_glm.Rds"))



imp_plot <- plot(imputations)
imp_plot
imp_strip_plot <-  mice::stripplot(imputations, 
                                   base_imd_decile +
                                   base_maternal_mh +
                                   base_maternal_education + 
                                   base_ld + 
                                   base_ethnicity  ~ .imp)
imp_strip_plot
# 
# imputation_plots <- list(imp_plot, imp_strip_plot)
# 
# saveRDS(imputation_plots, here::here(imputation_plots, "imp_plots_sdq_fcs_glm.RDS"))


