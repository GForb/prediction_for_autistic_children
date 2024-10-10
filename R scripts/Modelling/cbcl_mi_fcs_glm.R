library(micemd) # necessary to load library so calls to micemd imputation methods work.

analysis_data_wide <- readRDS(here(derived_data, "pooled_cbcl_wide.Rds")) |> filter(base_all_complete, out_all_complete) 

# Add spline variables for cbcl - think about how this is done with the MI models - do I include in every imputation?

plots_folder <- here::here(data_and_outputs, "Results", "CBCL", "Imputation Plots") # For some reason this doesn't work.

# Adding additional model terms:
# Spline for each cbcl domain using Stata mkspline
# Interaction term between sex and each model term.

# adding base_splines
# This will cause a problem for the analysis - if basesplines are already created...

spline_stata_code <- "
  foreach domain in aff anx som adhd odd con {
    mkspline base_cbcl_`domain'_spline = base_cbcl_`domain', nknots(3) cubic
    gen base_cbcl_`domain'_spline1Xsex = base_cbcl_`domain'_spline1*base_sex
    gen base_cbcl_`domain'_spline2Xsex = base_cbcl_`domain'_spline2*base_sex
    
    su base_cbcl_`domain'_spline*
  }
"

analysis_data_spline <- RStata::stata(spline_stata_code, data.in = analysis_data_wide, data.out = TRUE)



# Check which analysis model this will be used for and make appropriate spline.
# Wide data: For single timepoint analysis


analysis_data <- analysis_data_spline |> 
  mutate(study_no = factor(study) |> as.numeric()) |> 
  select(study_no, starts_with("out_cbcl"), out_age, starts_with("base_cbcl"), 
         base_age, base_sex, base_adi_65, base_ados_css_rrb, base_ados_css_sa,
         base_iq_standard, base_iq_perceptual, base_iq_full_scale, base_vabs_abc_ss,
         base_maternal_education , base_ethnicity) |> 
  mutate(across(where(is.numeric), as.numeric),
         base_iqXstandard = base_iq_standard*base_iq_full_scale)  |> 
  mutate(base_iq_standard = as.factor(base_iq_standard),
         base_ethnicity = as.factor(base_ethnicity),
         base_maternal_education = as.factor(base_maternal_education))

str(analysis_data)

ind.clust <- 1
predictor.matrix <- mice::mice(analysis_data,m=1,maxit=0)$pred
predictor.matrix[ind.clust,ind.clust] <- 0
predictor.matrix[-ind.clust,ind.clust] <- -2

method<-micemd::find.defaultMethod(analysis_data, ind.clust)
method["base_ethnicity"] <-  "2l.glm.bin"
method["base_iq_standard"] <-  "2l.glm.bin"

set.seed(1234)

non_imputed_vars <- analysis_data_wide |> select(ID, study, wave, starts_with("base_cbcl"))
tictoc::tic()
imputations <- mice::mice(analysis_data, 
                          predictorMatrix = predictor.matrix,
                          method = method,
                          maxit = 10, print=TRUE, m = 1) 
tictoc::toc()