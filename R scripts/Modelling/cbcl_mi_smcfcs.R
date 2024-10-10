library(micemd) # necessary to load library so calls to micemd imputation methods work.
library(smcfcs)

analysis_data_wide <- readRDS(here(derived_data, "pooled_cbcl_wide.Rds")) |> filter(base_all_complete, out_all_complete) 
analysis_data_wide |> count(study)
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


analysis_data_all <- analysis_data_spline |> 
  select(starts_with("study_"), starts_with("out_cbcl"), out_age, starts_with("base_cbcl"), 
         base_age, base_sex, base_adi_65, base_ados_css_rrb, base_ados_css_sa,
         base_iq_standard, base_iq_perceptual, base_iq_full_scale, base_vabs_abc_ss,
         base_maternal_education , base_ethnicity) |> 
  mutate(across(where(is.numeric), as.numeric),
         base_iq_standard = case_when(is.na(base_iq_standard) ~ 1,
                                             TRUE ~base_iq_standard),
         base_iqXstandard = base_iq_standard*base_iq_full_scale) 

# start small and build up 
get_data_formula <- function(analysis_data, domain) {
  domains <- c("aff", "anx", "som", "adhd", "odd", "con")
  baseline_outcomes_string = paste0("base_cbcl_", domains)
  non_outcome_baseline = baseline_outcomes_string[baseline_outcomes_string != paste0("base_cbcl_", domain)]
  non_outcome_baseline_string <- paste(non_outcome_baseline, collapse = " + ")
  spline_terms <- c("spline1", "spline2", "spline1Xsex" ,"spline2Xsex")
  spline_predictors <- paste0("base_cbcl_", domain, "_", spline_terms)
  spline_predictors_string <- paste(spline_predictors, collapse = " + ")
  study_terms <- analysis_data |> select(starts_with("study_"), -study_Pathways) |> colnames() |> paste0(collapse = " + ") # Remove pathways as model is estimated with intercept.
  other_predictors <- "
    base_sex + 
    base_iq_full_scale + 
    base_ados_css_rrb +
    base_ados_css_sa +
    base_vabs_abc_ss + 
    base_ethnicity +  
    base_iq_standard  + 
    base_maternal_education + 
    base_adi_65 + 
    base_iqXstandard +
    base_iq_perceptual"    




  outcome <- paste0("out_cbcl_", domain)
  
  formula_string <- glue::glue("
    {outcome} ~  out_age + base_age + {spline_predictors_string} + {other_predictors} + {study_terms} + {non_outcome_baseline_string}
  ")

  analysis_data <- analysis_data |> 
    select(
      everything(), 
      starts_with("study"),
      -study_Pathways,
      -starts_with("out_cbcl"), 
      -starts_with("base_cbcl"),
      any_of(c(outcome)) , non_outcome_baseline , spline_predictors
    )
  analysis_data_method <- analysis_data |> 
    mutate(ind.clust = 1,)
  method <- micemd::find.defaultMethod(analysis_data_method, "ind.clust")
  method <- method[-length(method)]
  method[method == "2l.2stage.norm"] <- "norm"
  method[method == "2l.glm.norm"] <- "norm"
  method$base_maternal_education <- "brlogreg"
  method$base_ethnicity <- "brlogreg"
  method$base_iqXstandard <- "base_iq_standard*base_iq_full_scale"
  method <- unlist(method)
  
    return(list(
      data = analysis_data, 
      formula = formula_string, 
      method = method
      ))
}

data_formula_aff <- analysis_data_all |> get_data_formula("aff")
data_formula_aff$method |> length()
data_formula_aff$data |> ncol()

data_formula_aff$method |> names() %in% data_formula_aff$data  |> colnames() 
# Test imputation run

set.seed(123456)

tictoc::tic()
imps_aff <- smcfcs(
  originaldata = data_formula_aff$data,
  smtype = "lm",
  smformula = data_formula_aff$formula,
  method = data_formula_aff$method,
  m = 50,
  numit = 20,
  rjlimit = 10000
  )
tictoc::toc()

imps_aff |> plot()

get_inp_data <- function(imp) {
  imp_data <- imp$impDatasets
  return(imp_data)
}

aff_data <- get_inp_data(imps_aff)

run_imps <- function(domain) {
  data_formula <- analysis_data_all |> get_data_formula(domain)
  
  tictoc::tic()
  imps <- smcfcs(
    originaldata = data_formula$data,
    smtype = "lm", 
    smformula = data_formula$formula,
    method = data_formula$method,
    m = 50,
    numit = 20,
    rjlimit = 10000
  )
  warnings() |> print()
  plot(imps)
  
  tictoc::toc()
  saveRDS(imps, file = file.path(results_folder, paste0("raw_imps_", domain, ".Rds")))

  return(imps)
  
}

# domains <- c("aff", "anx", "som", "adhd", "odd", "con")
# 

results_folder <- here::here(data_and_outputs, "Results", "CBCL", "Prelim")

set.seed(123456)
imps_aff <- run_imps("aff")

set.seed(61564)
imps_anx <- run_imps("anx")

set.seed(79254)
imps_som <- run_imps("som")

set.seed(31541)
imps_adhd <- run_imps("adhd")

set.seed(77225588)
imps_odd <- run_imps("odd")

set.seed(11654168)
imps_con <- run_imps("con")

cbcl_domains <- c("cbcl_aff", "cbcl_anx", "cbcl_som", "cbcl_adhd", "cbcl_odd", "cbcl_con")

imps_list <- list(
  cbcl_aff = imps_aff,
  cbcl_anx = imps_anx,
  cbcl_som = imps_som,
  cbcl_adhd = imps_adhd,
  cbcl_odd = imps_odd,
  cbcl_con = imps_con
)

imp_data_list <- map(imps_list, get_inp_data)
names(imp_data_list) <- cbcl_domains

saveRDS(imp_data_list, file = file.path(derived_data, here::here(derived_data, "cbcl_imputed_smcfcs.Rds")))



