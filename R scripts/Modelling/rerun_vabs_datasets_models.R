# Re-runs the VABS sensitivity dataset models (main, no_pa and short_fu datasets) only.
# The short_fu models failed with r(412) in the controller v4 run, as omitted study dummies (EpiTED is dropped
# from short_fu) were constrained in fit_hold_out_gsem. This is fixed in prediction_progs.do.
# The whole block is re-run with its original seed so results match a full run of run_vabs_models.R.
# Requires config_v4.R to be sourced.

# Evaluating run_vabs_models.R up to the creation of the analysis specs, without running any models
vabs_model_code <- parse(here(modelling_scripts, "run_vabs_models.R"))
spec_end <- which(map_lgl(as.list(vabs_model_code), \(x) is.call(x) && identical(x[[1]], as.name("set.seed"))))[1] - 1
for (expr in vabs_model_code[1:spec_end]) eval(expr)

set.seed(6146146)

tictoc::tic()
mt_results_cc <- run_many_models(analysis_spec_multi_timepoint_cc_datasets)
tictoc::toc()
