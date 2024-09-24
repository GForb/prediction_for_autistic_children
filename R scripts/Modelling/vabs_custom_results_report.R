results_folder <- here::here(data_and_outputs, "Results", "VABS", "Thesis")


mymodel_names <- c("mt_fi_study_rs", "mt_fi_study_ri")
myoutcomes <- c("vabs_dls_ae") # 
mypredictor_set = "pred3_mt"


results_folder <- here::here(data_and_outputs, "Results", "VABS", "Thesis")
analysis_spec <- readRDS(here::here(results_folder, "analysis_spec.RDS"))



myAnalysis <- analysis_spec |> 
  filter(
    predictor_set  %in% mypredictor_set, 
    outcome %in% myoutcomes, 
    model_name %in% mymodel_names)

tictoc::tic()
myResults <- create_full_results_table(results_folder, analysis_spec = myAnalysis)
tictoc::toc()
