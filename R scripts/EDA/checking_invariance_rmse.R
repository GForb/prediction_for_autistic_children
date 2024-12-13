# This analysis is run to check why RMSE is so stable when calibration changes.

# Load some results

longitudinal <- readRDS(here::here(data_and_outputs, "Results", "CBCL", "Thesis", "mt_fi_study_ri_cbcl_adhd_pred1_mt_estimate_cv.rds")) |> 
  mutate(model = "longitudinal")
st <- readRDS(here::here(data_and_outputs, "Results", "CBCL", "Thesis", "st_fi_study_cbcl_adhd_pred1_estimate_cv.rds")) |> 
  mutate(model = "st")
st_av <- readRDS(here::here(data_and_outputs, "Results", "CBCL", "Thesis", "st_fi_study_cbcl_adhd_pred1_average.rds")) |> 
  mutate(model = "st_av")






errors <- bind_rows(longitudinal, st, st_av) |>
  select(ID, study, model, actual, pred) |> 
  mutate(error = actual - pred,
         sq_error = error^2)



errors |> group_by(model) |> summarise(rmse = sqrt(mean(sq_error))/14)


cbcl_n_items<- tibble(
  outcome = c(
    "cbcl_aff",
    "cbcl_anx",
    "cbcl_som",
    "cbcl_adhd",
    "cbcl_odd",
    "cbcl_con"
  ),
  n_items = c(13, 6, 7, 7, 5, 17)
)