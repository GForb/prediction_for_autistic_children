add_analysis_name <- function(analysis_spec, log_folder, do_folder, suffix = "") {
  analysis_spec |> mutate(
    analysis_name = glue::glue("{model_name}_{outcome}_{predictor_set}{suffix}") |> as.character(),
    log_file = here::here(log_folder, analysis_name),
    do_file = here::here(do_folder, analysis_name),)
}
