select_analysis_variables <- function(data, var_metadata = var_metadata) {
  analysis_vars <- var_metadata$variable_name
  data |> select(any_of(analysis_vars))
}