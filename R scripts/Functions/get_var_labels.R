get_var_labels <- function(data){
  labels = data %>% map(attr_getter("label")) |> unlist()
  label_lookup_map <- tibble(
    col_name = labels %>% names(),
    labels = labels
  )
  return(label_lookup_map)
}