add_autistic_any_wave <- function(data) {
  autistic_id <- data |> filter(autism ==1) |> select(ID) |>  unique() |> tibble() |> mutate(autistic_any_wave = 1)
  data |> left_join(autistic_id, by = "ID")
}