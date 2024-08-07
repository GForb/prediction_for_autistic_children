make_wide_dataset <- function(data) {
  data |>   pivot_wider(
    names_from = wave,
    names_glue = "{.value}_{wave}",
    values_from = -all_of(c("ID", "study", "country"))
  )
}

make_long_dataset <- function(data) {
  data |>   pivot_longer(
    cols = -all_of("ID"),
    names_to = c(".value","wave"),
    names_pattern = "(.*)_(.)"
  )
}

