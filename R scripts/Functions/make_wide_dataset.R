make_wide_dataset <- function(data) {
  data |>   pivot_wider(
    id_cols = ID,
    names_from = wave,
    names_glue = "{.value}_{wave}",
    values_from = -all_of(c("ID", "wave"))
  )
}
