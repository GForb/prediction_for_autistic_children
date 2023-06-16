make_wide_dataset <- function(data) {
  data |>   pivot_wider(
    id_cols = ID,
    names_from = wave,
    names_glue = "{.value}_{wave}",
    values_from = -all_of("ID")
  )
  
}

wide_data <-  make_wide_dataset(ssc_data)

wide_data |> 
  select(starts_with("cbcl_int_total")) |> 
  cor(use = "complete.obs")

ssc_data |> filter(wave ==1)
ssc_data |> filter(wav  
                   
                     e ==2)