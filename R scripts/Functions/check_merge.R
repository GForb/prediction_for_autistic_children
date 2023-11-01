check_merge <- function(data, variable) {
  data |> 
    select(ID, wave, all_of(variable)) |> 
    make_wide_dataset() |> 
    select(-ID) |> 
    mice::md.pattern()
}