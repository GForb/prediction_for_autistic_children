add_follow_up_length <- function(data) {
   out_data <- data |> filter(wave == out_wave)
  base_data <- data |> filter(wave == base_wave)
  fu_lengths <- inner_join(base_data, out_data, by = "ID") |> 
    mutate(fu_length = age.y - age.x) |> 
    select(ID, fu_length)
  
  data |> left_join(fu_lengths, by = "ID", )
}