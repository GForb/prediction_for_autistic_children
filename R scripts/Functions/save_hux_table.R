save_hux_table <- function(hux_table, file_name, label, caption){
  huxtable::set_default_properties(
    font = "mathptmx",
    left_padding = 0.5,
    right_padding = 0.5,
    top_padding = 0.5,
    bottom_padding = 0.5
  )
  
  table <- hux_table |> 
    huxtable::set_top_border(row = 1, value = 0.8) |>
    huxtable::set_bottom_border(row = huxtable::final(1), value = 0.8) |> 
    huxtable::set_caption_pos("bottom") |> 
    huxtable::set_caption(caption) |> 
    huxtable::set_label(paste0("tab:", label)) |> 
    huxtable::set_escape_contents(value = FALSE) |> 
    huxtable::set_font_size(value = 11)
  
  
  
  ht_latex <- table  |> huxtable::to_latex(tabular_only = FALSE) |> str_remove_all(pattern = " \\+ 1em")
  ht_latex |> write_lines(here::here(latex_folder, file_name))
  return(table)
  
}