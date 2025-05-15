save_hux_table <- function(hux_table, file_name, label, caption, short_caption = "", font_size = 11, padding = 0.5, htb = FALSE){
  huxtable::set_default_properties(
    font = "mathptmx",
    left_padding = padding,
    right_padding = padding,
    top_padding = padding,
    bottom_padding = padding
  )
  
  table <- hux_table |> 
    huxtable::set_top_border(row = 1, value = 0.8) |>
    huxtable::set_bottom_border(row = huxtable::final(1), value = 0.8) |> 
    huxtable::set_caption_pos("bottom") |> 
    huxtable::set_caption(caption) |> 
    huxtable::set_label(paste0("tab:", label)) |> 
    huxtable::set_escape_contents(value = FALSE) |> 
    huxtable::set_font_size(value = font_size)
  
  
  
  ht_latex <- table  |> huxtable::to_latex(tabular_only = FALSE) |> str_remove_all(pattern = " \\+ 1em")
  if(htb){
    ht_latex <- ht_latex |> str_replace("\\[ht\\]", "[!htb]")
  }
  if(short_caption != ""){
    ht_latex <- ht_latex |> str_replace("\\caption", glue::glue("\\caption[{short_caption}]"))
  }
  ht_latex |> write_lines(here::here(latex_folder, file_name))
  return(table)
  
}