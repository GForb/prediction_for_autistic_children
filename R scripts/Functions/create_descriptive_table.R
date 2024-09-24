create_descriptive_table <- function(data, order = NULL, study_names = NULL) {

  table <- data |> mutate(study = "zOverall") |> 
    bind_rows(data) |> 
    pivot_longer(cols = -c(ID, study), names_to = "variable", values_to = "score") |>  
    group_by(study, variable) |> 
    summarise(
      total_n = n(),
      N = sum(!is.na(score)),
      Mean = mean(score, na.rm = TRUE),
      SD = sd(score, na.rm = TRUE),
      min = min(score, na.rm = TRUE),
      max = max(score, na.rm = TRUE),
      n_pos = sum(score ==1, na.rm = TRUE)) |> 
    ungroup() |> 
    mutate(per = n_pos/N*100,
           per_comp = N/total_n*100) |> 
    select(-total_n) |>
    pivot_longer(values_to = "summary", cols = c(N, Mean, SD, min, max, n_pos, per, per_comp)) |>
    left_join(var_metadata |> select(variable_name, sum_cat), by = c("variable" = "variable_name")) |>
    mutate(summary = case_when(!is.na(summary) & !is.infinite(summary) & name != "per" ~ round(summary, 1) |> as.character(),
                               !is.na(summary) & !is.infinite(summary) & name %in% c("per" ,"per_comp") ~ round(summary, 0) |> as.character(),
                               TRUE ~ "")) |> 
    pivot_wider(names_from = name, values_from = summary) |>
    mutate(
      Range = paste0(min, "-", max),
      n_per = case_when(
        N == "0" ~ "",
        TRUE ~ paste0(n_pos, " (",  per, ")")),
      N_per = case_when(N == 0 ~ "0 (0)",
                        TRUE ~ paste0(N, " (",  per_comp, ")"))
    ) |> 
    select(-min, -max, -per, -n_pos, -per_comp, -N) |> 
    pivot_longer(cols = c(N_per, Mean, SD, Range, n_per), values_to = "summary") |>
    pivot_wider(names_from = study, values_from = summary) |> 
    filter(
      (sum_cat ==0 & name %in% c("N_per", "Mean", "SD", "Range")) | 
        (sum_cat ==1 & name %in% c("N_per", "n_per"))
    ) |> 
    rowwise() |>
    mutate(Variable =  get_label(variable, label_no = 3),
           name = case_when(name == "n_per" ~ "n (\\%)",
                            name == "N_per" ~ "N (\\% complete)", 
                            TRUE ~ name)) 

  if(!is.null(order)) {
    table <- table |> left_join(order, by = "variable") |> 
      arrange(order_no) |> 
      select(-order_no)
  }
  
  table <- table |> 
    select(-sum_cat, -variable) |> 
    relocate(Variable, .before = name) |> 
    rename(Overall = zOverall, Summary = name)
  

  
  return(table)
  
}

save_descriptive_hux_table <- function(descriptive_table, outcome_str, what, study_names = NULL, table_width = 1) {
  
  if(!is.null(study_names)){
    descriptive_table <- rbind(study_names, descriptive_table)
  } else {
    descriptive_table <- rbind(colnames(descriptive_table), descriptive_table)
  }

  variable_rows <- which(descriptive_table$Summary == "N (\\% complete)") 

  table_width <- 0.15+0.14+(ncol(descriptive_table)-2)*0.115 # Calcualting overall width - column by column width set below.
  
  hux_table <- descriptive_table |> 
    huxtable::hux(add_colnames = FALSE) |> 
    huxtable::set_bold(, row = 1) |>
    huxtable::set_bottom_border(row = 1, value = 0.5) |>
    huxtable::set_align(value = "centre", col = 3:ncol(descriptive_table)) |>     
    huxtable::set_width(value = table_width) |> 
    huxtable::merge_repeated_rows(col = 1) |> 
    huxtable::set_wrap(value = TRUE, col = 1) |> 
    huxtable::set_valign( col = 1, value = "middle") 
  
  for(myRow in variable_rows){
    hux_table <-  hux_table |> 
      huxtable::set_top_border(row = myRow, value = 0.5)
  }
  hux_table <- hux_table |> huxtable::set_col_width(1, 0.15/table_width)
  hux_table <- hux_table |> huxtable::set_col_width(2, 0.14/table_width)
  for(myCol in 3:ncol(descriptive_table)){
    hux_table <- hux_table |> huxtable::set_col_width(myCol, 0.115/table_width)
  }
  

  
  outcome_str_caps <- toupper(outcome_str)
  
  what_words <- c(
    base = glue::glue("the {outcome_str_caps} at Baseline"), 
    out = glue::glue("the {outcome_str_caps} at Outcome"), 
    pred = glue::glue("the baseline predictors used in the analysis of {outcome_str_caps}"))
  what_word <- what_words[what]
  hux_table |> save_hux_table(
    file_name = paste0(outcome_str,"_", what,  "_desc.tex"),
    caption = glue::glue("Descriptive statistics for {what_word}."),
    label = paste0(outcome_str, outcome_str,"_", what,  "_desc"))
  # 
    
}

