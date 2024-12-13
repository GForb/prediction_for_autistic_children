create_descriptive_table <- function(data,
                                     order = NULL,
                                     study_names = NULL) {
  table <- data |> mutate(study = "zOverall") |>
    bind_rows(data) |>
    pivot_longer(
      cols = -c(ID, study),
      names_to = "variable",
      values_to = "score"
    ) |>
    group_by(study, variable) |>
    summarise(
      total_n = n(),
      N = sum(!is.na(score)),
      Mean = mean(score, na.rm = TRUE),
      SD = sd(score, na.rm = TRUE),
      min = min(score, na.rm = TRUE),
      max = max(score, na.rm = TRUE),
      n_pos = sum(score == 1, na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(per = n_pos / N * 100, per_comp = N / total_n * 100) |>
    select(-total_n) |>
    pivot_longer(values_to = "summary",
                 cols = c(N, Mean, SD, min, max, n_pos, per, per_comp)) |>
    left_join(var_metadata |> select(variable_name, sum_cat),
              by = c("variable" = "variable_name")) |>
    mutate(
      summary = case_when(
        !is.na(summary) &
          !is.infinite(summary) &
          name != "per" ~ round(summary, 1) |> as.character(),!is.na(summary) &
          !is.infinite(summary) &
          name %in% c("per" , "per_comp") ~ round(summary, 0) |> as.character(),
        TRUE ~ ""
      )
    ) |>
    pivot_wider(names_from = name, values_from = summary) |>
    mutate(
      Range = paste0(min, "-", max),
      n_per = case_when(N == "0" ~ "", TRUE ~ paste0(n_pos, " (", per, ")")),
      N_per = case_when(N == 0 ~ "0 (0)", TRUE ~ paste0(N, " (", per_comp, ")"))
    ) |>
    select(-min, -max, -per, -n_pos, -N_per) |>
    pivot_longer(cols = c(N, per_comp, Mean, SD, Range, n_per),
                 values_to = "summary") |>
    pivot_wider(names_from = study, values_from = summary) |>
    filter((sum_cat == 0 &
              name %in% c("N", "per_comp", "Mean", "SD", "Range")) |
             (sum_cat == 1 & name %in% c("N", "per_comp", "n_per"))) |>
    rowwise() |>
    mutate(
      Variable =  get_label(variable, label_no = 3),
      name = case_when(
        name == "n_per" ~ "n (\\%)",
        name == "per_comp" ~ "\\% data",
        TRUE ~ name
      )
    )
  
  if (!is.null(order)) {
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

create_descriptive_table_outcome <- function(data,
                                             order = NULL,
                                             study_names = NULL,
                                             cutoffs = NULL) {
  long_data <- data |> mutate(study = "zOverall") |>
    bind_rows(data) |>
    pivot_longer(
      cols = -c(ID, study),
      names_to = "variable",
      values_to = "score"
    )
  
  
  
  summaries <- long_data |>
    group_by(study, variable) |>
    summarise(
      Mean = mean(score, na.rm = TRUE),
      SD = sd(score, na.rm = TRUE),
      min = min(score, na.rm = TRUE),
      max = max(score, na.rm = TRUE)
    ) |>
    ungroup() |>
    pivot_longer(values_to = "summary", cols = c(Mean, SD, min, max)) |>
    left_join(var_metadata |> select(variable_name, sum_cat),
              by = c("variable" = "variable_name")) |>
    mutate(summary = round(summary, 1) |> as.character()) |>
    pivot_wider(names_from = name, values_from = summary) |>
    mutate(Range = paste0(min, "-", max)) |>
    select(-min, -max)
  
  
  table <-  summaries |>
    pivot_longer(cols = c(Mean, SD, Range), values_to = "summary") |>
    pivot_wider(names_from = study, values_from = summary) |>
    rowwise() |>
    mutate(Variable =  get_label(variable, label_no = 3))
  
  if (!is.null(cutoffs)) {
    cut_summaries <- long_data |>
      left_join(cutoffs) |>
      group_by(study, variable) |>
      summarise(
        cutoff = mean(cutoff),
        prop_over_cutoff =  sum(score > cutoff, na.rm = TRUE) / sum(!is.na(score)),
        prop_under_cutoff = sum(score < cutoff, na.rm = TRUE) /
          sum(!is.na(score))
      ) |>
      mutate(
        cutoff_prop = case_when(
          variable == "sdq_pro_p" ~ prop_under_cutoff,
          TRUE ~ prop_over_cutoff
        ),
        cutoff_per = case_when(
          !is.na(cutoff) ~ round(cutoff_prop * 100, 0) |> as.character() |> paste0("\\%"),
          is.na(cutoff)  ~ ""
        )
      ) |>
      select(study, variable, summary = cutoff_per) |>
      pivot_wider(names_from = study, values_from = summary) |>
      rowwise() |>
      mutate(name = "Likely disorder",
             Variable =  get_label(variable, label_no = 3)) |>
      ungroup()
    
    table <-  bind_rows(table, cut_summaries)
    
  }
  
  if (!is.null(cutoffs$n_items)) {
    item_means <- long_data |>
      left_join(cutoffs) |>
      group_by(study, variable) |>
      summarise(mean = mean(score, na.rm = TRUE),
                n_items = mean(n_items)) |>
      ungroup() |>
      mutate(summary = round(mean / n_items, 1) |> as.character())  |>
      select(study, variable, summary) |>
      pivot_wider(names_from = study, values_from = summary) |>
      rowwise() |>
      mutate(name = "Item mean",
             Variable =  get_label(variable, label_no = 3)) |>
      ungroup()
    
    table <-  bind_rows(table, item_means)
    
  }
  
  table <-  table |> arrange(Variable)
  
  if (!is.null(order)) {
    table <- table |> left_join(order, by = "variable") |>
      arrange(order_no) |>
      select(-order_no)
  }
  
  table <- table |>
    select(-variable, -sum_cat) |>
    relocate(Variable, .before = name) |>
    rename(Overall = zOverall, Summary = name)
  
  return(table)
  
}


save_descriptive_hux_table <- function(descriptive_table,
                                       outcome_str,
                                       what,
                                       study_names = NULL,
                                       table_width = 1,
                                       outcome_table = FALSE,
                                       col1_width = 0.15,
                                       col2_width = NULL,
                                       htb = FALSE) {
  if (!is.null(study_names)) {
    descriptive_table <- rbind(study_names, descriptive_table)
  } else {
    descriptive_table <- rbind(colnames(descriptive_table), descriptive_table)
  }
  if (outcome_table) {
    variable_rows <- which(descriptive_table$Summary == "Mean")
  } else {
    variable_rows <- which(descriptive_table$Summary == "N")
  }
  
  
  col1_width = table_width * col1_width
  rest_of_col_width = table_width * (1 - col1_width) / (ncol(descriptive_table) -
                                                          1)
  if(!is.null(col2_width)){
    rest_of_col_width = table_width * (1 - col1_width - col2_width) / (ncol(descriptive_table) -
                                                            2)
  } else {
    col2_width <- rest_of_col_width
  }
  
  hux_table <- descriptive_table |>
    huxtable::hux(add_colnames = FALSE) |>
    huxtable::set_bold(, row = 1) |>
    huxtable::set_bottom_border(row = 1, value = 0.5) |>
    huxtable::set_align(value = "centre", col = 3:ncol(descriptive_table)) |>
    huxtable::set_width(value = table_width) |>
    huxtable::merge_repeated_rows(col = 1) |>
    huxtable::set_wrap(value = TRUE, col = 1) |>
    huxtable::set_valign(col = 1, value = "middle")
  
  for (myRow in variable_rows) {
    hux_table <-  hux_table |>
      huxtable::set_top_border(row = myRow, value = 0.5)
  }
  hux_table <- hux_table |> huxtable::set_col_width(1, col1_width)
  hux_table <- hux_table |> huxtable::set_col_width(2, col2_width)
  for (myCol in 3:ncol(descriptive_table)) {
    hux_table <- hux_table |> huxtable::set_col_width(myCol, rest_of_col_width)
  }
  
  
  
  outcome_str_caps <- toupper(outcome_str)
  
  what_words <- c(
    base = glue::glue("the {outcome_str_caps} at Baseline"),
    out = glue::glue("the {outcome_str_caps} at Outcome"),
    pred = glue::glue(
      "the baseline predictors used in the analysis of {outcome_str_caps}"
    )
  )
  what_word <- what_words[what]
  hux_table |> save_hux_table(
    file_name = paste0(outcome_str, "_", what, "_desc.tex"),
    caption = glue::glue("Descriptive statistics for {what_word}."),
    label = paste0(outcome_str, "_", what, "_desc"),
    htb = htb
  )
  #
  
}
