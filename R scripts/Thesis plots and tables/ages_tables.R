make_ages_table <- function(analysis_data_wide, analysis_data_long) {
  n_obs_data <- analysis_data_long |> 
    select(ID, n_obs)
  
  ages <- analysis_data_wide |> 
    mutate(study = "zOverall") |> 
    bind_rows(analysis_data_wide) |> 
    select(ID, study, base_age, out_age, fu_length) |> 
    left_join(n_obs_data) |> 
    pivot_longer(cols = c(base_age, out_age, fu_length, n_obs), names_to = "predictor", values_to = "value") |> 
    group_by(study, predictor) |>
    summarise(med = median(value),
              uq = quantile(value, 0.75),
              lq = quantile(value, 0.25)) |> 
    mutate(text = paste0(round(med, 1), " (", round(lq, 1), ", ", round(uq, 1), ")")) |> 
    select(-med, -uq, -lq) |> 
    ungroup() 
  
  ages_table <- ages |> pivot_wider(names_from = predictor, values_from = text) |> select(study, base_age, out_age, fu_length, n_obs) |> 
    mutate(study = case_when(study == "zOverall" ~ "Overall",
                             TRUE ~ study))
  
  return(ages_table)
  
}

save_ages_hux_table <- function(ages_table, outcome_str, htb = FALSE) {
  header_row <- c("Study", "Base age", "Outcome age", "Follow-up length", "Number of observations")
    ages_table <- rbind(header_row, ages_table)

    hux_table <- ages_table |> 
      huxtable::hux(add_colnames = FALSE) |> 
      huxtable::set_bold(, row = 1) |> 
      huxtable::set_bottom_border(row = 1, value = 0.5) |> 
      huxtable::set_align(value = "centre", col = 2:ncol(ages_table)) |> 
      huxtable::set_width(value = 1) |>
      huxtable::set_col_width(col = 1, value = 0.15) |>
      huxtable::set_wrap(value = TRUE, row = 1)
    
    outcome_str_caps <- toupper(outcome_str)
    hux_table |> save_hux_table(
      file_name = paste0(outcome_str, "_ages_table.tex"),
      caption = glue::glue("Ages at baseline and follow-up for the {outcome_str_caps}. Data shown as median (lower quartile, upper quartile)."),
      label = paste0(outcome_str, "_ages"),
      htb = htb)
}

# SDQ:
analysis_data_wide <- readRDS(here(derived_data, "pooled_sdq_wide.Rds"))|> 
  filter(base_all_complete, out_all_complete, autism != "post baseline")

analysis_data_long <- readRDS(here(derived_data, "pooled_sdq.Rds")) |> 
  filter(base_all_complete, out_all_complete, autism != "post baseline", wave == base_wave) 
  

sdq_ages_table <- make_ages_table(analysis_data_wide, analysis_data_long) |> 
  left_join(study_metadata |> select(study = name, label)) |> 
  select(-study) |> 
  select(study = label, everything())


save_ages_hux_table(sdq_ages_table, "sdq") 

# CBCL:
analysis_data_wide <- readRDS(here(derived_data, "pooled_cbcl_wide.Rds"))|> 
  filter(base_all_complete, out_all_complete)

analysis_data_long <- readRDS(here(derived_data, "pooled_cbcl.Rds")) |> 
  filter(base_all_complete, out_all_complete, wave == base_wave) 
  

cbcl_ages_table <- make_ages_table(analysis_data_wide, analysis_data_long) 
  

save_ages_hux_table(cbcl_ages_table, "cbcl")

# VABS:
analysis_data_wide <- readRDS(here(derived_data, "pooled_vabs_wide.Rds"))|> 
  filter(base_all_complete, out_all_complete)

analysis_data_long <- readRDS(here(derived_data, "pooled_vabs.Rds")) |> 
  filter(base_all_complete, out_all_complete, wave == base_wave) 
  

vabs_ages_table <- make_ages_table(analysis_data_wide, analysis_data_long)

save_ages_hux_table(vabs_ages_table, "vabs", htb = TRUE)

