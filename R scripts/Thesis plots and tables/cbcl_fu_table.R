make_ages_table <- function(analysis_data_wide, analysis_data_long) {
  n_obs_data <- analysis_data_long |> 
    filter(base_all_complete, out_all_complete, wave == base_wave) |> 
    select(ID, n_obs)
  
  ages <- 
    analysis_data_wide |> 
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

save_ages_hux_table <- function(ages_table, outcome_str) {
  header_row <- c("Study", "Base age", "Outcome age", "Follow-up length", "Number of observations")
    ages_table <- rbind(header_row, ages_table)

    hux_table <- ages_table |> 
      hux(add_colnames = FALSE) |> 
      set_bold(, row = 1) |> 
      set_bottom_border(row = 1, value = 0.5) |> 
      set_align(value = "centre", col = 2:ncol(ages_table)) |> 
      set_width(value = 0.6) |> 
      set_wrap(value = TRUE, row = 1)
    
    outcome_str_caps <- toupper(outcome_str)
    hux_table |> save_hux_table(
      file_name = paste0(outcome_str, "_fu_table.tex"),
      caption = glue::glue("Ages at baseline and follo-up for the {outcome_str_caps}. Data shown as median (IQR)."),
      label = paste0(outcome_str, "_ages"))
}

# SDQ:
analysis_data_wide <- readRDS(here(derived_data, "pooled_sdq_wide.Rds"))|> 
  filter(base_all_complete, out_all_complete)

analysis_data_long <- readRDS(here(derived_data, "pooled_sdq.Rds"))

cbcl_ages_table <- make_ages_table(analysis_data_wide, analysis_data_long)

save_ages_hux_table(cbcl_ages_table, "sdq")

# CBCL:
analysis_data_wide <- readRDS(here(derived_data, "pooled_cbcl_wide.Rds"))|> 
  filter(base_all_complete, out_all_complete)

analysis_data_long <- readRDS(here(derived_data, "pooled_cbcl.Rds"))

cbcl_ages_table <- make_ages_table(analysis_data_wide, analysis_data_long)

save_ages_hux_table(cbcl_ages_table, "cbcl")

# VABS:
analysis_data_wide <- readRDS(here(derived_data, "pooled_vabs_wide.Rds"))|> 
  filter(base_all_complete, out_all_complete)

analysis_data_long <- readRDS(here(derived_data, "pooled_vabs.Rds"))

cbcl_ages_table <- make_ages_table(analysis_data_wide, analysis_data_long)

save_ages_hux_table(cbcl_ages_table, "vabs")


