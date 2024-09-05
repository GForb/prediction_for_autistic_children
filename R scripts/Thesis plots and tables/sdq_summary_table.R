analysis_data <- readRDS(here(derived_data, "pooled_sdq.Rds"))|> 
  filter(base_all_complete, out_all_complete, autism != "post baseline") 

analysis_data_base <- analysis_data |> filter(wave == base_wave)
analysis_data_out <- analysis_data |> filter(wave == out_wave)




sdq_cutoffs <- tibble(domain = c("sdq_cond_p", "sdq_emot_p", "sdq_hyp_p", "sdq_peer_p", "sdq_pro_p"), 
                      cutoff = c(4, 5, 8, 4, 6))

table_data_base <- analysis_data_base |> mutate(study = "zOverall") |> 
  bind_rows(analysis_data_base) |> 
  select(
    ID, study, starts_with("base"), 
    -base_all_complete, -base_iq_full_scale, -base_iq_full_scale_w1, 
    -base_iq_standard, -base_ados_css_rrb, -base_ados_css_sa, - base_hearing_impairment, -base_visual_impairment, -base_wave, - base_vabs_abc_ss, -base_ld_w1) |> 
  mutate(a_n = 1) |> 
  pivot_longer(cols = -c(ID, study), names_to = "domain", values_to = "Score") |> 
  group_by(study, domain) |> 
  summarise(n = sum(!is.na(Score)),
         mean = mean(Score, na.rm = TRUE),
         sd = sd(Score, na.rm = TRUE),
         min = min(Score, na.rm = TRUE),
         max = max(Score, na.rm = TRUE)) |> 
  ungroup() |> 
  pivot_longer(values_to = "summary", cols = c(n, mean, sd, min, max)) |>
  mutate(summary = case_when(!is.na(summary) & !is.infinite(summary) ~ round(summary, 1) |> as.character(),
                             TRUE ~ "")) |> 
  pivot_wider(names_from = name, values_from = summary) |>
  mutate(range = paste0(min, "-", max)) |> 
  select(-min, -max) |> 
  pivot_longer(cols = c(n, mean, sd, range), values_to = "summary") |>
  pivot_wider(names_from = study, values_from = summary)

create_descriptive_table <- function(data, order = NULL) {
  table <- data |> mutate(study = "zOverall") |> 
    bind_rows(data) |> 
    pivot_longer(cols = -c(ID, study), names_to = "variable", values_to = "score") |>  
    group_by(study, variable) |> 
    summarise(N = sum(!is.na(score)),
              Mean = mean(score, na.rm = TRUE),
              SD = sd(score, na.rm = TRUE),
              min = min(score, na.rm = TRUE),
              max = max(score, na.rm = TRUE),
              n_pos = sum(score ==1, na.rm = TRUE)) |> 
    ungroup() |> 
    mutate(per = n_pos/N*100) |> 
    pivot_longer(values_to = "summary", cols = c(N, Mean, SD, min, max, n_pos, per)) |>
    left_join(var_metadata |> select(variable_name, sum_cat), by = c("variable" = "variable_name")) |>
    mutate(summary = case_when(!is.na(summary) & !is.infinite(summary) & name != "per" ~ round(summary, 1) |> as.character(),
                               !is.na(summary) & !is.infinite(summary) & name == "per" ~ round(summary, 0) |> as.character(),
                               TRUE ~ "")) |> 
    pivot_wider(names_from = name, values_from = summary) |>
    mutate(
      Range = paste0(min, "-", max),
      n_per = case_when(
        N == "0" ~ "",
        TRUE ~ paste0(n_pos, " (",  per, ")")
      )
    ) |> 
    select(-min, -max, -per, -n_pos) |> 
    pivot_longer(cols = c(N, Mean, SD, Range, n_per), values_to = "summary") |>
    pivot_wider(names_from = study, values_from = summary) |> 
    rowwise() |>
    mutate(Variable = case_when(name == "N" ~ get_label(variable),
                               TRUE ~ ""),
           name = case_when(name == "n_per" ~ "n (%)",
                            TRUE ~ name)) |> 
    filter(
      (sum_cat ==0 & name %in% c("N", "Mean", "SD", "Range")) | 
      (sum_cat ==1 & name %in% c("N", "n (%)"))
      ) 
  
  if(!is.null(order)) {
    table <- table |> left_join(order, by = "variable") |> 
      arrange(order_no) |> 
      select(-order_no)
  }
  
  table |> 
    select(-sum_cat, -variable) |> 
    relocate(Variable, .before = name) |> 
    rename(Overall = zOverall)
    
}

base_sdq_table <- analysis_data_base |> select(ID, study, starts_with("sdq")) |>  create_descriptive_table()
out_sdq_table <- analysis_data_out |> select(ID, study, starts_with("sdq")) |>  create_descriptive_table()

order_n = tibble(variable = c("base_sex", "base_ld", "base_ethnicty", "base_maternal_education", "base_maternal_mh", "base_imd_decile"), 
                 order_no = 1:6)
base_predictors_table <- analysis_data_base |> 
  select(ID, study, base_sex, base_ld, base_ethnicity, base_maternal_education, base_maternal_mh, base_imd_decile) |>  
  create_descriptive_table(order = order_n) 


write_csv(base_sdq_table, file.path(thesis_tables, "sdq_descriptive_table_base.csv"))
write_csv(base_predictors_table, file.path(thesis_tables, "sdq_descriptive_table_predictors.csv"))
write_csv(out_sdq_table, file.path(thesis_tables, "sdq_descriptive_table_outcomes.csv"))
