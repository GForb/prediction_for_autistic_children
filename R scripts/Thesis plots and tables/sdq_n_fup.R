accounting_data <- readRDS(here(derived_data, "pooled_sdq_acc.Rds"))
analysis_data_wide <- readRDS(here(derived_data, "pooled_sdq_wide.Rds"))

analysis_data_wide <- analysis_data_wide |> mutate(out_partially_complete = case_when(
  out_all_complete == TRUE ~ FALSE,
  !out_all_complete & (!is.na(out_sdq_pro_p) | 
                       !is.na(out_sdq_emot_p) | 
                       !is.na(out_sdq_hyp_p) | 
                       !is.na(out_sdq_cond_p) | 
                       !is.na(out_sdq_peer_p)) ~ TRUE,
  TRUE ~ FALSE))

accounting_data_all <- accounting_data |> 
  left_join(analysis_data_wide |> select(ID, base_all_complete, out_all_complete, out_partially_complete), by = "ID") |> 
  filter(autism != "post baseline")

by_study_accounting <- accounting_data_all |> 
  mutate(study = "zOverall") |> 
  bind_rows(accounting_data_all) |> 
  filter(base_in_range ==1 & (base_all_complete ==1 | is.na(base_all_complete))) |>
  group_by(study) |> 
  summarise(
    eligble_baseline = n(),
    included = sum(include == "include" & out_all_complete),
    not_included = sum(include != "include" | out_all_complete !=1),
    loss_to_follow_up = sum(out_recorded ==0 | (out_all_complete == FALSE & out_partially_complete == FALSE), na.rm = TRUE),
    outcome_out_of_range = sum(include =="eligible baseline, no followup in range" | include =="ineligible follow up length"),
    partially_completed_outcome = sum(out_partially_complete == TRUE, na.rm = TRUE)
  ) |> 
  mutate(check1 = eligble_baseline - included - not_included,
         check2 =  not_included - loss_to_follow_up - outcome_out_of_range - partially_completed_outcome) 

by_study_accounting



table_data <- by_study_accounting  |> mutate(no_fu_sdq = loss_to_follow_up + partially_completed_outcome) |> 
  select(-partially_completed_outcome,-loss_to_follow_up, -check1, -check2) |> 
  pivot_longer(cols = c(included, not_included, outcome_out_of_range, no_fu_sdq), names_to = "status", values_to = "n") |> 
  mutate(prop = n/eligble_baseline,
         text = paste0(n, " (", round(prop*100, 0), "\\%)")) |> 
  select(-n, -prop) |> 
  pivot_wider(names_from = status, values_from = text) |> 
  select(-not_included)

write_csv(table_data, file.path(thesis_tables, "sdq_fu_status.csv"))
  
 