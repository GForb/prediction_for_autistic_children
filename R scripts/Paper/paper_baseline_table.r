bl_cbcl <- readRDS(here(derived_data, "pooled_cbcl.Rds"))|> 
  filter(base_all_complete, out_all_complete) |> 
  filter(wave == out_wave) |> 
  mutate(outcome = "cbcl")

bl_sdq <- readRDS(here(derived_data, "pooled_sdq.Rds"))|> 
  filter(base_all_complete, out_all_complete) |> 
  filter(wave == out_wave, autism != "post baseline") |> 
  mutate(outcome = "sdq") |> 
  select(-autism)



bl_vabs <- readRDS(here(derived_data, "pooled_vabs.Rds"))|> 
  filter(base_all_complete, out_all_complete) |> 
  filter(wave == out_wave) |> 
  mutate(outcome = "vabs")

order_n = tibble(
  variable = c("base_age","age", "fu_length", "base_sex", 
               "base_iq_full_scale", "base_ld",
               "base_ados_css_rrb", "base_ados_css_sa", "base_vabs_abc_ss",
               "base_adi_65", "base_maternal_education","base_maternal_mh", "base_ethnicity", "base_imd_decile",
               "base_cbcl_aff", "base_cbcl_anx", "base_cbcl_adhd", "base_cbcl_con", "base_cbcl_odd", "base_cbcl_som",
               "base_sdq_emot_p", "base_sdq_hyp_p", "base_sdq_cond_p", "base_sdq_peer_p", "base_sdq_pro_p",
               "base_vabs_com_ae", "base_vabs_dls_ae", "base_vabs_soc_ae"), 
  order_no = 1:28)


bl_data <- bind_rows(bl_cbcl, bl_vabs, bl_sdq) |> 
  select(ID,
    outcome,
    any_of(order_n$variable)
  )


bl_data_long <- bl_data |> 
  pivot_longer(!c(outcome, ID), names_to = "variable", values_to = "value") 

# to do: add catagorical summaries
bl_summ_long <- bl_data_long |> group_by(outcome, variable) |> 
  summarise(n = sum(!is.na(value)),  N = n(), 
            mean = mean(value, na.rm = TRUE), 
            sd = sd(value, na.rm = TRUE),
            n_1 = sum(value == 1, na.rm = TRUE)
            ) |> 
  mutate(
    per = round(n/N*100),
    n_1_per = round(n_1/n*100),
    n_per_recorded = case_when(n == 0 ~ "Not available",
                      n != 0 ~ glue::glue("{n} ({per}%)")),
    mean_sd = case_when(n == 0 ~ "",
                        n != 0 ~ glue::glue("{round(mean, 1)} ({round(sd, 1)})")),
    n_per = case_when(n == 0 ~ "",
                      n != 0 ~ glue::glue("{n_1} ({n_1_per}%)")),
    summary = case_when(variable %in% c("base_sex", "base_learning_disability", "base_ld", "base_ethnicity" ) ~ n_per, 
                        TRUE ~ mean_sd)
    )

bl_table <- bl_summ_long |> 
  select(outcome, variable, n_per_recorded, summary) |> 
  pivot_wider(names_from = outcome, values_from = c(n_per_recorded, summary) ) |> 
  mutate(label = get_label(variable, label_no = 2)) |> 
  left_join(order_n, by = "variable") |> 
  arrange(order_no) |> 
  select(label, ends_with("cbcl"), ends_with("sdq"), ends_with("vabs")) |> 
  print(n = 100)


write_csv(bl_table, file = here(outputs, "Paper", "bl_table.csv"))  