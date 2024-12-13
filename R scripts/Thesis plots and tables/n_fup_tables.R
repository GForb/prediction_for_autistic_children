

# SDQ
accounting_data <- readRDS(here(derived_data, "pooled_sdq_acc.Rds")) |>
  filter(autism != "post baseline")
analysis_data_wide <- readRDS(here(derived_data, "pooled_sdq_wide.Rds")) |>
  filter(autism != "post baseline")

analysis_data_wide <- analysis_data_wide |> mutate(
  out_partially_complete = case_when(
    out_all_complete == TRUE ~ FALSE,!out_all_complete &
      (
        !is.na(out_sdq_pro_p) |
          !is.na(out_sdq_emot_p) |
          !is.na(out_sdq_hyp_p) |
          !is.na(out_sdq_cond_p) |
          !is.na(out_sdq_peer_p)
      ) ~ TRUE,
    TRUE ~ FALSE
  )
)


by_study_acc <- get_by_study_acc(accounting_data, analysis_data_wide) 
sdq_table <- make_n_fup_table(by_study_acc) |> 
  left_join(study_metadata |> select(study = name, label)) |> 
  select(-study) |> 
  select(study = label, everything())

sdq_table |> save_n_fup_hux_table(outcome_str = "sdq")

# CBCL
accounting_data <- readRDS(here(derived_data, "pooled_cbcl_acc.Rds"))
analysis_data_wide <- readRDS(here(derived_data, "pooled_cbcl_wide.Rds"))

analysis_data_wide <- analysis_data_wide |> mutate(
  out_partially_complete = case_when(
    out_all_complete == TRUE ~ FALSE,!out_all_complete &
      (
        !is.na(out_cbcl_aff) |
          !is.na(out_cbcl_anx) |
          !is.na(out_cbcl_som) |
          !is.na(out_cbcl_adhd) |
          !is.na(out_cbcl_odd) |
          !is.na(out_cbcl_con)
      ) ~ TRUE,
    TRUE ~ FALSE
  )
)
by_study_acc <- get_by_study_acc(accounting_data, analysis_data_wide) 
cbcl_table <- make_n_fup_table(by_study_acc)
cbcl_table |> save_n_fup_hux_table(outcome_str = "cbcl")


# VABS
accounting_data <- readRDS(here(derived_data, "pooled_vabs_acc.Rds"))
analysis_data_wide <- readRDS(here(derived_data, "pooled_vabs_wide.Rds"))

analysis_data_wide <- analysis_data_wide |> mutate(
  out_partially_complete = case_when(
    out_all_complete == TRUE ~ FALSE,!out_all_complete &
      (
        !is.na(out_vabs_dls_ae) |
          !is.na(out_vabs_com_ae) |
          !is.na(out_vabs_soc_ae)
      ) ~ TRUE,
    TRUE ~ FALSE
  )
)
by_study_acc <- get_by_study_acc(accounting_data, analysis_data_wide) |> 
  mutate(eligble_baseline = case_when(study == "EpiTED" | study == "zOverall" ~ eligble_baseline +67,
                                      TRUE ~ eligble_baseline),
         loss_to_follow_up = case_when(study == "EpiTED"| study == "zOverall"  ~ loss_to_follow_up +67,
                                      TRUE ~ loss_to_follow_up))
vabs_table <- make_n_fup_table(by_study_acc)
vabs_table |> save_n_fup_hux_table(outcome_str = "vabs")
