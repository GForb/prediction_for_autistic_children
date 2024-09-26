make_n_fup_table <- function(accounting_data, analysis_data_wide) {
  accounting_data_all <- accounting_data |>
    left_join(
      analysis_data_wide |> select(
        ID,
        base_all_complete,
        out_all_complete,
        out_partially_complete
      ),
      by = "ID"
    )
  
  by_study_accounting <- accounting_data_all |>
    mutate(study = "zOverall") |>
    bind_rows(accounting_data_all) |>
    filter(base_in_range == 1 &
             (base_all_complete == 1 | is.na(base_all_complete))) |>
    group_by(study) |>
    summarise(
      eligble_baseline = n(),
      included = sum(include == "include" & out_all_complete),
      not_included = sum(include != "include" |
                           out_all_complete != 1),
      loss_to_follow_up = sum(
        out_recorded == 0 |
          (out_all_complete == FALSE &
             out_partially_complete == FALSE),
        na.rm = TRUE
      ),
      outcome_out_of_range = sum(
        include == "eligible baseline, no followup in range" |
          include == "ineligible follow up length"
      ),
      partially_completed_outcome = sum(out_partially_complete == TRUE, na.rm = TRUE)
    ) |>
    mutate(
      check1 = eligble_baseline - included - not_included,
      check2 =  not_included - loss_to_follow_up - outcome_out_of_range - partially_completed_outcome
    )
  
  by_study_accounting
  
  
  
  table_data <- by_study_accounting  |> mutate(no_fu = loss_to_follow_up + partially_completed_outcome) |>
    select(-partially_completed_outcome,
           -loss_to_follow_up,
           -check1,
           -check2) |>
    pivot_longer(
      cols = c(included, not_included, outcome_out_of_range, no_fu),
      names_to = "status",
      values_to = "n"
    ) |>
    mutate(prop = n / eligble_baseline,
           text = paste0(n, " (", round(prop * 100, 0), "\\%)")) |>
    select(-n, -prop) |>
    pivot_wider(names_from = status, values_from = text) |>
    select(-not_included)  |>
    mutate(study = case_when(study == "zOverall" ~ "Overall", TRUE ~ study))
  
  return(table_data)
  
}

save_n_fup_hux_table <- function(n_fup_table, outcome_str) {
  header_row <- c(
    "Study",
    "Eligible at baseline",
    "Included in analysis",
    "Outcome out of range",
    "No follow-up data"
  )
  n_fup_table <- rbind(header_row, n_fup_table)
  
  hux_table <- n_fup_table |>
    huxtable::hux(add_colnames = FALSE) |>
    huxtable::set_bold(, row = 1) |>
    huxtable::set_bottom_border(row = 1, value = 0.5) |>
    huxtable::set_align(value = "centre", col = 2:ncol(n_fup_table)) |>
    huxtable::set_width(value = 0.9) |>
    huxtable::set_wrap(value = TRUE, row = 1)
  
  outcome_str_caps <- toupper(outcome_str)
  hux_table |> save_hux_table(
    file_name = paste0(outcome_str, "_n_fup_table.tex"),
    caption = glue::glue(
      "Number of people with eligible baseline data, numbers included in analysis, and reason for exlusion for the {outcome_str_caps}."
    ),
    label = paste0(outcome_str, "_n_fup")
  )
}

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



sdq_table <- make_n_fup_table(accounting_data, analysis_data_wide) |> 
  left_join(study_labels |> select(study = name, label)) |> 
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

cbcl_table <- make_n_fup_table(accounting_data, analysis_data_wide)
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

vabs_table <- make_n_fup_table(accounting_data, analysis_data_wide)
vabs_table |> save_n_fup_hux_table(outcome_str = "vabs")
