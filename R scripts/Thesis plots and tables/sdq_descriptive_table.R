analysis_data <- readRDS(here(derived_data, "pooled_sdq.Rds")) |>
  filter(base_all_complete, out_all_complete, autism != "post baseline")

analysis_data_base <- analysis_data |> filter(wave == base_wave)
analysis_data_out <- analysis_data |> filter(wave == out_wave)




sdq_cutoffs <- tibble(
  domain = c(
    "sdq_cond_p",
    "sdq_emot_p",
    "sdq_hyp_p",
    "sdq_peer_p",
    "sdq_pro_p"
  ),
  cutoff = c(4, 5, 8, 4, 6)
)

table_data_base <- analysis_data_base |> mutate(study = "zOverall") |>
  bind_rows(analysis_data_base) |>
  select(
    ID,
    study,
    starts_with("base"),-base_all_complete,
    -base_iq_full_scale,
    -base_iq_full_scale_w1,-base_iq_standard,
    -base_ados_css_rrb,
    -base_ados_css_sa,
    -base_hearing_impairment,
    -base_visual_impairment,
    -base_wave,
    -base_vabs_abc_ss,
    -base_ld_w1
  ) |>
  mutate(a_n = 1) |>
  pivot_longer(cols = -c(ID, study),
               names_to = "domain",
               values_to = "Score") |>
  group_by(study, domain) |>
  summarise(
    n = sum(!is.na(Score)),
    mean = mean(Score, na.rm = TRUE),
    sd = sd(Score, na.rm = TRUE),
    min = min(Score, na.rm = TRUE),
    max = max(Score, na.rm = TRUE)
  ) |>
  ungroup() |>
  pivot_longer(values_to = "summary", cols = c(n, mean, sd, min, max)) |>
  mutate(summary = case_when(
    !is.na(summary) &
      !is.infinite(summary) ~ round(summary, 1) |> as.character(),
    TRUE ~ ""
  )) |>
  pivot_wider(names_from = name, values_from = summary) |>
  mutate(range = paste0(min, "-", max)) |>
  select(-min, -max) |>
  pivot_longer(cols = c(n, mean, sd, range), values_to = "summary") |>
  pivot_wider(names_from = study, values_from = summary)

base_sdq_table

base_sdq_table <- analysis_data_base |> select(ID, study, starts_with("sdq")) |>  create_descriptive_table()
out_sdq_table <- analysis_data_out |> select(ID, study, starts_with("sdq")) |>  create_descriptive_table()

order_n = tibble(
  variable = c(
    "base_sex",
    "base_ld",
    "base_ethnicty",
    "base_maternal_education",
    "base_maternal_mh",
    "base_imd_decile"
  ),
  order_no = 1:6
)
base_predictors_table <- analysis_data_base |>
  select(
    ID,
    study,
    base_sex,
    base_ld,
    base_ethnicity,
    base_maternal_education,
    base_maternal_mh,
    base_imd_decile
  ) |>
  create_descriptive_table(order = order_n)

sdq_study_names <- study_labels$label[match(colnames(base_predictors_table), study_labels$name)]

base_sdq_table |> save_descriptive_hux_table(outcome_str = "sdq",
                                             what = "base",
                                             study_names = sdq_study_names)
base_predictors_table |> save_descriptive_hux_table(outcome_str = "sdq",
                                                    what = "pred",
                                                    study_names = sdq_study_names)
out_sdq_table |> save_descriptive_hux_table(outcome_str = "sdq",
                                            what = "out",
                                            study_names = sdq_study_names) 




#write_csv(base_sdq_table, file.path(thesis_tables, "sdq_descriptive_table_base.csv"))
# write_csv(base_predictors_table, file.path(thesis_tables, "sdq_descriptive_table_predictors.csv"))
# write_csv(out_sdq_table, file.path(thesis_tables, "sdq_descriptive_table_outcomes.csv"))
