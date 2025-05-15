data <- readRDS(here(derived_data, "pathways_vabs.Rds"))
data |> count(wave)
  
mod_data <- data |> select(timepoint = wave, ID, treat = base_sex, base_covariate = base_iq_full_scale, outcome = vabs_dls_ae) 
  

  
mod_data <-  mod_data |> 
  filter(timepoint != 1, timepoint != -3) |> 
  mutate(timepoint = timepoint + 7,
         timepoint = case_when(timepoint == 5 ~ 4,
                          timepoint == 7 ~ 5,
                          timepoint == 9 ~ 6,
                          TRUE ~ timepoint) )
mod_data |> count(timepoint)
baseline_data <- mod_data |> filter(timepoint == 0) |> select(ID, base_outcome = outcome, everything(), -timepoint)

analysis_data <- mod_data |> select(ID, outcome, timepoint) |>  filter(timepoint != 0) |> left_join(baseline_data, by = "ID") |> filter(!is.na(base_outcome))

analysis_data |> count(timepoint)

analysis_data |> haven::write_dta(here(derived_data, "sample_trial.dta"))

analysis_data3 <- analysis_data |> filter(timepoint <5)

model <- nlme::lme(
  outcome ~ timepoint * treat, 
  random = ~ 1 | ID, 
  weights = nlme::varIdent(form = ~ 1 | timepoint),
  data = analysis_data
)

summary(model)
