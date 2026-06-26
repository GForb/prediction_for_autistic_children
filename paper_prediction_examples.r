# SDQ

pi_data_sdq <- readRDS(here(derived_data, "pi_data_sdq.Rds"))

sdq_data <- readRDS(here(derived_data, "pooled_sdq_wide.Rds")) |> filter(base_all_complete, out_all_complete, autism != "post baseline")

# prediction of hyperactivity outcome



sdq_data |> filter(base_sdq_hyp_p == 8, base_sdq_cond_p ==2, base_sex == 0) |> 
  select(ID, out_age, out_sdq_hyp_p, base_age, base_sex, base_sdq_hyp_p, base_sdq_cond_p) |>
  print(n = 100) 


pi_data_sdq |> filter(outcome == "sdq_hyp_p", ID == "lsac_k_53302613") |> 
  select(ID, actual, pred, outcome, starts_with("pi_lower"), starts_with("pi_upper"))


# Predicted scores the 95% prediction interval is from 2.4 to 9.7

pi_data_vabs <- readRDS(here(derived_data, "pi_data_vabs.Rds"))

vabs_data <- readRDS(here(derived_data, "pooled_vabs_wide.Rds")) 

vabs_data |> 
  select(ID, out_age,base_age, starts_with("base_vabs"), base_sex) |>
  filter(base_age < 11, out_age > 14, base_vabs_dls_ae > 7 & base_vabs_dls_ae <8, base_sex == 1) |> 
  print(n = 100) 

pi_data_vabs |> filter(ID == "Pathways_127") |> 
  select(ID, outcome, actual, pred, outcome, starts_with("pi_lower"), starts_with("pi_upper"))


# prediction of hyperactivity outcome



analysis_data_wide |> filter(base_sdq_hyp_p == 8, base_sdq_cond_p ==2, base_sex == 0) |> 
  select(ID, out_age, out_sdq_hyp_p, base_age, base_sex, base_sdq_hyp_p, base_sdq_cond_p) |>
  print(n = 100) 


pi_data_sdq |> filter(outcome == "sdq_hyp_p", ID == "SNAP_788") |> 
  select(ID, actual, pred, outcome, starts_with("pi_lower"), starts_with("pi_upper"))



# CBCL
pi_data_cbcl <- readRDS(here(derived_data, "pi_data_cbcl.Rds"))

cbcl_data <- readRDS(here(derived_data, "pooled_cbcl_wide.Rds")) 

cbcl_data |> filter(base_cbcl_adhd == 6, base_cbcl_con <6 , base_sex == 0) |> 
  select(ID, out_age, out_cbcl_adhd, base_age, base_sex, base_cbcl_adhd, base_cbcl_con) |>
  print(n = 100) 



pi_data_cbcl |> filter(outcome == "cbcl_adhd", ID == "TRAILS_20489") |> 
  select(ID, actual, pred, outcome, starts_with("pi_lower"), starts_with("pi_upper"))

