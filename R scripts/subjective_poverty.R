gui_data |> select(autistic_any_wave) |> table()
wide <- gui_data |> filter(is.na(autistic_any_wave)) |> make_wide_dataset() |> filter(!is.na(subjective_poverty_1), !is.na(household_income_1))
wide <- wide |> mutate(poverty_bin = case_when(
  subjective_poverty_1 %in% c(1,2,3) ~ 1,
  subjective_poverty_1 %in% c(4,5,6) ~ 0,
  TRUE ~ NA
))

wide |> select(poverty_bin) |> table() 
lm("sdq_emot_p_2 ~ age_1 + sex_1 + sdq_emot_p_1 +sdq_hyp_p_1 + sdq_cond_p_1+ sdq_peer_p_1+ sdq_pro_p_1", data = wide) |> broom::tidy()
lm("sdq_emot_p_2 ~ age_1 + sex_1 + sdq_emot_p_1 +sdq_hyp_p_1 + sdq_cond_p_1+ sdq_peer_p_1+ sdq_pro_p_1 + household_income_1", data = wide) |> broom::tidy()
lm("sdq_emot_p_2 ~ age_1 + sex_1 + sdq_emot_p_1 +sdq_hyp_p_1 + sdq_cond_p_1+ sdq_peer_p_1+ sdq_pro_p_1 + subjective_poverty_1", data = wide) |> broom::tidy()
lm("sdq_emot_p_2 ~ age_1 + sex_1 + sdq_emot_p_1 +sdq_hyp_p_1 + sdq_cond_p_1+ sdq_peer_p_1+ sdq_pro_p_1 + poverty_bin", data = wide) |> broom::tidy()

lm("sdq_emot_p_2 ~ age_1 + sex_1 + sdq_emot_p_1 +sdq_hyp_p_1 + sdq_cond_p_1+ sdq_peer_p_1+ sdq_pro_p_1", data = wide) |> summary()
lm("sdq_emot_p_2 ~ age_1 + sex_1 + sdq_emot_p_1 +sdq_hyp_p_1 + sdq_cond_p_1+ sdq_peer_p_1+ sdq_pro_p_1 + household_income_1", data = wide) |> summary()
lm("sdq_emot_p_2 ~ age_1 + sex_1 + sdq_emot_p_1 +sdq_hyp_p_1 + sdq_cond_p_1+ sdq_peer_p_1+ sdq_pro_p_1 + subjective_poverty_1", data = wide) |> summary()
lm("sdq_emot_p_2 ~ age_1 + sex_1 + sdq_emot_p_1 +sdq_hyp_p_1 + sdq_cond_p_1+ sdq_peer_p_1+ sdq_pro_p_1 + poverty_bin", data = wide) |> summary()


wide_small <- wide |> dplyr::slice_sample(n = 600)

lm("sdq_emot_p_2 ~ age_1 + sex_1 + sdq_emot_p_1 +sdq_hyp_p_1 + sdq_cond_p_1+ sdq_peer_p_1+ sdq_pro_p_1", data = wide_small) |> summary()
lm("sdq_emot_p_2 ~ age_1 + sex_1 + sdq_emot_p_1 +sdq_hyp_p_1 + sdq_cond_p_1+ sdq_peer_p_1+ sdq_pro_p_1 + poverty_bin", data = wide_small) |> summary()

