means <- pi_data_vabs |> 
  mutate(round_base = round(base_spline1),
         outcome = get_label(outcome, label_no = 3)) |> 
  select(outcome, round_base, pred, ID, starts_with("pi_lower"), starts_with("pi_upper"))  |> 
  pivot_longer(cols = -c("ID", "round_base", "outcome"), names_to = "what", values_to = "value") |>
  group_by(round_base, what, outcome) |> summarise(mean = mean(value)) |>
  pivot_wider(names_from = what, values_from = mean) 

means |> filter(outcome == "ADHD")

ADHD14 <- pi_data_cbcl |> 
  mutate(round_base = round(base_spline1),
         outcome = get_label(outcome, label_no = 3)) |> 
  filter(outcome == "ADHD", round_base == 14) |> 
  select(ID, starts_with("pi_upper"))




