pi_data_vabs <- readRDS(here(derived_data, "pi_data_vabs.Rds"))

pi_data_vabs |> filter(outcome == "vabs_dls_ae") |> 
  make_pi_plot() +
  theme_bw(base_size = 24)  +
  theme(legend.position = "top") +
  labs(y = "Predicted Age Equivalent Score at 14",
       x = "Age equivalent at age 10/11")

pi_data_sdq <- readRDS(here(derived_data, "pi_data_sdq.Rds"))

pi_data_sdq |> filter(outcome == "sdq_emot_p", cutoffs = TRUE) |> 
  make_pi_plot() +
  theme_bw(base_size = 24)  +
  theme(legend.position = "top") +
  labs(y = "Predicted Age Equivalent Score at 14.5",
       x = "Age equivalent at age 11")



