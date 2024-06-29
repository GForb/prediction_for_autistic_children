data <- readRDS(here(derived_data , "pooled_sdq_wide.Rds")) |> 
  filter(autism != "post baseline") |> 
  mutate(base_sdq_total = base_sdq_emot_p + base_sdq_cond_p + base_sdq_hyp_p + base_sdq_peer_p)


ggplot(data, aes(x = study, y = base_sdq_total, color = study)) +
  geom_boxplot() +
  labs(title = "SDQ Total problems at baseline",
       x = "Study",
       y = "Base SDQ Total") +
  theme_minimal()+
  theme(legend.position = "none")



ggplot(data, aes(x = study, y = base_age, color = study)) +
  geom_boxplot() +
  labs(title = "Age at baseline",
       x = "Study",
       y = "Base SDQ Total") +
  theme_minimal()+
  theme(legend.position = "none")



ggplot(data, aes(x = study, y = fu_length, color = study)) +
  geom_boxplot() +
  labs(title = "Length of follow-ip",
       x = "Study",
       y = "Base SDQ Total") +
  theme_minimal()+
  theme(legend.position = "none")