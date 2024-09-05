results_folder <- here::here(data_and_outputs, "Results", "SDQ", "Prelim")

outcome <- "sdq_peer_p"
model_full_name <- paste0("st_ri_study_", outcome, "_pred3_mi_average")

model_name <-  model_full_name
full_results <- readRDS(here::here(results_folder, "results_meta_analysis_long.rds")) 

my_full_results <- full_results |> 
  mutate(analysis_name = paste0(analysis_name, "_", intercept_est_method))|> 
  filter(analysis_name == model_full_name)

by_study <- my_full_results |>  filter(metric == "calib_slope") |> pull(by_study) 
by_study <- by_study[[1]] |>  filter(metric == "calib_slope")
by_study 

analysis_data_wide <- readRDS(here(derived_data, "pooled_sdq_wide.Rds")) |> filter(base_all_complete, out_all_complete)

study_char <- analysis_data_wide |> group_by(study) |> 
  summarise(
    ld = mean(base_ld, na.rm = TRUE), 
    fu_length = mean(fu_length),
    base_score = mean(base_sdq_peer_p, na.rm = TRUE),
    base_age = mean(base_age, na.rm = TRUE),
    out_age = mean(out_age, na.rm = TRUE)
  ) |> 
  mutate(year_of_birth = c(2006, 1991, 1997, 2001, 2002, 1991, 1995, 2004, 1999))


plot_data <- study_char |> left_join(by_study) |> 
  pivot_longer(
    cols = c(ld, fu_length, base_score, year_of_birth, base_age, out_age), 
    names_to = "var", 
    values_to = "value") |> 
  mutate(var = factor(var, 
         levels = c("ld", "fu_length", "base_score", "year_of_birth", "base_age", "out_age"),
         labels = c("Prop \n Interlectual \n Disability", "Follow-up \n Length", "Mean \n Baseline \n Score", "Year of \n Birth", "Age at \n baseline", "Age at \n outcome")))


plot_data |> 
  ggplot(aes(x = value, y = coef, color = study)) +
  geom_point(size = 2) + 
  ylab("Calibration Slope") + 
  ggrepel::geom_text_repel(aes(label = study), vjust = -0.5, hjust = 0.5) + 
  facet_wrap(~ var, scales = "free_x") +
  theme(legend.position = "none")

