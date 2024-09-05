save_folder <- "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/Conferences/ISCB 2024"


eb_R <- function(ICC, n_j){
  sigma2_e = 1-ICC
  sigma2_u = ICC
  
  R = sigma2_u/(sigma2_u + (sigma2_e/n_j))
  return(R)
}

R_table <- expand_grid(ICC = 0:20/100,
                   n_j = c(10, 50, 200, 1000)) |> 
  mutate(R = eb_R(ICC, n_j) |> round(2))

eb_plot <- R_table |> ggplot(aes(x = ICC, y = R, color = as.factor(n_j))) +
  geom_line(linewidth = 2) +
  labs(x = "ICC", 
       y = "Penalisation (1 = no penalisation)", color = "Cluster Size",
       title = "Peanalisation applied in \n Empirical Bayes Predictions") +
  theme_minimal(base_size = 18) +
  theme(legend.position = "top") + 
  theme(plot.title = element_text(hjust = 0.5)) 
eb_plot

ggsave(plot = eb_plot, filename =  file.path(save_folder, "eb_plot.png"), width = 14, height = 14, units = "cm")



omega <- function(ICC_x, J, n_j, ICC_y){
  sigma2_e = 1-ICC_y
  sigma2_u = ICC_y
  
  s_xB2 = ICC_x
  s_xW2 = 1 - ICC_x
  
  se_beta_b2 = sigma2_u/((J-1)*s_xB2)
  se_beta_w2 = sigma2_e*(1 - 1/n_j)/((J*(n_j - 1)-1)*s_xW2)
  
  omega = se_beta_b2/(se_beta_b2 + se_beta_w2)
  return(omega)
}

omega(ICC_x = 0.05, J = 64, n_j = 50, ICC_y = 0.05)
omega(ICC_x = 0.9, J = 64, n_j = 50, ICC_y = 0.05)

omega_data <- expand_grid(ICC_x = 1:99/100, J = 8, n_j = c(50, 200, 1000), ICC_y = 0.05) |>  
  mutate(omega =  1- omega(ICC_x, J, n_j, ICC_y) |> round(2))

omega_plot <- omega_data |> ggplot(aes(x = ICC_x, y = omega, color = as.factor(n_j))) +
  geom_line(linewidth = 2) +
  labs(
    x = "Predictor ICC*", y = "Relative contribution", 
    color = "Cluster Size:", 
    title = "Use of between-study information \n in random-intercept models",
    caption = "*Ratio of between-study sum of square to total  \n sum of squares  for predictors") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  theme_minimal(base_size = 18) +
  theme(legend.position = "top")
omega_plot

ggsave(plot = omega_plot, filename =  file.path(save_folder, "omega_plot.png"), width = 14, height = 14, units = "cm")
  


