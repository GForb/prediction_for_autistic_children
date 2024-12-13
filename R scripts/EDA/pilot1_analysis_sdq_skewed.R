
# What data!!!
set.seed = 12345


data <- readRDS(here::here(derived_data, "SDQ_gen_pop", "sdq_eda_1tp_test_data.rds")) |> 
  select(ID, studyid, wave, age, starts_with("y"), starts_with("sdq")) |> na.omit() |> 
  mutate(mcs = case_when(studyid == "mcs" ~ 1,
                         TRUE ~ 0),
         gui = case_when(studyid == "gui" ~ 1,
                         TRUE ~ 0),
         lsac_k = case_when(studyid == "lsac_k" ~ 1,
                            TRUE ~ 0) ,
         lsac_b = case_when(studyid == "lsac_b" ~ 1,
                            TRUE ~ 0) )

# Data Summaries
# data$sdq_emot_p |> hist(breaks = 10)
# 
# data$sdq_hyp_p |> hist(breaks = 10)
# 
# data$sdq_cond_p |> hist(breaks = 10)
# 
# data$sdq_pro_p |> hist(breaks = 10)
# data$sdq_peer_p |> hist(breaks = 10)

mean(data$sdq_emot_p, na.rm = TRUE)
var(data$sdq_emot_p, na.rm = TRUE)

summarise_age <- function(data) {
  data |>  summarise(mean_age = mean(age, na.rm = TRUE), 
                     min_age = min(age, na.rm = TRUE),
                     max_age = max(age, na.rm = TRUE),
                     n = sum(!is.na(age))) |> 
    print(n = 30)
  
} 

data  |> group_by(studyid, wave) |> summarise_age()





data |> group_by(studyid) |> summarise_age()
data |> group_by(studyid) |> select(-age, age = y_age) |>  summarise_age() 



get_model_evaluate_df <- function(model_name, model, test_data) {
  AIC <- tibble(metric = "AIC", coef = extractAIC(model)[2])
  preds <- predict(model, newdata = test_data, type = "response")
  preds_actual <- tibble(actual = test_data$y_sdq_emot_p, pred = preds) |> 
    mutate(log_pred = log(pred))
  
  
  poisson_calib_slope <- tibble(
    metric = "poisson_calib_slope",
    coef = glm("actual ~ log_pred", family = poisson, data = preds_actual) |> coef() |> pluck(2) 
  )
  neg_bin_calib_slope <- tibble(
    metric = "neg_bin_calib_slope",
    coef = MASS::glm.nb("actual ~ log_pred", data = preds_actual) |> coef() |> pluck(2) 
  )
  
  model_performance <- ProfacSims:::evaluate_performance_continuous(data.frame(test_data), model) |> 
    bind_rows(poisson_calib_slope, neg_bin_calib_slope, AIC) |> 
    mutate(model = model_name,
           model_object = list(model),
           preds = list(preds_actual)) 
  
}



# Modelling and assessment of model performance
get_performance <- function(rep_no){
  
  train_data <- data |> slice_sample(n = 200, by = studyid)
  test_data <- data |> filter(!(ID %in% train_data$ID))
  
  model_linear <- glm("y_sdq_emot_p ~ gui + lsac_b + lsac_k + mcs + age + sdq_emot_p +  sdq_cond_p + sdq_hyp_p + sdq_peer_p + sdq_pro_p + y_age", 
                      data = train_data, family = gaussian)
  
  
  model_poisson  <- glm("y_sdq_emot_p ~ gui + lsac_b + lsac_k + mcs + age + sdq_emot_p +  sdq_cond_p + sdq_hyp_p + sdq_peer_p + sdq_pro_p + y_age", 
                        data = train_data, family = poisson)
  
  
  model_neg_bin <-  MASS::glm.nb("y_sdq_emot_p ~ gui + lsac_b + lsac_k + mcs + age + sdq_emot_p +  sdq_cond_p + sdq_hyp_p + sdq_peer_p + sdq_pro_p + y_age", 
                                 data = train_data)
  

  model_performance <- bind_rows(
    get_model_evaluate_df(model_name = "Linear", model = model_linear, test_data = test_data),
    get_model_evaluate_df(model_name = "Poisson", model = model_poisson, test_data = test_data),
    get_model_evaluate_df(model_name = "NegativeBinomial", model = model_neg_bin, test_data = test_data)
  )
  
  # Model performace template
  # model_performance <- bind_rows(linear_pref,pois_perf , neg_bin_perf) |> 
  #   select(-se) |> 
  #   pivot_wider(names_from = metric, values_from = coef) |> 
  #   mutate(AIC = aics,
  #          rep_no = rep_no,
  #          preds = list(preds_actual))
  
  
  return(model_performance |> mutate(rep_no = rep_no) |> tibble())
  
}

tictoc::tic()
model_performance <- map(1:100, get_performance) |> 
  bind_rows() 
tictoc::toc()

saveRDS(model_performance, here::here(data_and_outputs,"Results", "Pilot Analysis", "sdq_emot_p_skewed_model_performance.rds"))

model_performance |> filter(model == "Linear", metric == "calib_itl") |> pull(coef) |> quantile(coef, probs = 0.025) > round(2)



model_performance_summary <- model_performance |> group_by(model, metric) |> 
  summarise(mean = t.test(coef,conf.level = 0.95)  |>  broom::tidy() |>  pull(estimate) |> round(2), 
            conf.low = quantile(coef, probs = 0.025) |>  round(2), 
            conf.high = quantile(coef, probs = 0.975) |> round(2)) |> 
  mutate(summary = paste0(mean, " (" , conf.low , ", ",conf.high, ")")) |> 
  select(model, metric, summary) |> 
  filter(metric %in% c("calib_itl", "calib_slope", "r-squared"))|> 
  pivot_wider(names_from = metric, values_from = summary) 

headers <- c("Model", "Calibration In-the-large", "Calibration Slope", "$R^2$")
headers |> rbind(model_performance_summary |> as.data.frame()) |> 
  huxtable::hux(add_colnames = FALSE) |> 
  huxtable::set_top_border(row = 2, value = 0.5) |>
  save_hux_table(file_name = "Ch3_pilot1.tex",
                 caption = "Estimates (95\\% CI) of calibration and $R^2$ for different models 
                 for the emotional problems domain of the SDQ, developed on data from non-autistic 
                 participants from four general population cohorts. Results are calculated on 
                 hold-out data not used in model development.",
                 label = "pilot1_res")

selected_rep <- model_performance |> 
  select(model, metric, coef, rep_no) |> 
  filter(metric == "calib_slope") |>
  pivot_wider(names_from = "model", values_from = "coef") |> 
  filter(abs(Linear - 1) < 0.03,
         abs(Poisson - 0.81) < 0.03,
         abs(NegativeBinomial - 0.69) < 0.03) |> 
  slice_sample(n = 1) |> 
  pull(rep_no)

selected_rep_pref <- model_performance |> 
  filter(rep_no == selected_rep) |> 
  select(metric, coef, model) |> 
  pivot_wider(names_from = "metric", values_from = "coef")


selected_rep_data <-  model_performance |> 
  filter(rep_no == selected_rep) |> 
  filter(metric == "calib_slope") |> 
  select(model, preds) |> 
  pivot_wider(names_from = "model", values_from = "preds") 

linear_tibble <- selected_rep_data$Linear[[1]] |> mutate(row_num = row_number())
poisson_tibble <- selected_rep_data$Poisson[[1]]|> mutate(row_num = row_number())
negative_binomial_tibble <- selected_rep_data$NegativeBinomial[[1]]|> mutate(row_num = row_number())

# Combine the tibbles
combined_data <- bind_rows(
  linear_tibble %>% mutate(model = "Linear"),
  poisson_tibble %>% mutate(model = "Poisson"),
  negative_binomial_tibble %>% mutate(model = "NegativeBinomial")
)


my_calib_plot <- ggplot(combined_data, aes(x = pred, y = actual)) + 
  geom_jitter(size = 0.001, width = 0, height = 0.3) + 
  geom_abline(data = selected_rep_pref, aes(intercept = calib_itl, slope = calib_slope, linetype = "Calibration Slope"), 
                                        color = "black", show.legend = TRUE) + 
  geom_smooth(aes(linetype = "Smoothed Calibration"), 
              method = "auto", se = FALSE, fullrange = FALSE, level = 0.95, 
              linewidth = 0.5, colour = "blue") + 
  geom_abline(aes(linetype = "Perfect calibration",intercept = 0, slope = 1), color = "pink", show.legend = TRUE,  linewidth = 1.2) + 
  geom_vline(aes(linetype = "Maximum possible score",xintercept = 10), color = "red", show.legend = TRUE,  linewidth = 1.2) + 
  facet_wrap(facets = vars(model), nrow = 1) + 
  scale_linetype_manual(
    name = "", 
    values = c(
      `Calibration Slope` = "solid", 
      `Smoothed Calibration` = "solid", 
      `Perfect calibration` = "dotdash",
      `Maximum possible score` = "dashed"),
    guide = guide_legend(override.aes = list(colour = c("black", "red", "pink", "blue")),
                         nrow = 1)
    )+ 
  labs(y = "Observed", 
       x = "Predicted", 
       title = "Calibration of Poisson and negative binomial models is poor", 
       caption = "Vertical Jitter applied to observed values") +
  theme_bw() + 
  theme(legend.position = "top")


my_calib_plot

plot_folder <- here::here(outputs,"Thesis Plots")
ggsave( filename = here::here(plot_folder, "Ch3_ipdma_skewed_sdq_calib.png"), plot = my_calib_plot, device = "png", units = "cm",height = 8, width = 18)

# Plotting resiudals

# REset seed:
set.seed(61464641)

train_data <- data |> slice_sample(n = 200, by = studyid)
test_data <- data |> filter(!(ID %in% train_data$ID))

model_linear <- glm("y_sdq_emot_p ~ gui + lsac_b + lsac_k + mcs + age + sdq_emot_p +  sdq_cond_p + sdq_hyp_p + sdq_peer_p + sdq_pro_p + y_age", 
                    data = train_data, family = gaussian)

preds_in_sample <- predict(model_linear, newdata = train_data, type = "response")
Residuals <- train_data$y_sdq_emot_p - preds_in_sample 
png(file = here::here(plot_folder, "Ch3_pilot_1_residuals.png"), width = 18, height = 8, "cm", res = 300)
hist(Residuals, main =)
dev.off()
summary(Residuals)

preds_out_sample <- predict(model_linear, newdata = test_data, type = "response")
res_out <-  test_data$y_sdq_emot_p - preds_out_sample
hist(res_out)
summary(res_out)

moments::skewness(res_out)
moments::skewness(Residuals)