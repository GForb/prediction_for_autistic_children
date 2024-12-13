# Are these useful models
#???
# Functions

get_cutoff <- function(risk, cutoff, rmse) {
  z_score <- qnorm(risk)
  threshold_pred <- cutoff + z_score * rmse
  return(threshold_pred)
}

exceedance_prob <- Vectorize(function(prediction, rmse, threshold) {
  # Calculate the probability that the true value exceeds the threshold
  prob <- 1 - pnorm(threshold, mean = prediction, sd = rmse)
  return(prob)
})

# Set up outcome

myOutcome <- "sdq_emot_p"
data <- pi_data_sdq <- readRDS(here(derived_data, "pi_data_sdq.Rds"))
cutoff <- sdq_cutoffs |> filter(outcome == myOutcome) |> pull(cutoff)
rmse <- 1.94
  
prediction_data <- data |> filter(outcome == "sdq_emot_p") |> arrange(pred) |> select(ID, actual, pred, base_spline1)
p_threshold <- 0.3



net_benifit <- tibble(thresholds = seq(0.1, 0.9, 0.1)) |> 
  rowwise() |> 
  mutate(
    pred_cutoff = get_cutoff(thresholds, cutoff = 5, rmse = 2),
    n =  prediction_data  |> nrow(),
    true_poistive = prediction_data |> filter(pred > pred_cutoff, actual >= 5) |> nrow(),
    false_positive = prediction_data |> filter(pred > pred_cutoff, actual < 5) |> nrow(),
    screen_all_tp = prediction_data |> filter(actual >=5) |> nrow(),
    screen_all_fp  = prediction_data |> filter(actual <5)  |> nrow())|> 
  ungroup() |> 
  mutate(
    net_benifit = true_poistive/n-(false_positive/n *thresholds/(1-thresholds)),
    net_benifit_screen_all = screen_all_tp/n-(screen_all_fp/n *thresholds/(1-thresholds))
    
  )


prediction_data_with_bin_out <- prediction_data |> mutate(outcome = case_when(actual >= cutoff ~ 1, TRUE ~ 0),
                                                          base_dichot = case_when(base_spline1 >= cutoff ~ 1, TRUE ~ 0),
                                                          outcome_name = "what",
                                                          risk_from_normal = exceedance_prob(pred, rmse, cutoff)) |> 
  select(risk_from_logistic = pred, everything())


dc <- dcurves::dca(
  outcome ~ risk_from_normal + risk_from_logistic + base_dichot,
  thresholds = seq(0.1, 1, by = 0.1),
  data = prediction_data_with_bin_out,
  as_probability = "risk_from_logistic"
) 

dc |> as_tibble() |>  dplyr::filter(!is.na(net_benefit)) %>%
  ggplot(aes(x = threshold, y = net_benefit, color = label)) +
  geom_line() +
  coord_cartesian(ylim = c(-0.0408791208791209, 0.408791208791209
  )) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Threshold Probability", y = "Net Benefit", color = "") +
  theme_bw() + facet_grid(vars(variable))

dcurves::dca(
  outcome ~ risk_from_normal,
  thresholds = seq(0.1, 1, by = 0.1),
  data = prediction_data_with_bin_out,
) |> as.tibble()