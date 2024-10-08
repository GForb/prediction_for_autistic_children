pool_est_all_metrics <- function(results) {
  metrics <- results |> pull(metric) |> unique()
  pool_est_results <- function(metric){
    pool_est_results <- pool_est(metric, results)
  }
  map(metrics, pool_est_results) |> bind_rows()
  
  
}

pool_est <- function(my_metric, results) {
  results <- results |> filter(metric == my_metric)

  ma <- results |> slice(1) |> pull(meta_analysis)
  df <- ma[[1]]$df

  pooled_estiamtes <- miceafter::pool_scalar_RR(results$est, se = results$se, df_small = FALSE, dfcom = df, statistic = TRUE) # Edit the df_small option to change whether a dof correction is made.
  pooled_tau2 <- mean(results$tau2)
  
  tibble(est = pooled_estiamtes$pool_est,
         se = pooled_estiamtes$pool_se,
         t = pooled_estiamtes$t,
         df = df,
         tau2 = pooled_tau2
         ) |> 
    mutate(metric = my_metric, 
           ci.lb = est - qt(0.975, df)*se,
           ci.ub = est + qt(0.975, df)*se,
           se_pred = sqrt(tau2 + se^2),
           pi.lb = est - qt(0.975, df)*se_pred,
           pi.ub = est + qt(0.975, df)*se_pred) |> 
    select(metric, est, se, tau2, ci.lb, ci.ub ,  pi.lb, pi.ub , everything())
}



