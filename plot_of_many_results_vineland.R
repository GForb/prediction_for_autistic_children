# Load required packages
library(ggplot2)
library(strngr)
results_folder <- here::here(data_and_outputs, "Results", "VABS", "Prelim")
data <- readRDS(here::here(results_folder, "results_meta_analysis.rds")) 

data <- data |>
  filter(model == "results_reg",
         intercept_est_method == "estimate") |> 
  select(outcome, value = `r-squared`  ) 

# Sample data
data <- data.frame(
  outcome = c("vabs_com_ae", "vabs_dls_ae", "vabs_soc_ae"),
  value = c("0.71 (0.49, 0.92) (0.31, 1.11) [0.011]",
            "0.71 (0.6, 0.81) (0.6, 0.81) [0]",
            "0.7 (0.61, 0.78) (0.61, 0.78) [0]")
)

# Process the data
processed_data <- data %>%
  mutate(
    est = str_extract(value, "^[^ ]+") |> as.numeric(),
    ci = str_extract(value, "\\(([^\\)]+)\\)"),
    pi = str_extract(value, "(?<=\\)\\s)\\(([^\\)]+)\\)"),
    ci.lb = str_extract(ci, "(?<=\\()[^,]+")|> as.numeric(),
    ci.ub = str_extract(ci, "(?<=, )[^\\)]+")|> as.numeric(),
    pi.lb = str_extract(pi, "(?<=\\()[^,]+")|> as.numeric(),
    pi.ub = str_extract(pi, "(?<=, )[^\\)]+")|> as.numeric()
  ) %>%
  select(outcome, est, ci.lb, ci.ub, pi.lb, pi.ub) |> 
  mutate(label = get_label(outcome , label_no = 1),
            position = factor(label) |> as.numeric() )

# Print the processed data
print(processed_data)

processed_data |> plot_many_ma()

