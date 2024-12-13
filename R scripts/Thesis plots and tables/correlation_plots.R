# Load SDQ data (wide form)
# Select all predictor variables from primary model



save_cor_plot <- function(outcome_name) {
  plots_folder <- here::here(thesis_plots, "Main Results")
  
  results_folder <- get(paste0("results_folder_", outcome_name))
  print(results_folder)

  analysis_data_wide <- readRDS(here(derived_data, glue::glue("pooled_{outcome_name}_wide.Rds"))) |> filter(base_all_complete, out_all_complete) 
  if(outcome_name == "sdq") {
    analysis_data_wide <- analysis_data_wide |> filter(autism != "post baseline")
  }
  nrow(analysis_data_wide) |> print()
  analysis_spec <- readRDS(file = here::here(results_folder, "analysis_spec.rds"))
  analysis_spec |> count(predictor_set) |> print()  
  predictors <- analysis_spec |> 
    filter(predictor_set == "pred3_mt") |> 
    slice(1) |> 
    pull(predictors) |> 
    strsplit(split = " ") |> 
    unlist() |> 
    str_replace("///\\n", "")
  
  outcome <- analysis_spec |> 
    filter(predictor_set == "pred3_mt") |> 
    slice(1) |> 
    pull(outcome)
  
  print(predictors)
  print(outcome)
  cor_data <- analysis_data_wide |> select(any_of(predictors), any_of(paste0("base_", outcome))) |> 
    select(starts_with(glue::glue("base_{outcome_name}")), everything())
  
  print(cor_data)
  
  cor_mat <- cor(cor_data, use = "pairwise.complete.obs", method = "pearson") |> round(2)
  rownames(cor_mat) <- get_label(rownames(cor_mat), label_no = 3)
  colnames(cor_mat) <- get_label(rownames(cor_mat), label_no = 3)
  
  plot <-   ggcorrplot::ggcorrplot(cor_mat, type = "lower", outline.col = "white", lab = TRUE,  ggtheme = ggplot2::theme_gray,
                                   colors = c("#6D9EC1", "white", "#E46726"), lab_size = 2.5) +
    theme_grey() +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1),
          legend.position = "none") +
    labs(x = "", y = "")
  plot |> print()
  ggsave(here::here(plots_folder, paste0("cor_", outcome_name, ".png")), width = 18, height = 12, units = "cm")
}



save_cor_plot("sdq")
save_cor_plot("cbcl")
save_cor_plot("vabs")