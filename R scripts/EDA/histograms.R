library(tidyverse)

load( file = file.path(derived_data, "ssc.Rdata"))

ssc_data_long <- ssc_data |> pivot_longer(cols = starts_with("cbcl"), names_to = "cbcl_measure", values_to = "score")

ssc_data_long |> 
  ggplot(aes(x = score)) + 
  geom_histogram() +
  facet_grid(rows = "cbcl_measure")

plot(ssc_data$cbcl_int_total, ssc_data$cbcl_int_tscore)
plot(ssc_data$cbcl_ext_total, ssc_data$cbcl_ext_tscore)