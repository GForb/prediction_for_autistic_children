# add code which loads clean data
library(tidyverse)
library(here)
library(ggplot2)
library(patchwork)

source(here("R scripts", "Functions","raincloudplot_function.R" ))
load(file.path(derived_data, "gui.Rdata"))


# make_raincloudplot  ----
column <- gui_data |> select(sdq_emot_p)
plot1 <- make_raincloudplot(column, col_label = "Parent 1", colour = "darkgreen")
plot1

# make_raincloudplots  ----
plots <- gui_data |> 
  select(ends_with("_p")) |> 
  make_raincloudplots(colour = "darkgreen")
plots


# compare_raincloudplots  ----
plots <- final_plot <- gui_data |> 
  select(ends_with("_p")) |> 
  compare_raincloudplots("darkgreen", 3)

