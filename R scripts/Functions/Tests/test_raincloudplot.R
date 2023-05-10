# add code which loads clean data



load(file.path(derived_data, "gui.Rdata"))

#testing functions
test_that("find_muin_max", {
  expect_equal(find_min_max("sdq_tot_p", var_metadata),
               c(0, 40))
  expect_equal(find_min_max("bla", var_metadata),
               NULL)
})

test_that("divide_by_waves", {
  data_divided2 <- divide_by_wave(gui_data)
  expect_equal(length(data_divided2), 3)
  expect_equal(nrow(data_divided2[[3]]), 110)
  
})

variables <- colnames(gui_data)
ylims <- lapply(variables, find_min_max, var_metadata)

# make_raincloudplot  ----
column <- gui_data |> select(sdq_emot_p)
plot1 <- make_raincloudplot(column, col_label = "Parent 1", colour = "darkgreen")
plot1

plot2 <- make_raincloudplot(column, col_label = "Parent 1", colour = "darkgreen", ylims = c(-5, 15))
plot2



# make_raincloudplots  ----
plots3 <- gui_data |> 
  filter(wave ==3) |> 
  select(ends_with("_p")) |> 
  make_raincloudplots(colour = "darkgreen")
plots3

plots4 <- gui_data |> 
  filter(wave ==3) |> 
  select(ends_with("_p")) |> 
  make_raincloudplots(colour = "darkgreen", var_metadata)
plots4


# compare_raincloudplots  ----
plots <- final_plot <- gui_data |> 
  select(ends_with("_p")) |> 
  compare_raincloudplots("darkgreen", 3)


# raincloud plots by wave

wave_plots1  <- make_raincloudplot_wave(gui_data,  "sdq_tot_p", "Total SDQ", "springgreen")
wave_plots1

wave_plots2  <- make_raincloudplot_wave(gui_data,  
                                        "sdq_tot_p", 
                                        "Total SDQ", "springgreen", 
                                        var_metadata = var_metadata)
wave_plots2

combined_plot <- combined_raincloudplot_wave(gui_data, "sdq_tot_p", "red", var_metadata)
combined_plot

combined_plot2 <- combined_raincloudplot_wave2(
  data = gui_data,
  variable = "sdq_tot_p",
  col_label = "SDQ Total", 
  colour =  "red",
  metadata =  var_metadata)
combined_plot2

box_plot_test(
  data = gui_data,
  variable = "sdq_tot_p",
  col_label = "SDQ Total", 
  colour =  "red",
  metadata =  var_metadata)
