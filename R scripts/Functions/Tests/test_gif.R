#making a gif


test_that("gif_plots", {
  load(file.path(derived_data, "lsac.Rdata"))
  data <- lsac_data |> filter(wave <3) # reducing the number of waves to speed up test
  
  system_time <- Sys.time()
  gif_plots <- make_raincloudplot_wave(data, "sdq_tot_p", "Total SDQ", "springgreen")
  suppressWarnings(gif <- create_gif(gif_plots))
  image_write(image = gif, path = file.path(outputs,"Tests", "lsac_gif.gif"))
  #check if document has been created
  expect_true("lsac_gif.gif" %in% list.files(file.path(outputs, "Tests")))
  file_info <- file.info(file.path(outputs, "Tests/lsac_gif.gif"))
  expect_true(file_info$mtime > system_time)
})

test_that("gif_plots2", {
    load(file.path(derived_data, "lsac.Rdata"))
    data <- lsac_data |> filter(wave <3) # reducing the number of waves to speed up test
  
  system_time <- Sys.time()
  gif_plots2 <- make_raincloudplot_wave(data, "sdq_tot_p", "Total SDQ", "springgreen", var_metadata = var_metadata)
  suppressWarnings(gif2 <- create_gif(gif_plots2))
  image_write(image = gif2, path = file.path(outputs,"Tests", "lsac_gif2.gif"))
  #check if document has been created
  expect_true("lsac_gif2.gif" %in% list.files(file.path(outputs, "Tests")))
  file_info <- file.info(file.path(outputs, "Tests/lsac_gif2.gif"))
  expect_true(file_info$mtime > system_time)
  
  
})

