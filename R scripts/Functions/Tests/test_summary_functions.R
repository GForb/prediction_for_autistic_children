# add code which loads clean data
test_that("make_summary_table", {
  load(file.path(file.path(derived_data, "gui.Rdata")))
  
  table <-  make_summary_table(gui_data)
  expect_equal(ncol(table), 9)
})

test_that("make_summary_by_wave", {
  load(file.path(file.path(derived_data, "gui.Rdata")))
  
  suppressWarnings( tables <- make_summary_by_wave(gui_data))
  expect_equal(length(tables), 3)
})
