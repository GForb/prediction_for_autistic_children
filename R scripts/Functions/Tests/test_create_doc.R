test_that("create_doc", {
  source(here::here("R scripts", "Functions", "create_doc.R"))
  system_time <- Sys.time()
  create_doc( 
    template = here::here("Rmarkdown/test_create_doc.rmd"),
    dataset = "gui", 
    variables = c("sdq_tot_p", "sdq_tot_t"), 
    colour = "red", 
    output_file = "Tests/test"
  )
  
  #check if document has been created
  expect_true("test.html" %in% list.files(file.path(outputs, "Tests")))
  file_info <- file.info(file.path(outputs, "Tests/test.html"))
  expect_true(file_info$mtime > system_time)
})

