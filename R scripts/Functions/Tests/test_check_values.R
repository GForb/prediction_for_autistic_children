

test_that("check_column", {
  check_column(column = 1:10, min = 1, max = 10) |> expect_false()
  check_column(column = 1:12, min = 1, max = 10) |> expect_true()
  check_column(column = 0:10, min = 1, max = 10) |> expect_true()
})

test_that("check_columns", {
  data <- data.frame(a = 1:10, b = 2:11, c = 0:9)
  expect_equal(check_columns(data, min = 1, max = 10), c("b", "c"))
})



test_that("check_data", {
  check_nothing <- data.frame(dont_check = 1)
  good_data <- data.frame(sdq_tot_p = c(1:40),sdq_tot_t = c(1:40), gender = c(1:2), sdq_pro = c(1:40), dont_check = 1)
  bad_data <- data.frame(sdq_tot_p = c(11:50),sdq_tot_t = c(11:50), sdq_pro_t = c(1:10), gender = c(1:2), sdq_pro_p = c(2:11), dont_check = 1)
  bad_data1 <- bad_data |> select(sdq_tot_p)
  check_data("sdq_tot", data = check_nothing, min = 1, max = 40) |> expect_equal(character(0))
  check_data("sdq_tot", good_data, min = 1, max = 40) |> expect_equal(character(0))
  check_data("sdq_tot", bad_data, min = 1, max = 40) |> expect_equal(c("sdq_tot_p", "sdq_tot_t"))
  check_data("sdq_tot", bad_data1, min = 1, max = 40) |> expect_equal(c("sdq_tot_p"))
  
})


test_that("check_values", {
  check_nothing <- data.frame(dont_check = 1)
  good_data <- data.frame(sdq_tot_p = c(1:40),sdq_tot_t = c(1:40), sdq_pro_t = c(1:10), gender = c(1:2), sdq_pro_p = c(1:10), dont_check = 1)
  bad_data <- data.frame(sdq_tot_p = c(11:50),sdq_tot_t = c(1:40), sdq_pro_t = c(1:10), gender = c(1:2), sdq_pro_p = c(2:11), dont_check = 1)
  bad_data1 <- bad_data |> select(sdq_tot_p)
  
  nothing <- check_values(data = check_nothing, var_metadata)  
  expect_true(nothing$passed)
  
  good <- check_values(good_data, var_metadata) 
  expect_true(good$passed)
  
  bad <- check_values(bad_data, var_metadata)
  expect_equal(bad$var_out_of_range, c("sdq_tot_p", "sdq_pro_p"))
  expect_false(bad$passed)
  
  bad1 <- check_values(bad_data1, var_metadata)
  expect_false(bad1$passed)
  expect_equal(bad1$var_out_of_range, "sdq_tot_p")
  
})

