test_data <- data.frame(
  ID = c(1,1,2,2,3,3,4,4,5,5),
  wave = 1:2,
  iq = rnorm(10, 100, 20),
  sdq = rnorm(10, 10, 2)
)

test_that("make_wide_data", {
  wide_data <- make_wide_dataset(test_data)
  expect_equal(colnames(wide_data), c("ID", "iq_1", "iq_2", "sdq_1", "sdq_2"))
})

