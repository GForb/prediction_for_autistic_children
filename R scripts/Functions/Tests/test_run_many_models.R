test_fun <- function(a, b) {
  print(a + b)
}

spec <- tibble(a = 1:3, b = 11:13, c = 1:3, model_function = list(test_fun))

test_results <- run_many_models(spec)

test_results