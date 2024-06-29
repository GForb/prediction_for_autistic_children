make_rcs <- function(data, variable) {
  col <- data[[variable]]
  knots <- quantile(col, probs = 0.5, na.rm = TRUE, type = 6) # type 6 is used to match exactly the output from stata
  boundry_knots <- quantile(col, probs = c(0.1, 0.90), na.rm = TRUE, type = 6) # type 6 is used to match exactly the output from stata
  splines::ns(col, knots = knots, Boundary.knots = boundry_knots)
}
