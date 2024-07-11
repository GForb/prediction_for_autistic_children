back_transform_rsq <- function(transformed_rsq) {
  rsq <- 1-exp(-transformed_rsq)
  return(rsq)
}