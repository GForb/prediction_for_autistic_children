process_adi65 <- Vectorize(function(adi65) {
  ifelse(adi65 <=3, adi65, NA)
})
