# functions implement scoring from this paper: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3612387/
# Work for ages:
# Module 1: no words: 4 to 14
# Module 1:, some words: 5 to 14
# Module 2: 7 to 16
# Module 3: 6 to 16


calculate_css_rrb <- Vectorize(function(rrb_raw_score, module, words = 1) {
  if(is.na(rrb_raw_score) | is.na(module)){
    return(NA_integer_)
  }
  module1_no_words <- data.frame(raw_score = c(0,1,2,3,4,5,6,7,8),
                                 css = c(4,4,4,6,7,8,8,9,10))
  module1_some_words <- data.frame(raw_score = c(0,1,2,3,4,5,6,7,8),
                                 css = c(4,4,6,6,7,8,9,10, 10))
    
  module2 <- data.frame(raw_score = c(0,1,2,3,4,5,6,7,8),
                                 css = c(4,4,6,6,7,8,9,10, 10))
  
  
  module3 <- data.frame(raw_score = c(0,1,2,3,4,5,6,7,8),
                        css = c(4,4,7,8,9,9,10,10,10))
  
  if (module == 1 & words == 0) {
    css <- module1_no_words[module1_no_words$raw_score == rrb_raw_score, "css"]
  } else if (module == 1 & words == 1) {
    css <- module1_some_words[module1_some_words$raw_score == rrb_raw_score, "css"]
  } else if (module == 2) {
    css <- module2[module2$raw_score == rrb_raw_score, "css"]
  } else if (module == 3) {
    css <- module3[module3$raw_score == rrb_raw_score, "css"]
  } else {
    return(NA_integer_)
  }
  return(css)
})


calculate_css_sa <- Vectorize(function(sa_raw_score, module, words = 1) {
  if(is.na(sa_raw_score) | is.na(module)){
    return(NA_integer_)
  }
  module1_no_words <- data.frame(raw_score = c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
                                 css = c(1, 1,1,2,2,2,3,3,3,3,4,5,5,6,6,7,7,8,8,9,10))
  module1_some_words <-  data.frame(raw_score = c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
                                    css = c(1,1,2,2,3,3,4,4,5,5,6,6,6,6,7,7,8,9,9,10,10))
  
  module2 <-  data.frame(raw_score = c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
                         css = c(1,1,2,3,3,4,5,5,6,6,6,7,7,7,8,8,9,9,10,10,10))
  
  
  module3 <-  data.frame(raw_score = c(0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
                         css = c(1,1,2,3,3,4,5,6,7,7,8,8,9,9,9,10,10,10,10,10,10))
  
  if (module == 1 & words == 0) {
    css <- module1_no_words[module1_no_words$raw_score == sa_raw_score, "css"]
  } else if (module == 1 & words == 1) {
    css <- module1_some_words[module1_some_words$raw_score == sa_raw_score, "css"]
  } else if (module == 2) {
    css <- module2[module2$raw_score == sa_raw_score, "css"]
  } else if (module == 3) {
    css <- module3[module3$raw_score == sa_raw_score, "css"]
  }  else {
    return(NA_integer_)
  }
  return(css)
})