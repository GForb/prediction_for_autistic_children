check_cbcl_items <- function(cbcl_item_data) {
  permitted_items <- c(
    paste0("cbcl_item_", 1:55),
    paste0("cbcl_item_56", letters[1:8]),
    paste0("cbcl_item_", 57:113),
    paste0("cbcl_item_113", letters[1:3])
  )
  
  
  
  
  # check all names are permitted
  colnames <- colnames(cbcl_item_data)
  is_subset <- all(colnames %in% permitted_items)
  if (!is_subset) {
    bad_items <- paste(colnames[!(colnames %in% permitted_items)], sep = ", ")
    stop(paste("The following items are not permitted: ", bad_items))
  }
  
  # List missing items:
  missing_items <- permitted_items[!(permitted_items %in% colnames)]
  if (length(missing_items > 0)) {
    print("Missing items:")
    print(missing_items)
  } else {
    print("variables exist for all possible items")
  }
  
  # Check values
  value_checks <- cbcl_item_data |> 
    pivot_longer(everything()) |> 
    filter(!(value %in% c(0, 1, 2, NA)))

  
  if(nrow(value_checks) > 0) {
    
    bad_items <- value_checks |> 
      group_by(name) |> summarise(min = min(value, na.rm = TRUE), max = max(value, na.rm = TRUE))
    
    print("Out of range items:")
    print(bad_items)
    stop("Some values are not 0, 1, 2 or NA")
  }
                       
  
}

calc_cbcl_dsmIV_domains <- function(data){
  domains <- list("cbcl_aff", "cbcl_anx", "cbcl_som", "cbcl_adhd", "cbcl_odd", "cbcl_con")
  items <- list(
    paste0("cbcl_item_", c(5, 14, 18, 24, 35, 52, 54, 76, 77, 91, 100, 102, 103)),
    paste0("cbcl_item_", c(11, 29, 30, 45, 50, 112)),
    paste0("cbcl_item_56", letters[1:7]),
    paste0("cbcl_item_", c(4, 8, 10, 41, 78, 93, 104)),
    paste0("cbcl_item_", c(3, 22, 23, 86, 95)),
    paste0("cbcl_item_", c(15, 16, 21, 26, 28, 37, 39, 43, 57, 67, 72, 81, 82, 90, 97, 101, 106))
  )
  if(is.null(data)){
    return(tibble(domain = domains |> unlist(), items ))
  } else {
  print(domains)
  print(items)
  
  for(i in 1:length(domains)){
    data <- score_domain(domains[[i]], items[[i]], data)
  }
  return(data)
  }
}

score_domain <- function(domain_name,items, data) {
  n_items = length(items)
  n_items_present <- sum(colnames(data) %in% items)
  
  print(paste0("Number of items present for ", domain_name, ": ", n_items_present, " out of ", n_items))
  
  if(n_items_present >0){
    data[,domain_name] <- data |> select(any_of(items)) |> rowSums()
    data[,domain_name] <- data[,domain_name]/n_items_present*n_items
  } else {
    stop(paste0("No items present for ", domain_name))
  }
  
  return(data)
  
}

check_cbcl_domain_scores <- function(data1, data2) {
  domains <- c("cbcl_aff", "cbcl_anx", "cbcl_som", "cbcl_adhd", "cbcl_odd", "cbcl_con")
  if("wave" %in% colnames(data1) & "wave" %in% colnames(data2)){
    by_cols <- c("ID", "wave")
  } else {
    by_cols <- "ID"
  }
  tol <- 0.01
  diffs <- inner_join(data1, data2, by = by_cols) |> 
    mutate(diff_cbcl_aff =  cbcl_aff.x - cbcl_aff.y,
           diff_cbcl_anx =  cbcl_anx.x - cbcl_anx.y,
           diff_cbcl_som =  cbcl_som.x - cbcl_som.y,
           diff_cbcl_adhd =  cbcl_adhd.x - cbcl_adhd.y,
           diff_cbcl_odd =  cbcl_odd.x - cbcl_odd.y,
           diff_cbcl_con =  cbcl_con.x - cbcl_con.y) |> 
    rowwise() |> 
    mutate(diff_any = all(
      abs(diff_cbcl_aff) < tol | (is.na(cbcl_aff.x) & is.na(cbcl_aff.y)), 
      abs(diff_cbcl_anx) < tol | (is.na(cbcl_anx.x) & is.na(cbcl_anx.y)), 
      abs(diff_cbcl_som) < tol | (is.na(cbcl_som.x) & is.na(cbcl_som.y)),
      abs(diff_cbcl_adhd) < tol | (is.na(cbcl_adhd.x) & is.na(cbcl_adhd.y)), 
      abs(diff_cbcl_odd) < tol | (is.na(cbcl_odd.x) & is.na(cbcl_odd.y)), 
      abs(diff_cbcl_con) < tol | (is.na(cbcl_con.x) & is.na(cbcl_con.y)))) |> 
    select(ID, 
           starts_with("diff"),  
            starts_with("cbcl_aff"),
             starts_with("cbcl_anx"),
             starts_with("cbcl_som"),
            starts_with("cbcl_adhd"),
           starts_with("cbcl_odd"),
           starts_with("cbcl_con"),
            everything())
    
    
  return(diffs)
  
}

