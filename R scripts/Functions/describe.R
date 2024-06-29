describe_df <- function(data, metadata = var_metadata, overall_n = NULL) {

  if(is.null(overall_n)){
    overall_n <- nrow(data)
  }
  
  data |> 
    select(-any_of("ID")) |> 
    pivot_longer(cols = everything(), names_to = "variable_name", values_to = "value") |> 
    group_by(variable_name) |> 
    sum_detail(column = "value") |> 
    mutate(complete = paste0(round((n/overall_n*100)), "%"),
           variable_name = get_label(variable_name, metadata = metadata)) |> 
    select(n, complete, everything()) |> 
    flextable::flextable() |> 
    colformat_double(digits = 1) |> 
    autofit() 
}


describe_all <- function(data, metadata) {
  data <- data |> select(-any_of(c("ID")))
  walk(colnames(data),  describe_variable, metadata = metadata, data = data)
}


describe_variable <- function(variable_name, metadata, data) {
  label1 <- get_label(variable_name, metadata = metadata, label_no = 1)
  
  label2 <- get_label(variable_name, metadata = metadata, label_no = 2)
  sum_cat <- metadata$sum_cat[metadata$variable_name == variable_name]
  
  
  if(sum_cat){
    print(label2)
    desc_df <- describe_cat(data, variable_name)
    print(desc_df)
  } else {
    describe_cont(data, variable_name, label1, label2)
  }
  
}
describe_cont <- function (data, variable_name, label1, label2) {
  try(hist(data[[variable_name]], main = label2, xlab = label1))
}

describe_cat <- function(data, variable_name) {
  variable <- data |> pull(variable_name)
  
  counts <- table(variable, useNA = "ifany")
  frequencies <- prop.table(counts)
  
  # Create a data frame with counts and frequencies
  df <- data.frame(
    Category = names(counts),
    Count = as.vector(counts),
    Frequency = as.vector(frequencies)
  )
  df$Frequency <- paste0(round(df$Frequency * 100, 1), "%")
  
  
  # Add total row
  total_row <- c("Total", sum(df$Count), 1)
  df <- rbind(df, total_row)
  
  return(df)
  
}