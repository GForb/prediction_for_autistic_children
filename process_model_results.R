process_model_results <- function(name, results) {
  if(!is.null(results$base_sex)){
    return(tibble(analysis_name = name, coef = "Model did not run" ,summary = ""))
  } else  if(!is.null(results$r1)){
    n_str <- results |> slice(1) |> pull(N) |> as.character()
    n_tibble <- tibble(coef = "n",analysis_name = name,  summary = paste0("n = ", n_str))
    results <- extract_log_tables(name, results_folder) 
    processed_results <- process_stata_rtable(results,name, n_tibble) 
    return(processed_results)
    
  } else {
    n_str <- results |> slice(1) |> pull(N) |> as.character()
    n_tibble <- tibble(coef = "n", analysis_name = name,  summary = paste0("n = ", n_str))
    processed_results <- results |>
      filter(coef != "STUDY[study]", (b !=0 & !is.na(se)) ) |> 
      process_stata_rtable(name, n_tibble)
    return(processed_results)
  }
  
}

# This function is necessary because stata does not save the results from mixed after multiple imputaiton to r(table), the matrix used to extract results. 
# See statalist for discussion (or just my question): https://www.statalist.org/forums/forum/general-stata-discussion/general/1764584-feature-or-bug-no-r-table-after-running-a-mixed-model-with-multiple-imputation
#The function loads the log file saved with the analysis and exrtacts and formats the relevant table.


extract_log_tables <- function(name, results_folder) {
  file_path <- here::here(results_folder,
                          "Logs",
                          paste0(name, "_model_only.log"))
  log_text <- readLines(file_path)
  start_idx <- grep("^\\s*study_", log_text)[1]  # Locate first occurrence of "study_"
  end_idx <- grep("^\\s*sd\\(Residual\\)", log_text) # Locate end of table
  table_text <- log_text[start_idx:end_idx]
  table_text <- table_text[!grepl("^-{2,}", table_text)]  # Filter out lines with only dashes
  
  table_cleaned <- gsub("\\s{2,}", " ", table_text)  # Replace multiple spaces with a single space
  table_cleaned <- gsub("^\\s+|\\s+$", "", table_cleaned)  # Trim leading and trailing whitespace
  temp_file <- tempfile()
  writeLines(table_cleaned, temp_file)
  
  table_df <- read.table(
    temp_file,
    header = FALSE,
    fill = TRUE,
    stringsAsFactors = FALSE
  ) |>
    mutate(V1 = trimws(V1)) |>
    select(-V2) |>
    filter(!(V1 %in% c (
      "Random-effects", "interval]", "ID:", "|"
    ))) |>
    select(
      coef = V1,
      b = V3,
      pvalue = V6,
      ll = V7,
      ul = V8
    ) |>
    replace_coef_empty_b() |>
    filter(b != "") |>
    mutate(across(c(b, pvalue, ll, ul), as.numeric))
}

# Function to collect and replace coef values
replace_coef_empty_b <- function(data) {
  # Loop through the data frame
  i <- 1
  while (i <= nrow(data)) {
    # Check if column b is empty
    if (data$b[i] == "") {
      # Start collecting coef values
      collected_coef <- c(data$coef[i])  # Start with current coef
      j <- i + 1  # Start from the next index
      
      # Continue collecting until we find a non-empty b value
      while (j <= nrow(data)) {
        collected_coef <- c(collected_coef, data$coef[j])
        
        # Break if we find a non-empty b value
        if (data$b[j] != "") {
          # Replace coef in the first empty line
          data$coef[j] <- paste(collected_coef, collapse = " ")
          # Move i to j to continue scanning from there
          i <- j
          break
        }
        
        j <- j + 1  # Move to the next index
      }
    } else {
      # Move to the next row if b is not empty
      i <- i + 1
    }
  }
  return(data)
}


process_stata_rtable <- function(rtable,name,  n_tibble) {
  results <- rtable |>
    mutate(
      analysis_name = name,
      p_str = format_p(pvalue),
      b_str = round(b, 2) |> as.character(),
      ll_str = round(ll, 2) |> as.character(),
      ul_str = round(ul, 2) |> as.character(),
      summary = paste0(b_str, " (", ll_str, ", ", ul_str, ")", " p=", p_str),
      coef = case_when(grepl("var\\(e", coef) ~ "var(e)",
                       TRUE ~ coef)
    ) |>
    select(analysis_name, coef, summary)
  bind_rows(n_tibble, results)
  
}

format_p <- Vectorize(function(p) {
  if(is.na(p)){
    return("")
  } else  if(p > 0.06){
    return(round(p, 2) |> as.character())
  } else if(p >= 0.045){
    return(round(p, 3)|> as.character())
  } else if(p >= 0.01 ) {
    return(round(p, 2)|> as.character())
  } else if(p >= 0.001){
    return(round(p,3)|> as.character())
  }  else{
    return("<0.001")
  }
  
})


