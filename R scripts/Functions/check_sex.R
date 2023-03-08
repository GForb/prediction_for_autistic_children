check_sex <- function(data, detail = FALSE) {
  sex_data <- data[["sex"]]
  males <- sex_data |> 
    table()
  males <- males[names(males) == 1]
  females <- sex_data |> 
    table()
  females <- females[names(females) == 2]
  if(males > females){
    if(detail){
      print(paste0("There are ", males, " males, and ", females, " females in this dataset"))
    }else{
      print("There are more males than females in this dataset.")
    }
  }else{
    if(detail){
      print(paste0("This is unexpected. There are ", males, " males, and ", females, " females in this dataset"))
    }else{
      print("This is unexpected. It seems like there are more females than males in the dataset.")
    }
  }
}

#check_sex(gui_data)

#check_sex(gui_data, detail = TRUE)
