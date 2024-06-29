
get_age_range_data_cbcl <- function(wave1_data, wave2_data) {
  get_age_range_data(wave1_data, wave2_data, check_base = cbcl_check_base_age, check_out = cbcl_check_out_age)
}


get_age_range_data_vabs <- function(wave1_data, wave2_data) {
  get_age_range_data(wave1_data, wave2_data, check_base = vabs_check_base_age, check_out = vabs_check_out_age)
}

get_age_range_data_sdq <- function(wave1_data, wave2_data) {
  get_age_range_data(wave1_data, wave2_data, check_base = sdq_check_base_age, check_out = sdq_check_out_age)
}

cbcl_check_out_age <- Vectorize(function(age) {
  check_age(age, 12, 17)
})


cbcl_check_base_age <- Vectorize(function(age) {
  check_age(age, 8, 13)
})


vabs_check_out_age <- Vectorize(function(age) {
  check_age(age, 12, 18)
})

vabs_check_base_age <- Vectorize(function(age) {
  check_age(age, 7, 14)
})

sdq_check_out_age <- Vectorize(function(age) {
  check_age(age, 13, 18)
})


sdq_check_base_age <- Vectorize(function(age) {
  check_age(age, 9.8, 15)
})


check_age <- function(age, min_age, max_age) {
  if(is_character(age)){
    if(age == "max"){
      return(max_age)
    } else if(age == "min"){
      return(min_age)
    }
  } else if(!is.na(age)) {
    return(ifelse(age >=min_age & age <max_age,1, 0))
  } else {
    return(0)
  }
}
  

get_age_range_data <- function(wave1_data, wave2_data, check_base, check_out) {
  age_range <- full_join(wave1_data |> select(ID, age1 = age, any_of("autism")), wave2_data |> select(ID, age2 = age), by = "ID") |> 
    mutate(fu_length = age2 - age1, 
           base_in_range = check_base(age1),
           out_in_range = check_out(age2),
           out_recorded = case_when(is.na(age2)  ~ 0,
                                    !is.na(age2)~ 1),
           include = case_when(base_in_range ==1 & out_in_range ==1 & fu_length >= 2~ "include",
                               base_in_range ==1 & out_in_range ==1 & fu_length <2 ~ "ineligible follow up length",
                               base_in_range == 0 ~ "no eligible baseline data",
                               base_in_range == 1 & out_recorded ==0  ~ "eligible baseline, no followup",
                               base_in_range == 1 & out_in_range == 0 & out_recorded ==1 ~ "eligible baseline, no followup in range",
                               is.na(base_in_range) ~ "no eligible baseline data")
    )
  return(age_range)
}
