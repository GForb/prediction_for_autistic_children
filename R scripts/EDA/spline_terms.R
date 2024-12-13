analysis_data_long_mi <- readRDS(here(derived_data, "sdq_imputed_ml.Rds")) 


sample_data <- analysis_data_long_mi[[1]]

plot(sample_data$age_spline1, sample_data$age_spline2)

# For low ages spline 2 is close to zero
# For high ages spline 2 is more than spline 1, but linear in spline 1

# So main effect is linear 

# At high ages spline2 will have a bigger effect than spline 1

