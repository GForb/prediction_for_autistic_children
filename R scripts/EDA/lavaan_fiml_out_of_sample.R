library(lavaan)

analysis_data_long <- readRDS(here(derived_data, "pooled_cbcl_spline.Rds")) |> 
  mutate(base_iq_full_scale = base_iq_full_scale/10)


data <- analysis_data_long |> filter(study != "SSC")
test_data <- analysis_data_long |> filter(study == "SSC") |> 
  select(ID, base_sex, cbcl_aff, base_iq_full_scale, base_ethnicity, age) |> 
  drop_na()
mod <- '
level:1
  cbcl_aff ~ age 
level:2
  cbcl_aff ~ base_sex + base_iq_full_scale + base_ethnicity
   I =~ cbcl_aff
'

m0 <- sem(data = data, model = mod,
          cluster = 'ID')
summary(m0) #ignoring missing

m1 <- sem(data = data, model = mod,
          missing = 'fiml', cluster = 'ID',
          fixed.x = FALSE)
summary(m1) 

varTable(m1)

m2 <- sem(data = data, model = mod,
          missing = 'fiml', cluster = 'ID',
          fixed.x = TRUE) # Fixed.x = FALSE required to use FIML with missing data
summary(m2)

lavPredict(m0, newdata = test_data, level = 2)