library(lme4)
data_folder <-  here::here(derived_data, "SDQ_gen_pop")

train_data <- readRDS(file = here(data_folder, "sdq_gen_pop_non_autistic1000.rds"))
test_data <- readRDS(file = here(data_folder, "sdq_gen_pop_non_autistic_test.rds"))


test_data_cc <- test_data |> 
  select(ID, studyid, wave, out_wave, age, starts_with("base"), starts_with("sdq"), -base_sex) |> na.omit() |> data.frame()

train_data_stp  <-  train_data |> filter(wave == out_wave)
test_data_cc_stp <- test_data_cc |> filter(wave == out_wave)


model = lme4::lmer("sdq_emot_p ~ age +studyid  +  base_sdq_cond_p + base_sdq_hyp_p + base_sdq_peer_p + base_sdq_pro_p  + (1|ID)", 
                   data = train_data, REML = TRUE)

simulate(model, newdata = test_data_cc_stp,  allow.new.levels = TRUE)[1,1]
simulate(model, newdata = test_data_cc_stp,  allow.new.levels = TRUE, re.form = NA)[1,1]
#I don't think so. If you have new levels simulate() produces 'unconditional' predictions - setting the random effects to zero

library(RStata)

data_folder <-  here::here(derived_data, "SDQ_gen_pop")

test_data <- readRDS(file = here(data_folder, "sdq_gen_pop_non_autistic_test.rds"))
train_data <- readRDS(file = here(data_folder, "sdq_gen_pop_non_autistic1000.rds"))

test_data <- test_data |> mutate(test = 1)
train_data <- train_data |> mutate(train = 1)
combined_data <- bind_rows(test_data, train_data)

reg_pred <- stata("regress sdq_emot_p   age_cent  base_sdq_emot_p   base_sdq_cond_p  base_sdq_hyp_p  base_sdq_peer_p base_sdq_pro_p base_age_cent if relative_wave ==1 & train_data
      predict pred_regress if test_data
      ",  data.in = combined_data, data.out = TRUE)

reg_pred |> select(sdq_emot_p, pred_regress)


stata("est store A")