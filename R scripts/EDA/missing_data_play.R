source(here::here("R scripts/config.R"))
library(lme4)
library(MASS)
library(jomo)
library(jomo)

select <-  dplyr::select

data_folder <-  here::here(derived_data, "SDQ_gen_pop")

combined_data <- readRDS(file = here(data_folder, "sdq_gen_pop_non_autistic.rds")) |> 
  mutate(base_sex = factor(base_sex))
combined_data_cc <- combined_data |>  select(ID, studyid, wave, out_wave, age, starts_with("base"), starts_with("sdq"), train_data, test_data, relative_wave) |> 
  na.omit() |> 
  data.frame()

combined_data_pc <- combined_data_cc |> mutate(base_sex = case_when(studyid == "mcs" ~ NA,
                                                                    TRUE ~ base_sex),
                                               base_sdq_cond_p = case_when(studyid == "mcs" ~ NA,
                                                                           TRUE ~ base_sdq_cond_p))


studyid <- combined_data_pc$studyid
Y <- combined_data_pc |> select(base_sdq_cond_p, base_sex)
combined_data_pc$cons <- 1
X <- combined_data_pc |> select(cons, sdq_emot_p, age, base_age, base_sdq_emot_p, base_sdq_hyp_p, base_sdq_peer_p, base_sdq_pro_p, age)
set.seed(1569)

# Follow the algorithm proposed in R-journal jomo paper to generate impuations

# run jomo.MCMCchain to register first imputation
imp1 <- jomo.MCMCchain(Y = Y, X = X, clus = studyid) 

# Capture the state of the sampler as starting values for the second set of iterations:
beta.start <- imp1$collectbeta[,,1000]   # capture the fixed parameter values
l1cov.start <- imp1$collectomega[,,1000] # capture the level-1 covariance matrix values
start.imp <- imp1$finimp.latnorm         # capture the final imputed data set (with
# latent normals for categorical variables)
imp2 <- jomo.MCMCchain(Y = Y, X = X, clus =studyid, nburn = 10000)

plot(imp2$collectbeta[1, 1, 1:10000], type = "l", ylab = expression(beta["e,0"]),
     xlab = "Iteration number" )


plot(imp2$collectomega[1, 1, 1:10000], type = "l", ylab = expression(omega[e,1,1]^2),
     xlab = "Iteration number")

imp3 <- jomo(Y = Y, X = X, nburn = 2000,  clus =studyid, nbetween = 1000, nimp = 10)




jomo_imp_object <- imp3


list_of_imputed_datasets <- split(jomo_imp_object, jomo_imp_object$Imputation)[-1]


list_of_imputed_datasets
sdq_lm <- function(data) {
  lm("sdq_emot_p ~ studyid + age  +base_age  +base_sdq_emot_p +  base_sdq_cond_p + base_sdq_hyp_p + base_sdq_peer_p + base_sdq_pro_p + base_sex + age", data = data)
}
  evaluate_imputations_MI_Val(imp3, 
                            model_function = sdq_lm, 
                            evaluate_performance =  evaluate_continuous, 
                            orig_data = combined_data_pc)