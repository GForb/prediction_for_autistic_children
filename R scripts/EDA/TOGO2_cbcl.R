data_folder <- here::here(raw_data, "TOGO")
data_cohort2  <- haven::read_sav(here::here(data_folder, "ASS_W1_W2_cohort2.zsav")) 


data_cohort1<- haven::read_sav(here::here(data_folder, "ASS_W1_W2_W3_cohort1.sav"))

############ Cohort 2 ################
# Caculating DSM scores

data_w_dsm <- data_cohort2 |> 
  mutate(
    w1_dsm_od = W1CBCL_3_AGod + W1CBCL_22_AGod + W1CBCL_23_AGod+ W1CBCL_93_AGod + W1CBCL_102_AGod,
    w2_dsm_od = W2CBCL_3_AGod + W2CBCL_22_AGod + W2CBCL_23_AGod + W2CBCL_93_AGod + W2CBCL_102_AGod,
    w1_dsm_ad = W1CBCL_4_ATad +W1CBCL_8_ATad+W1CBCL_10_ATad+W1CBCL_41_ATad+W1CBCL_85_ATad+W1CBCL_111_AGad,
    w2_dsm_ad = W2CBCL_4_ATad +W2CBCL_8_ATad+W2CBCL_10_ATad+W2CBCL_41_ATad+W2CBCL_85_ATad+W1CBCL_111_AGad,
    w1_dsm_af = W1CBCL_5_WIaf + W1CBCL_14_ANaf + W1CBCL_18_THaf + W1CBCL_35_ANaf + W1CBCL_52_ANaf + W1CBCL_54_SMaf + W1CBCL_83_THaf + W1CBCL_98_ANaf + W1CBCL_107_THaf + W1CBCL_109_WIaf + W1CBCL_110_WIaf,
    w2_dsm_af = W2CBCL_5_WIaf + W2CBCL_14_ANaf + W2CBCL_18_THaf + W2CBCL_35_ANaf + W2CBCL_52_ANaf + W2CBCL_54_SMaf + W2CBCL_83_THaf + W2CBCL_98_ANaf + W2CBCL_107_THaf + W2CBCL_109_WIaf + W2CBCL_110_WIaf,
    w1_dsm_an = W1CBCL_11_SCan + W1CBCL_29_ANan + W1CBCL_30_ANan + W1CBCL_45_ANan + W1CBCL_50_ANan + W1CBCL_119_ANan ,
    w2_dsm_an = W2CBCL_11_SCan + W2CBCL_29_ANan + W2CBCL_30_ANan + W2CBCL_45_ANan + W2CBCL_50_ANan + W2CBCL_119_ANan ,
    w1_dsm_cd = W1CBCL_16_AGcd+W1CBCL_21_AGcd+W1CBCL_26_RBcd+W1CBCL_28_RBcd+W1CBCL_37_AGcd+W1CBCL_39_RBcd+W1CBCL_43_RBcd+W1CBCL_64_AGcd+W1CBCL_74_RBcd+W1CBCL_79_RBcd+W1CBCL_88_RBcd+W1CBCL_89_RBcd+W1CBCL_97_RBcd,
    w2_dsm_cd = W2CBCL_16_AGcd+W2CBCL_21_AGcd+W2CBCL_26_RBcd+W2CBCL_28_RBcd+W2CBCL_37_AGcd+W2CBCL_39_RBcd+W2CBCL_43_RBcd+W2CBCL_64_AGcd+W2CBCL_74_RBcd+W2CBCL_79_RBcd+W2CBCL_88_RBcd+W2CBCL_89_RBcd+W2CBCL_97_RBcd,
    w1_dsm_so = W1CBCL_56_SMso+W1CBCL_57_SMso+W1CBCL_58_SMso+W1CBCL_59_SMso+W1CBCL_60_SMso+W1CBCL_61_SMso+W1CBCL_62_SMso,
    w2_dsm_so = W2CBCL_56_SMso+W2CBCL_57_SMso+W2CBCL_58_SMso+W2CBCL_59_SMso+W2CBCL_60_SMso+W2CBCL_61_SMso+W2CBCL_62_SMso
)

data_w_dsm |> 
  select(IDcode, W1age_child, starts_with("w1_dsm"),W2age_child, starts_with("w2_dsm")) |> 
  pivot_longer(cols = -IDcode, names_to = c("domain")) |> 
  group_by(domain) |> 
  summarise(n = sum(!is.na(value)),
            mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE)) |> 
  ungroup()

# Correlations anxiety and odd are negative, other domains are lower have been observed in other cohorts. (below 0.25)
cor(data_w_dsm$w2_dsm_ad, data_w_dsm$w1_dsm_ad, use = "pairwise.complete.obs")
cor(data_w_dsm$w2_dsm_af, data_w_dsm$w1_dsm_af, use = "pairwise.complete.obs")
cor(data_w_dsm$w2_dsm_an, data_w_dsm$w1_dsm_an, use = "pairwise.complete.obs")
cor(data_w_dsm$w2_dsm_cd, data_w_dsm$w1_dsm_cd, use = "pairwise.complete.obs")
cor(data_w_dsm$w2_dsm_od, data_w_dsm$w1_dsm_od, use = "pairwise.complete.obs")
cor(data_w_dsm$w2_dsm_so, data_w_dsm$w1_dsm_so, use = "pairwise.complete.obs")

############ Cohort 1 ################
# Correlations in this cohort are inline with other datastes.

data_cohort1 |> 
  select(idnr, W1age_child, W2age_child,  starts_with("W1CBCL_DSM"), starts_with("W2CBCL_DSM")) |>
  pivot_longer(cols = -idnr, names_to = c("domain")) |> 
  group_by(domain) |> 
  summarise(n = sum(!is.na(value)),
            mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE)) |> 
  ungroup()
  
# All correaltions abvove 0.4
cor(data_cohort1$W2CBCL_DSM_ad, data_cohort1$W1CBCL_DSM_ad, use = "pairwise.complete.obs")
cor(data_cohort1$W2CBCL_DSM_af, data_cohort1$W1CBCL_DSM_af, use = "pairwise.complete.obs")
cor(data_cohort1$W2CBCL_DSM_an, data_cohort1$W1CBCL_DSM_an, use = "pairwise.complete.obs")
cor(data_cohort1$W2CBCL_DSM_cd, data_cohort1$W1CBCL_DSM_cd, use = "pairwise.complete.obs")
cor(data_cohort1$W2CBCL_DSM_od, data_cohort1$W1CBCL_DSM_od, use = "pairwise.complete.obs")
cor(data_cohort1$W2CBCL_DSM_so, data_cohort1$W1CBCL_DSM_so, use = "pairwise.complete.obs")
