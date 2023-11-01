library(lme4)

data <- readRDS(file = here(derived_data, "sdq_gen_pop_non_autistic1000.rds"))

# Processing test data
test_data <- readRDS(file = here(derived_data, "sdq_gen_pop_non_autistic.rds"))
test_data <- test_data |> filter(non_autistic_eda_sample1000 !=1 | is.na(non_autistic_eda_sample1000))

base_test_data <- test_data |> filter(
  (wave == 2 & study  ==  "gui") |
    (wave  ==  3 & study  ==  "mcs") | 
    (wave  ==  7 & study  ==  "lsac_b") | 
    (wave  ==  5  & study  ==  "lsac_k")
)

out_test_data <- test_data |> filter(
  (wave == 3 & study  ==  "gui") |
    (wave  ==  4 & study  ==  "mcs") | 
    (wave  ==  8 & study  ==  "lsac_b") | 
    (wave  ==  6  & study  ==  "lsac_k")
) |> 
  select(ID, study, age, starts_with("sdq")) |> 
  rename_with(
    ~ paste0("y_", .x, recycle0 = TRUE),
    starts_with("sdq")
  ) |> 
  rename(y_age = age)

model_test_data <- base_test_data |> full_join(out_test_data) |> 
  mutate(mcs = case_when(study == "mcs" ~ 1,
                         TRUE ~ 0),
         gui = case_when(study == "gui" ~ 1,
                         TRUE ~ 0),
         lsac_k = case_when(study == "lsac_k" ~ 1,
                            TRUE ~ 0) ,
         lsac_b = case_when(study == "lsac_b" ~ 1,
                            TRUE ~ 0) )


data$sdq_emot_p |> hist(breaks = 10)

data$sdq_hyp_p |> hist(breaks = 10)

data$sdq_cond_p |> hist(breaks = 10)

data$sdq_pro_p |> hist(breaks = 10)
data$sdq_peer_p |> hist(breaks = 10)

mean(data$sdq_emot_p, na.rm = TRUE)
var(data$sdq_emot_p, na.rm = TRUE)

summarise_age <- function(data) {
  data |>  summarise(mean_age = mean(age, na.rm = TRUE), 
                     min_age = min(age, na.rm = TRUE),
                     max_age = max(age, na.rm = TRUE),
                     n = sum(!is.na(age))) |> 
    print(n = 30)

} 

data  |> group_by(study, wave) |> summarise_age()

studies = c("gui", "mcs", "lsac_b", "lsac_k")
model_ages <- tibble(study = studies, base_wave = c(2, 3, 7, 5)) |> mutate(out_wave = base_wave + 1)

base_data <- data |> filter(
  (wave == 2 & study  ==  "gui") |
  (wave  ==  3 & study  ==  "mcs") | 
  (wave  ==  7 & study  ==  "lsac_b") | 
  (wave  ==  5  & study  ==  "lsac_k")
)

out_data <- data |> filter(
  (wave == 3 & study  ==  "gui") |
    (wave  ==  4 & study  ==  "mcs") | 
    (wave  ==  8 & study  ==  "lsac_b") | 
    (wave  ==  6  & study  ==  "lsac_k")
) |> 
  select(ID, study, age, starts_with("sdq")) |> 
  rename_with(
    ~ paste0("y_", .x, recycle0 = TRUE),
    starts_with("sdq")
  ) |> 
  rename(y_age = age)

model_data <- base_data |> full_join(out_data) |> 
  mutate(mcs = case_when(study == "mcs" ~ 1,
                         TRUE ~ 0),
         gui = case_when(study == "gui" ~ 1,
                         TRUE ~ 0),
         lsac_k = case_when(study == "lsac_k" ~ 1,
                                         TRUE ~ 0) ,
         lsac_b = case_when(study == "lsac_b" ~ 1,
                            TRUE ~ 0) )


base_data |> group_by(study) |> summarise_age()
out_data |> group_by(study) |> summarise_age() 

model_data |> group_by(study) |> summarise_age() 

base_data |> select(starts_with("sdq")) |> cor(use = "pairwise.complete.obs")
base_data |> select(starts_with("sdq")) |> cor(use = "pairwise.complete.obs", method = "spearman")
base_data |> select(starts_with("sdq")) |> cor(use = "pairwise.complete.obs", method = "kendall")

cor(model_data$sdq_emot_p, model_data$y_sdq_emot_p, use = "pairwise.complete.obs")


model <- lme4::lmer("y_sdq_emot_p ~ age  +sdq_emot_p +  sdq_cond_p + sdq_hyp_p + sdq_peer_p + sdq_pro_p + y_age + (1|study)", 
                    data = model_data, REML = FALSE)

model2 = lme4::glmer("y_sdq_emot_p ~ age  +sdq_emot_p +  sdq_cond_p + sdq_hyp_p + sdq_peer_p + sdq_pro_p + y_age + (1|study)", 
                     data = model_data, family = "poisson")
model3 = lm("y_sdq_emot_p ~ study + age  +sdq_emot_p +  sdq_cond_p + sdq_hyp_p + sdq_peer_p + sdq_pro_p + y_age", data = model_data)
model4 = glm("y_sdq_emot_p ~ study + age  +sdq_emot_p +  sdq_cond_p + sdq_hyp_p + sdq_peer_p + sdq_pro_p + y_age", data = model_data, family = "poisson")

summary(model)
summary(model2)
summary(model3)
summary(model4)

ProfacSims:::evaluate_performance_continuous(model_test_data, model3)