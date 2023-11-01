library(lme4)



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

data  |> group_by(studyid, wave) |> summarise_age()


base_data <- data |> filter(
  (wave == 2 & studyid  ==  "gui") |
  (wave  ==  3 & studyid  ==  "mcs") | 
  (wave  ==  7 & studyid  ==  "lsac_b") | 
  (wave  ==  5  & studyid  ==  "lsac_k")
)

out_data_orig <- data |> filter(
  (wave == 3 & studyid  ==  "gui") |
    (wave  ==  4 & studyid  ==  "mcs") | 
    (wave  ==  8 & studyid  ==  "lsac_b") | 
    (wave  ==  6  & studyid  ==  "lsac_k")
) |> 
  select(ID, studyid, age, starts_with("sdq")) 
out_data <- out_data_orig |> 
  rename_with(
    ~ paste0("y_", .x, recycle0 = TRUE),
    starts_with("sdq")
  ) |> 
  rename(y_age = age)

model_data <- base_data |> full_join(out_data) |> 
  mutate(mcs = case_when(studyid == "mcs" ~ 1,
                         TRUE ~ 0),
         gui = case_when(studyid == "gui" ~ 1,
                         TRUE ~ 0),
         lsac_k = case_when(studyid == "lsac_k" ~ 1,
                                         TRUE ~ 0) ,
         lsac_b = case_when(studyid == "lsac_b" ~ 1,
                            TRUE ~ 0) )


base_data |> group_by(studyid) |> summarise_age()
out_data_orig |> group_by(studyid) |> summarise_age() 

model_data |> group_by(studyid) |> summarise_age() 

base_data |> select(starts_with("sdq")) |> cor(use = "pairwise.complete.obs")
base_data |> select(starts_with("sdq")) |> cor(use = "pairwise.complete.obs", method = "spearman")
base_data |> select(starts_with("sdq")) |> cor(use = "pairwise.complete.obs", method = "kendall")

cor(model_data$sdq_emot_p, model_data$y_sdq_emot_p, use = "pairwise.complete.obs")


model <- lme4::lmer("y_sdq_emot_p ~ age  +sdq_emot_p +  sdq_cond_p + sdq_hyp_p + sdq_peer_p + sdq_pro_p + y_age + (1|studyid)", 
                    data = model_data, REML = FALSE)

model2 = lme4::glmer("y_sdq_emot_p ~ age  +sdq_emot_p +  sdq_cond_p + sdq_hyp_p + sdq_peer_p + sdq_pro_p + y_age + (1|studyid)", 
                     data = model_data, family = "poisson")
model3 = lm("y_sdq_emot_p ~ studyid + age  +sdq_emot_p +  sdq_cond_p + sdq_hyp_p + sdq_peer_p + sdq_pro_p + y_age", data = model_data)
model4 = glm("y_sdq_emot_p ~ studyid + age  +sdq_emot_p +  sdq_cond_p + sdq_hyp_p + sdq_peer_p + sdq_pro_p + y_age", data = model_data, family = "poisson")

summary(model)
summary(model2)
summary(model3)
summary(model4)

extractAIC(model)
aictab(cand.set = list(model))
extractAIC(model2)
aictab(cand.set = list(model2))

extractAIC(model3)
aictab(cand.set = list(model3))
extractAIC(model4)
aictab(cand.set = list(model4))



model_test_data_cc <- model_test_data |> 
  select(ID, studyid, wave, age, starts_with("y"), starts_with("sdq")) |> na.omit() |> data.frame()

  
ProfacSims:::evaluate_performance_continuous(data.frame(model_test_data_cc), model3)
ProfacSims:::evaluate_performance_continuous(data.frame(model_test_data_cc), model4)

ProfacSims:::get_performance_by_study(test_data = model_test_data_cc, model = model3, evaluate_performance = ProfacSims:::evaluate_performance_continuous)

ProfacSims:::ipdma_prediction_pipeline(test_data = model_test_data_cc, 
                                       model = model3, 
                                       evaluate_performance = ProfacSims:::evaluate_performance_continuous)

ProfacSims:::ipdma_prediction_pipeline(test_data = model_test_data_cc, 
                                       model = model, 
                                       evaluate_performance = ProfacSims:::evaluate_performance_continuous)


pred3 <- predict(model3, newdata = model_test_data_cc)
pred4 <- predict(model4, newdata = model_test_data_cc, type = 'response')

preds <- tibble(lm = pred3, poisson = pred4, actual = model_test_data_cc$y_sdq_emot_p)

preds <- preds |> mutate(res_lm = lm - actual, res_poisson = poisson -actual, sq_error_lm = res_lm^2, sq_error_p = res_poisson^2)

mean(preds$res_poisson)
mean(preds$res_lm)

mean(preds$sq_error_p)
mean(preds$sq_error_lm)


hist(preds$res_lm)
plot(preds$actual, preds$poisson, ylim = c(0,15))
plot(preds$actual, preds$lm, ylim = c(0,15))

hist_with_norm <- function(vector, ...) {
  m<-mean(vector)
  std<-sqrt(var(vector))
  hist(vector, prob=TRUE, ...)
  curve(dnorm(x, mean=m, sd=std), 
        col="darkblue", lwd=2, add=TRUE, yaxt="n")
}
hist_with_norm(preds$res_lm, main = "Residuals from linear model")
hist(model_data$y_sdq_emot_p, main = "Observed outcome", prob=TRUE)

calibration_plot <- function(actual, pred, ylim= c(0,15), xlab = "Predicted", ylab = "Actual", main = "Calibration Plot",  ...) {
  plot(pred, actual, ylim =ylim, xlab = xlab, ylab = ylab, main = main, ...)
  lines(lowess(pred, actual), col='red')
  abline(a=0, b=1)
}

calibration_plot(actual = preds$actual, pred = preds$poisson, ylim = c(0, 15), xlim = c(0,15), main = "Calibration:Poisson GLM")
calibration_plot(actual = preds$actual, pred = preds$lm, ylim = c(0, 15), xlim = c(0,15), main = "Calibration:Linear Regression")

# Missing data 
plot(preds$poisson, preds$actual, ylim = c(0,15))
lines(lowess(preds$poisson, preds$actual), col='red')
abline(a=0, b=1)



# Missing data JOMO

data_missing <- model_data |> mutate(sdq_cond_p = case_when(studyid == "lsac_b" ~ NA,
                                                      TRUE ~sdq_cond_p))

data_missing |> filter(studyid == "lsac_b")


