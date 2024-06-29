data_folder <- here::here(raw_data, "TEDS")
colnames(data)
data <- readRDS(here::here(data_folder, "srs_data.rds"))

# missing counts
miss_counts <- colSums(is.na(data))

miss_table <- tibble(variable = names(miss_counts), missing = miss_counts) |> 
  mutate(prop_missing = missing/nrow(data))


miss_table |> print(n = 30)

data_autistic <- data |> filter(group_splitHTandDx ==1)
miss_counts_autistic = colSums(is.na(data_autistic))
miss_table_autistic <- miss_table <- tibble(variable = names(miss_counts_autistic), missing = miss_counts_autistic) |> 
  mutate(prop_missing = missing/nrow(data))



miss_table_autistic |> print(n = 30)

# Sex
data |> count(Sex)

# Age

summary(data$Age_at_assessment_years_SRS1_SDQ)
hist(data$Age_at_assessment_years_SRS1_SDQ)

# IQ
data|> count(standard_IQ)

summary(data$FSIQ)
summary(data[data$standard_IQ == 1, "FSIQ"])
summary(data[data$standard_IQ == 0, "FSIQ"])

data |> filter(standard_IQ ==0, FSIQ > 100)

hist(data$FSIQ)

#ADOS

summary(data$ados_css_rrb)
hist(data$ados_css_rrb)

summary(data$ados_css_sa)
hist(data$ados_css_sa)

hist(data_autistic$ados_css_rrb)
hist(data_autistic$ados_css_sa)


# SDQ
summary(data$SDQ_emotional)
hist(data$SDQ_emotional)

summary(data$SDQ_conduct)
hist(data$SDQ_conduct)

summary(data$SDQ_hyperactivity)
hist(data$SDQ_hyperactivity)

summary(data$SDQ_peer)
hist(data$SDQ_peer)

summary(data$SDQ_prosocial)
hist(data$SDQ_prosocial)

summary(data$SDQ_total)
hist(data$SDQ_total)