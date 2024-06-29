data_folder <- here::here(raw_data, "TEDS")

ados_raw_data <- readRDS(here::here(data_folder, "ados_data.rds"))

data_raw <- haven::read_sav(here::here(data_folder, "Data For Gordon - ADOS, SDQ, IQ, CAST.sav"))


data <- data_raw |> left_join(ados_raw_data, by = "Twin_ID")

data$Age_at_assessment_years_SRS1_SDQ |> summary()

data$ados_rrb_raw |> summary()
data$SRS1_ADOS_overall_CSS |> summary()
data |> filter(is.na(SRS1_ADOS_overall_CSS)) |> select(SRS1_ADOS_overall_CSS, ados_rrb_raw) |> print(n = 50)

data <- data |> 
  mutate(ados_css_rrb = calculate_css_rrb(ados_rrb_raw, module = module, words = words),
         ados_css_sa = calculate_css_sa(ados_sa_raw, module = module, words = words)) |> 
  filter(Family_ID.x < 100000, !is.na(Twin_ID)) |> 
  select(Family_ID = Family_ID.x, everything(), -Family_ID.y) |> 
  mutate(standard_IQ = case_when(IQ_Source == "" ~ 1,
                                 IQ_Source != "" ~ 0))

data |> select(Family_ID, Twin_ID, SRS1_ADOS_overall_CSS, ados_rrb_raw, ados_css_rrb, ados_sa_raw, ados_css_sa) |> print(n = 200)

colnames(data)

sum(!is.na(data$SRS1_ADOS_overall_CSS))
sum(!is.na(data$ados_css_sa))
sum(!is.na(data$ados_css_rrb))

write_csv(data, here::here(data_folder, "srs_data.csv"))
saveRDS(data, here::here(data_folder, "srs_data.rds"))

