

howlin_rutter <- haven::read_sav(file.path(raw_data, "Howlin-Rutter/Gordon Anon f-up data .sav"))

howlin_rutter <- howlin_rutter |> 
  rename(ID = Case, 
         age_at_testing = critage,
         age_follow_up = testage, 
         iq_performance_c = cpiq, 
         iq_verbal_c = cviq, 
         language_c = langrat, 
         iq_performance_a = fupiq,
         iq_verbal_a = viqest, 
         iq_full_scale_a = bestfina,
         work_type = wktype, 
         qual_of_relations = qualfrs,
         place_of_edu = edplmaj,
         high_edu_lev = highed, 
         adi_a_a = totlang,
         adi_b_a = totablan, 
         adi_c_a = totrits)

howlin_rutter_childhood <- howlin_rutter |> 
  select(ID, sex, age_at_testing, iq_performance_c, iq_verbal_c, language_c) |> 
  rename(age = age_at_testing, 
         iq_performance = iq_performance_c, 
         iq_verbal = iq_verbal_c)

howlin_rutter_adulthood <- howlin_rutter |> 
  select(ID, sex, age_follow_up, iq_performance_a, iq_verbal_a, iq_full_scale_a, work_type, work, friends, qual_of_relations, living, place_of_edu, high_edu_lev, adi_a_a, adi_b_a, adi_c_a) |> 
  rename(age = age_follow_up, 
         iq_perforamnce = iq_performance_a,
         iq_verbal = iq_verbal_a, 
         iq_full_scle= iq_full_scale_a)

howlin_rutter_childhood$wave <- 1
howlin_rutter_adulthood$wave <- 2

howlin_rutter_childhood$age_at_testing <- howlin_rutter_childhood$age/12
howlin_rutter_adulthood$age_follow_up <- howlin_rutter_adulthood$age/12

howlin_rutter_data <- bind_rows(howlin_rutter_childhood, howlin_rutter_adulthood)

howlin_rutter_data$study <- "Howlin-Rutter"

howlin_rutter_data <- howlin_rutter_data |> 
  arrange(ID)

check_values(howlin_rutter_data)

save(howlin_rutter_data, file = file.path(derived_data, "howlin-rutter.Rdata"))

save(howlin_rutter_data, file = file.path(derived_data, "howlin_rutter.Rdata"))