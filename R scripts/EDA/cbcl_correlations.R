load( file = file.path(derived_data, "ssc.Rdata"))

wide_data <- make_wide_dataset(ssc_data)

wide_data |> filter(!is.na(cbcl_ext_total_2)) |> make_summary_table()

make_summary_table_by_wave(ssc_data)

wide_data |> 
  filter(!is.na(cbcl_ext_total_2)) |> make_summary_table()


check_merge(ssc_data, "age")
check_merge(ssc_data, "cbcl_ext_total")
check_merge(ssc_data, "cbcl_int_total")

complete_wide_data <- 
  wide_data |> 
  filter(!is.na(cbcl_ext_total_2)) |> 
  filter(!is.na(cbcl_ext_total_1)) 

complete_wide_data_adolescent <- complete_wide_data |> 
  filter(age_1 < 14, age_2 > 14) 

complete_wide_data_adolescent |> filter(follow_up < 6) |>  select(ID, age_1, age_2, follow_up)

cor.test(complete_wide_data_adolescent$cbcl_ext_total_1,complete_wide_data_adolescent$cbcl_ext_total_2)
cor.test(complete_wide_data_adolescent$cbcl_int_total_1,complete_wide_data_adolescent$cbcl_int_total_2)

complete_wide_data_adolescent |> make_long_dataset() |> make_summary_by_wave()

complete_wide_data_adolescent |> make_summary_table()


cor(complete_wide_data$cbcl_ext_total_1,complete_wide_data$cbcl_ext_total_2)
cor(complete_wide_data$cbcl_int_total_1,complete_wide_data$cbcl_int_total_2)

ssc_data_adolescent <-  ssc_data |> filter(age >10, age <19)
cor.test(ssc_data$cbcl_ext_total, ssc_data$cbcl_int_total)

cor.test(ssc_data_adolescent$cbcl_ext_total, ssc_data_adolescent$cbcl_int_total)