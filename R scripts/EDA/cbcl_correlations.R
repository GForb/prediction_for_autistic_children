load( file = file.path(derived_data, "ssc.Rdata"))

wide_data <- make_wide_dataset(ssc_data)

wide_data |> filter(!is.na(cbcl_ext_total_2)) |> make_summary_table()

make_summary_table_by_wave(ssc_data)

wide_data |> filter(!is.na(cbcl_ext_total_2)) |> make_summary_table()
