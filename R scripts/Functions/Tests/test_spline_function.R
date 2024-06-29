vabs_wide_data <- readRDS(here(derived_data, "pooled_vabs_wide.Rds"))

my_spline <- make_rcs(vabs_wide_data, "base_vabs_dls_ae")


spline_command <- "mkspline base_spline = base_vabs_dls_ae, cubic nknots(3)  displayknots"
stata_spline <- RStata::stata(spline_command, data.in = vabs_wide_data, data.out = TRUE)

stata_spline |> select(base_vabs_dls_ae, starts_with("base_spline")) |> bind_cols(my_spline) |> arrange(base_vabs_dls_ae)

quantile(vabs_wide_data$base_vabs_dls_ae, probs = c(0.1, 0.90), na.rm = TRUE, type = 1)
quantile(vabs_wide_data$base_vabs_dls_ae, probs = c(0.1, 0.90), na.rm = TRUE, type = 2)
quantile(vabs_wide_data$base_vabs_dls_ae, probs = c(0.1, 0.90), na.rm = TRUE, type = 3)
quantile(vabs_wide_data$base_vabs_dls_ae, probs = c(0.1, 0.90), na.rm = TRUE, type = 4)
quantile(vabs_wide_data$base_vabs_dls_ae, probs = c(0.1, 0.90), na.rm = TRUE, type = 5)
quantile(vabs_wide_data$base_vabs_dls_ae, probs = c(0.1, 0.90), na.rm = TRUE, type = 6)
quantile(vabs_wide_data$base_vabs_dls_ae, probs = c(0.1, 0.90), na.rm = TRUE, type = 7)
quantile(vabs_wide_data$base_vabs_dls_ae, probs = c(0.1, 0.90), na.rm = TRUE, type = 8)
quantile(vabs_wide_data$base_vabs_dls_ae, probs = c(0.1, 0.90), na.rm = TRUE, type = 9)

model_rms <- rms::ols(out_vabs_dls_ae ~ rms::rcs(base_vabs_dls_ae,3), data=vabs_wide_data)
model_lm_stata <- lm("out_vabs_dls_ae ~ base_spline1 + base_spline2", data=stata_spline)