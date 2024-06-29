

use "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data/pooled_vabs_wide.dta", clear

one_hot_encode study

mkspline base_vabs_dls_spline = base_vabs_dls_ae, nknots(3) cubic

local model_code regress out_vabs_dls_ae study_* base_vabs_dls_spline* base_age out_age base_vabs_com_ae base_vabs_soc_ae base_sex
 
local model_options nocons 
`model_code', `model_options'

cap drop pred*

predict pred_raw

`model_code', `model_options'
get_all_preds_reg study, model_code(`model_code') pred_var_name("pred_est") intercept_est("estimate") model_options(`model_options')
 
`model_code', `model_options'
get_all_preds_reg study, model_code(`model_code') pred_var_name("pred0") intercept_est("value") model_options(`model_options') intercept_value(0)
 
 
`model_code', `model_options'
get_all_preds_reg study, model_code(`model_code') pred_var_name("pred_average") intercept_est("average") model_options(`model_options') intercept_value(0)
 
 
 su pred*
 
twoway (scatter out_vabs_dls_ae pred_raw) (function y = x, range(0 18)), name("raw", replace)
twoway (scatter out_vabs_dls_ae  pred_est ) (function y = x, range(0 18)), name("iecv", replace)
twoway (scatter out_vabs_dls_ae  pred_average ) (function y = x, range(0 18)), name("iecv_av", replace)

gen res_av = pred_average - out_vabs_dls_ae

scatter res_av out_age
scatter res_av base_age
scatter res_av fu_length
scatter res_av out_vabs_dls_ae


hist res_av, normal


cor out_vabs_dls_ae pred_est
*Check intercept estimateion

