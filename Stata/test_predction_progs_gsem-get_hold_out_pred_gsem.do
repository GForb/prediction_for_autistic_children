qui do "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/R/prediction_for_autistic_children/Stata/prediction_progs.do"

cd "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data" //. set to data and outputs 


use "pooled_sdq.dta", clear
gsem (sdq_pro_p <- study_*  age M1[ID]@1), nocons

*** Estiamte cv ***
use "pooled_sdq.dta", clear
keep if out_all_complete ==1

local model_code_uv gsem (sdq_pro_p <- study_*  age M1[ID]@1)
local model_options _nocons

get_hold_out_pred_gsem study, ///
	hold_out_fold("Quest") ///
	model_code(`model_code_uv') ///
	model_options(nocons) ///
	out_wave(2) ///
	predictor_waves(0 -1) ///
	intercept_est(estimate_cv)
	
	
	su pred if wave ==2 & study =="Quest"
	local pred = r(mean)
	su sdq_pro_p if wave ==2 & study =="Quest"
	local actual = r(mean)
	assert abs(`pred' -`actual') < 1

*** Average ***
use "pooled_sdq.dta", clear
keep if out_all_complete ==1

local model_code_uv gsem (sdq_pro_p <- study_*  age M1[ID]@1)
local model_options _nocons

get_hold_out_pred_gsem study, ///
	hold_out_fold("Quest") ///
	model_code(`model_code_uv') ///
	model_options(nocons) ///
	out_wave(2) ///Î
	predictor_waves(0 -1) ///
	intercept_est(average)
	
	su pred if wave ==2 & study =="Quest"
	local pred = r(mean)
	su sdq_pro_p if wave ==2 & study =="Quest"
	local actual = r(mean)
	assert abs(`pred' -`actual') < 1
	
*** Estiamte ***
use "pooled_sdq.dta", clear
keep if out_all_complete ==1

local model_code_uv gsem (sdq_pro_p <- study_*  age M1[ID]@1)
local model_options _nocons

get_hold_out_pred_gsem study, ///
	hold_out_fold("Quest") ///
	model_code(`model_code_uv') ///
	model_options(nocons) ///
	out_wave(2) ///
	predictor_waves(0 -1) ///
	intercept_est(estimate)
	
	
	su pred if wave ==2 & study =="Quest"
	local pred = r(mean)
	su sdq_pro_p if wave ==2 & study =="Quest"
	local actual = r(mean)
	assert abs(`pred' -`actual') < 1

*** All ***
use "pooled_sdq.dta", clear
keep if out_all_complete ==1

local model_code_uv gsem (sdq_pro_p <- study_*  age M1[ID]@1)
local model_options _nocons

get_hold_out_pred_gsem study, ///
	hold_out_fold("Quest") ///
	model_code(`model_code_uv') ///
	model_options(nocons) ///
	out_wave(2) ///
	predictor_waves(0 -1) ///
	intercept_est(estimate average estimate_cv) 

	
	
	su pred_estimate if wave ==2 & study =="Quest"
	local pred_e = r(mean)
		
	su pred_average if wave ==2 & study =="Quest"
	local pred_a = r(mean)
	su sdq_pro_p if wave ==2 & study =="Quest"
	local actual = r(mean)
	assert abs(`pred_e' -`actual') < 1
	assert abs(`pred_a' -`actual') < 1
	su pred*
	
*** All - random study ***
use "pooled_sdq.dta", clear
keep if out_all_complete ==1

local model_code_uv gsem (sdq_pro_p <- STUDY[study]  age M1[ID]@1)

get_hold_out_pred_gsem study, ///
	hold_out_fold("Quest") ///
	model_code(`model_code_uv') ///
	out_wave(2) ///
	predictor_waves(0 -1) ///
	intercept_est(estimate average estimate_cv) ///
	random_study

	
	
	su pred_estimate if wave ==2 & study =="Quest"
	local pred_e = r(mean)
		
	su pred_average if wave ==2 & study =="Quest"
	local pred_a = r(mean)
	su sdq_pro_p if wave ==2 & study =="Quest"
	local actual = r(mean)
	assert abs(`pred_e' -`actual') < 1
	assert abs(`pred_a' -`actual') < 1
	su pred*
	
 
 use "pooled_vabs_long_complete.dta", clear // vabs data, fixed study

    mkspline age_spline = age_c, nknots(3) cubic
	
	
local model_code_uv gsem (vabs_soc_ae <- study_* age_spline1 age_spline2 ///
    base_vabs_dq base_sex ///
    c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
    c.age_spline1#c.base_vabs_dq_dec c.age_spline2#c.base_vabs_dq_dec ///
    base_vabs_dls_ae base_vabs_com_ae   M1[ID]@1)

get_hold_out_pred_gsem study, ///
	hold_out_fold("EDX") ///
	model_code(`model_code_uv') ///
	model_options(nocons) ///
	out_wave(2) ///
	predictor_waves(0 -1) ///
	intercept_est(estimate) 
	

	
	
	
