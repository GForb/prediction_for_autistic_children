qui do "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/R/prediction_for_autistic_children/Stata/prediction_progs_gsem.do"

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
local model_options _nocons

get_hold_out_pred_gsem study, ///
	hold_out_fold("Quest") ///
	model_code(`model_code_uv') ///
	model_options(nocons) ///
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
	
 
	
