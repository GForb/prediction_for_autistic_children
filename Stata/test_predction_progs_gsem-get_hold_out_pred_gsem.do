qui do "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/R/prediction_for_autistic_children/Stata/prediction_progs_gsem.do"

cd "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data" //. set to data and outputs 


*** Average ***
use "pooled_sdq.dta", clear
keep if out_all_complete ==1

local model_code_uv gsem (sdq_pro_p <- study_*  age M1[ID]@1)
local model_options _nocons

get_hold_out_pred_gsem study, ///
	pred_var_name(pred) ///
	hold_out_fold("MCS") ///
	model_code(`model_code_uv') ///
	intercept_est("average") ///
	model_options(nocons) ///
	predict_function(predict_multiwave_gsem_uv) ///
	predict_args( ///
		wave_var(wave) ///
		out_wave(2) ///
		predictor_waves(0 -1) ///
		id_var(ID) ///
	)
	
	su pred if wave ==2 & study =="MCS"
	local pred = r(mean)
	su sdq_pro_p if wave ==2 & study =="MCS"
	local actual = r(mean)
	assert abs(`pred' -`actual') < 0.5
	
*** Estiamte ***
use "pooled_sdq.dta", clear
keep if out_all_complete ==1

local model_code_uv gsem (sdq_pro_p <- study_*  age M1[ID]@1)
local model_options _nocons

get_hold_out_pred_gsem study, ///
	pred_var_name(pred) ///
	hold_out_fold("MCS") ///
	model_code(`model_code_uv') ///
	intercept_est("estimate") ///
	model_options(nocons) ///
	predict_function(predict_multiwave_gsem_uv) ///
	predict_args( ///
		wave_var(wave) ///
		out_wave(2) ///
		predictor_waves(0 -1) ///
		id_var(ID) ///
	)
	
	su pred if wave ==2 & study =="MCS"
	local pred = r(mean)
	su sdq_pro_p if wave ==2 & study =="MCS"
	local actual = r(mean)
	assert abs(`pred' -`actual') < 1

	


	
