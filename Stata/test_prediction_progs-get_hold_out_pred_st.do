
cd "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data" //. set to data and outputs 


use "pooled_sdq_wide.dta", clear
local model gsem (out_sdq_pro_p <- base_sdq_pro_p  base_age STUDY[study])
get_hold_out_pred_st study, ///
	hold_out_fold("Quest") ///
	model_code(`model') ///
	intercept_est(average  estimate estimate_cv) ///  estimate_cv
	random_study

su pred* 	out_sdq_pro_p

qui do "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/R/prediction_for_autistic_children/Stata/prediction_progs.do"

use "pooled_sdq_wide.dta", clear
local model  regress out_sdq_pro_p  base_sdq_pro_p  base_age study_*
get_hold_out_pred_st study, ///
	hold_out_fold("MCS") ///
	model_code(`model') ///
	model_options(nocons) ///
	intercept_est(average estimate estimate_cv)
	
su pred* 	out_sdq_pro_p



