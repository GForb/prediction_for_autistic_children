qui do "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/R/prediction_for_autistic_children/Stata/prediction_progs.do"

cd "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data" //. set to data and outputs 


use "pooled_sdq.dta", clear
local model gsem (sdq_pro_p <-   age STUDY[study])
get_hold_out_pred_st study, ///
	hold_out_fold("MCS") ///
	model_code(`model') ///
	intercept_est(average estimate estimate_cv) ///
	random_study
	
use "pooled_sdq.dta", clear
local model regress sdq_pro_p  study_* age )
get_hold_out_pred_st study, ///
	hold_out_fold("MCS") ///
	model_code(`model') ///
	intercept_est(average estimate estimate_cv) ///
	



