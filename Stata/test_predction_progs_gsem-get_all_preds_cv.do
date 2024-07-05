cd "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data" //. set to data and outputs 

use "pooled_vabs_long_complete.dta", clear // vabs data, fixed study


qui do "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/R/prediction_for_autistic_children/Stata/prediction_progs.do"
    

*do prediction_progs_gsem.do

*********************** Validating ***********************

use "pooled_sdq_wide.dta", clear
keep if out_all_complete  ==1 & base_all_complete ==1
count 
local N = r(N)*2 // times 5 as we run n_reps


local model  regress out_sdq_pro_p  base_sdq_pro_p  base_age study_*
get_all_preds study, ///
    model_code(`model') ///
    model_options(nocons) ///
	single_time_point ///
	cross_validation ///
	n_cv_folds(5) ///
	n_reps(2) 

su pred
local pred = r(mean) 
su actual
local actual = r(mean)
assert abs(`actual' - `pred') < 0.1

assert (`N' - r(N)) == 0

*Fixed study, Single timepoint

use "pooled_sdq_wide.dta", clear
keep if out_all_complete  ==1 & base_all_complete ==1
count
local N = r(N)*2 // times 5 as we run n_reps

local model  gsem ( out_sdq_pro_p <-  base_sdq_pro_p  base_age STUDY[study])
get_all_preds study, ///
    model_code(`model') ///
    model_options(nocons) ///
	single_time_point ///
	random_study ///
	cross_validation ///
	n_cv_folds(5) ///
	n_reps(2) 

su pred
local pred = r(mean) 
su actual
local actual = r(mean)
assert abs(`actual' - `pred') < 0.1
di `N'
di r(N)
assert (`N' - r(N)) == 0

***** Multi-timepoint
qui do "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/R/prediction_for_autistic_children/Stata/prediction_progs.do"


use "pooled_sdq.dta", clear
keep if out_all_complete  ==1 & base_all_complete ==1
count if wave == 2
local N = r(N)*2 // times 5 as we run n_reps

local model  gsem (sdq_pro_p <- STUDY[study] age M1[ID])
get_all_preds study, ///
    model_code(`model') ///
	random_study ///
	out_wave(2) ///
	predictor_waves(0 -1) ///
	cross_validation ///
	n_cv_folds(5) ///
	n_reps(2) 
	
su pred
local pred = r(mean) 
su actual
local actual = r(mean)
assert abs(`actual' - `pred') < 0.1
di `N'
di r(N)
assert (`N' - r(N)) == 0



use "pooled_sdq.dta", clear
keep if out_all_complete  ==1 & base_all_complete ==1
count if wave == 2
local N = r(N)*2 // times 5 as we run n_reps

local model  gsem (sdq_pro_p <- study_* age M1[ID])
get_all_preds study, ///
    model_code(`model') ///
	model_options(nocons) ///
	out_wave(2) ///
	predictor_waves(0 -1) ///
	cross_validation ///
	n_cv_folds(5) ///
	n_reps(2) 
	
su pred
local pred = r(mean) 
su actual
local actual = r(mean)
assert abs(`actual' - `pred') < 0.1
di `N'
di r(N)
assert (`N' - r(N)) == 0




use "pooled_sdq.dta", clear
keep if out_all_complete  ==1 & base_all_complete ==1
count if wave == 2
local N = r(N)*2 // times 5 as we run n_reps

local model  gsem (sdq_pro_p <- study_* age M1[ID] c.age#M2[ID])
get_all_preds study, ///
    model_code(`model') ///
	model_options(nocons) ///
	random_slope ///
	out_wave(2) ///
	predictor_waves(0 -1) ///
	cross_validation ///
	n_cv_folds(5) ///
	n_reps(2) 
	
su pred
local pred = r(mean) 
su actual
local actual = r(mean)
assert abs(`actual' - `pred') < 0.1
di `N'
di r(N)
assert (`N' - r(N)) == 0
