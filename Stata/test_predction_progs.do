clear
set seed 1234
local n_obs 4000 
set obs 4000
gen latent = rnormal(0,1)
gen shared_error = rnormal(0, 0.5)
gen study = round(_n/(`n_obs'/4))
gen study2 = round(_n/(`n_obs'/10))

gen id = _n
gen latent1 = rnormal(0,1)
gen latent2 = rnormal(0,1)
gen latent3 = rnormal(0,2)
gen rand_slope1 = latent1/4 + rnormal(0, 0.25)

expand 4
bysort id: gen time = _n
gen age = time + rnormal(0, 0.25)

gen y1 = latent  + latent1 +age*1 -study/2 + rnormal(0, 1)
gen y2 = latent  + latent2 + age*0.5 + 0.5 -study/4 + rnormal(0, 1)
gen y3 = latent  + latent3 + age*0.25 + 2 - study  + rnormal(0, 1)

tostring study, replace
tab study
qui levelsof study, local(studies)
foreach study in `studies' {
	gen study_`study' = 0
	replace study_`study' = 1 if study == "`study'"
}


*do prediction_progs.do


*Test fit_hold_out_gsem
local model_code_uv gsem (y1 <- study_*  age M1[id]@1)

gsem (y1 <- study_*  age M1[id]@1) if study !="4", nocons
fit_hold_out_gsem study, model_code(`model_code_uv') hold_out_fold(4) intercept_est("estimate") model_options(nocons)
mat A = r(table)
local se = A[2,5]
assert `se' > 0 & !missing(`se')


gsem (y1 <- study_*  age M1[id]@1) if study !="3", nocons
fit_hold_out_gsem study, model_code(`model_code_uv') hold_out_fold(3) intercept_est("estimate") model_options(nocons)
mat A = r(table)
local se = A[2,4]
assert `se' > 0 & !missing(`se')

gsem (y1 <- study_*  age M1[id]@1) if study !="0", nocons
fit_hold_out_gsem study, model_code(`model_code_uv') hold_out_fold(0) intercept_est("estimate") model_options(nocons)
mat A = r(table)
local se = A[2,1]
assert `se' > 0 & !missing(`se')


di "Hold out fold 4, average intercept"
local model_code_uv gsem (y1 <- study_*  age M1[id]@1) 
gsem (y1 <- study_*  age M1[id]@1) if study !="4", nocons
fit_hold_out_gsem study, model_code(`model_code_uv') hold_out_fold(4) intercept_est("average") model_options(nocons)
mat A = r(table)
local average = (A[1,1] + A[1,2]+A[1,3]+A[1,4])/4
local b = A[1,5]

di `average'
di `b'

assert abs(`average' - `b') < 0.01

local model_code_uv gsem (y1 <- study_*  age M1[id]@1)

gsem (y1 <- study_*  age M1[id]@1) if study != "3", nocons
fit_hold_out_gsem study, model_code(`model_code_uv') hold_out_fold(3) intercept_est("average") model_options(nocons)
mat A = r(table)
local average = (A[1,1] + A[1,2]+A[1,3]+A[1,5])/4
local b = A[1,4]

di `average'
di `b'

assert abs(`average' - `b') < 0.01


di "Hold out fold 4, intercept value 1"
gsem (y1 <- study_*  age M1[id]@1) if study != "4", nocons

fit_hold_out_gsem study, model_code(`model_code_uv') hold_out_fold(4) intercept_est("value") model_options(nocons) intercept_value(1)
mat A = r(table)
local b = A[1,5]
assert `b' ==1

*Test predict_multiwave_gsem_ri
preserve

gsem (y1 <- study_*   age M1[id]@1)
cap drop pred*
predict_multiwave_gsem_ri pred, wave_var(time) out_wave(4) predictor_waves(1 2) id_var(id)
	
	su pred*  if time ==4 & study == "0"
	local pred = r(mean)
	su y1 if time ==4  & study =="0"
	local actual = r(mean)
	assert abs(`pred' -`actual') < 0.15

restore
preserve	
	
gsem (y1 <- study_*  age M1[id]@1) (y2 <- `study_vars'  age M2[id]@1)
cap drop pred*
predict_multiwave_gsem_ri pred, wave_var(time) out_wave(4) predictor_waves(1 2) id_var(id)
forvalues i = 1 (1) 2 {
	su pred_y`i'  if time ==4 
	local pred = r(mean)
	su y`i' if time ==4  
	local actual = r(mean)
	assert abs(`pred' -`actual') < 0.1
}
restore

preserve

local model_code_uv gsem (y1 <- study_*  age M1[id]@1)

local model_options _nocons
cap drop pred*
get_hold_out_pred_gsem study, ///
	pred_var_name(pred) ///
	hold_out_fold(0) ///
	model_code(`model_code_uv') ///
	intercept_est("estimate") ///
	model_options(nocons) ///
	predict_function(predict_multiwave_gsem_ri) ///
	predict_args(wave_var(time) ///
	out_wave(4) ///
	predictor_waves(1 2) ///
	id_var(id))
	
	su pred*  if time ==4 & study =="0"
	local pred = r(mean)
	su y1 if time ==4  & study =="0"
	local actual = r(mean)
	assert abs(`pred' -`actual') < 0.1
restore
	
	
	preserve
*Test get_all_preds_gsems
local model_code_uv gsem (y1 <- study_*  age M1[id]@1)
local model_options _nocons
cap drop pred*
get_all_preds_gsem study, ///
	pred_var_name(pred) ///
	model_code(`model_code_uv') ///
	intercept_est("estimate") ///
	model_options(nocons) ///
	predict_function(predict_multiwave_gsem_ri) ///
	predict_args(wave_var(time) ///
	out_wave(4) ///
	predictor_waves(1 2) ///
	id_var(id))
	
	su pred*  if time ==4 
	local pred = r(mean)
	su y1 if time ==4  
	local actual = r(mean)
	assert abs(`pred' -`actual') < 0.1
	
	
* With simple model
preserve
local model_code_uv gsem (y1 <- study_*  age M1[id]@1)
local model_options nocons
cap drop pred*
get_all_preds_gsem study, ///
	pred_var_name(pred) ///
	model_code(`model_code_uv') ///
	intercept_est("estimate") ///
	model_options(nocons) ///
	predict_function(predict_multiwave_gsem_ri) ///
	predict_args(wave_var(time) ///
	out_wave(4) ///
	predictor_waves(1 2) ///
	id_var(id)) ///
	simple_model(`model_code_uv') ///
	simple_model_options( nocons var(M1[id]@0.01))
	
	su pred*  if time ==4 
	local pred = r(mean)
	su y1 if time ==4  
	local actual = r(mean)
	assert abs(`pred' -`actual') < 0.1
restore

* With random study
preserve
local model_code_uv_rs gsem (y1 <-   age STUDY[study] M1[id]@1)
local model_options nocons
cap drop pred*
get_all_preds_gsem study, ///
	pred_var_name(pred) ///
	model_code(`model_code_uv_rs') ///
	intercept_est("estimate") ///
	model_options(nocons) ///
	predict_function(predict_multiwave_gsem_ri) ///
	predict_args(wave_var(time) ///
	out_wave(4) ///
	predictor_waves(1 2) ///
	id_var(id)) ///
	random_study ///
	simple_model(`model_code_uv_rs') ///
	simple_model_options( nocons var(M1[id]@0.01))
	
	su pred*  if time ==4 
	local pred = r(mean)
	su y1 if time ==4  
	local actual = r(mean)
	assert abs(`pred' -`actual') < 0.1
restore
