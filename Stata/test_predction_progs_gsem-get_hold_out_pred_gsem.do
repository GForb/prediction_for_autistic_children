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


*do prediction_progs_gsem.do



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
	
