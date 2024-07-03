qui do "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/R/prediction_for_autistic_children/Stata/prediction_progs_gsem.do"


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


*Fixed study, random intercept
	
	
gsem (y1 <- study_*  age M1[id]@1)
cap drop pred*

preserve
predict_multiwave_gsem_uv pred, wave_var(time) out_wave(4) predictor_waves(1 2) id_var(id)
forvalues i = 1 (1) 2 {
	su pred  if time ==4 
	local pred = r(mean)
	su y1 if time ==4  
	local actual = r(mean)
	assert abs(`pred' -`actual') < 0.1
}
restore

*Radnom study, random intercept
preserve	
	
gsem (y1 <- study_*  age M1[id]@1)
cap drop pred*
predict_multiwave_gsem_uv pred, wave_var(time) out_wave(4) predictor_waves(1 2) id_var(id)
forvalues i = 1 (1) 2 {
	su pred_y`i'  if time ==4 
	local pred = r(mean)
	su y`i' if time ==4  
	local actual = r(mean)
	assert abs(`pred' -`actual') < 0.1
}
restore


*Fixed study, random slope
preserve	
	
gsem (y1 <- study_*  age M1[id]@1)
cap drop pred*
predict_multiwave_gsem_uv pred, wave_var(time) out_wave(4) predictor_waves(1 2) id_var(id)
forvalues i = 1 (1) 2 {
	su pred_y`i'  if time ==4 
	local pred = r(mean)
	su y`i' if time ==4  
	local actual = r(mean)
	assert abs(`pred' -`actual') < 0.1
}
restore
