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

preserve
gsem (y1 <- study_*  age M1[id]@1) if study !="4", nocons
fit_hold_out_gsem study, model_code(`model_code_uv') hold_out_fold(4) intercept_est("estimate") model_options(nocons)
mat A = r(table)
local se = A[2,5]
assert `se' > 0 & !missing(`se')
restore

preserve
gsem (y1 <- study_*  age M1[id]@1) if study !="3", nocons
fit_hold_out_gsem study, model_code(`model_code_uv') hold_out_fold(3) intercept_est("estimate") model_options(nocons)
mat A = r(table)
local se = A[2,4]
assert `se' > 0 & !missing(`se')
restore

preserve
gsem (y1 <- study_*  age M1[id]@1) if study !="0", nocons
fit_hold_out_gsem study, model_code(`model_code_uv') hold_out_fold(0) intercept_est("estimate") model_options(nocons)
mat A = r(table)
local se = A[2,1]
assert `se' > 0 & !missing(`se')
restore

preserve
di "Hold out fold 4, average intercept"
local model_code_uv gsem (y1 <- study_*  age M1[id]@1) 
gsem (y1 <- study_*  age M1[id]@1) if study !="4", nocons
fit_hold_out_gsem study, model_code(`model_code_uv') hold_out_fold(4) intercept_est("average") model_options(nocons)
mat A = r(table)
local b = A[1,5]

gsem (y1 <- study_*  age M1[id]@1) if study !="4", nocons
mat B = r(table)
local average = (B[1,1]*1/(B[2,1])^2 + B[1,2]*1/(B[2,2])^2+B[1,3]*1/(B[2,3])^2+B[1,4]*1/(B[2,4])^2)/ ///
(1/(B[2,1])^2 +  1/(B[2,2])^2 +  1/(B[2,3])^2 + 1/(B[2,4])^2)

di "My average: `average'"

assert abs(`average' - `b') < 0.01
restore


preserve

di "Hold out fold 4, intercept value 1"
gsem (y1 <- study_*  age M1[id]@1) if study != "4", nocons

fit_hold_out_gsem study, model_code(`model_code_uv') hold_out_fold(4) intercept_est("value") model_options(nocons) intercept_value(1)
mat A = r(table)
local b = A[1,5]
assert `b' ==1

restore
