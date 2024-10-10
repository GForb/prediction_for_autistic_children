qui do "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/R/prediction_for_autistic_children/Stata/prediction_progs.do"

cd "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data" //. set to data and outputs 

use "pooled_sdq.dta", clear
keep if out_all_complete ==1

*Fixed study, random intercept

splitsample, generate(fold)  cluster(ID) nsplit(5)  
tab fold
gen int_est_marker = 0
replace int_est_marker = 1 if fold ==1

gsem (sdq_pro_p <- study_*  age_c M1[ID]@1)

preserve
predict_multiwave_gsem_uv pred,  out_wave(2) predictor_waves(0 -1) 

su pred  if wave ==2 
local pred = r(mean)
su sdq_pro_p if wave ==2  
local actual = r(mean)
assert abs(`pred' -`actual') < 0.2
restore

preserve
predict_multiwave_gsem_uv pred, out_wave(2) predictor_waves(0 -1)   int_est_marker(int_est_marker)
su pred  if wave ==2 & int_est_marker ==1
assert r(N)  ==0
su pred  if wave ==2 & int_est_marker !=1
local pred = r(mean)
su sdq_pro_p if wave ==2  & int_est_marker !=1
local actual = r(mean)
assert abs(`pred' -`actual') < 0.2
restore



*Radnom study, random intercept
use "pooled_sdq.dta", clear
keep if out_all_complete ==1
splitsample, generate(fold)  cluster(ID) nsplit(5)  
gen int_est_marker = 0
replace int_est_marker = 1 if fold ==1

gsem (sdq_pro_p <- STUDY[study]  age M1[ID]@1)

preserve
predict_multiwave_gsem_uv pred,  out_wave(2) predictor_waves(0 -1) random_study
su pred  if wave ==2 
local pred = r(mean)
su sdq_pro_p if wave ==2  
local actual = r(mean)
assert abs(`pred' -`actual') < 0.2
restore

preserve
predict_multiwave_gsem_uv pred,  out_wave(2) predictor_waves(0 -1)   int_est_marker(int_est_marker) random_study
su pred  if wave ==2 & int_est_marker ==1
assert r(N)  ==0
su pred  if wave ==2 & int_est_marker !=1
local pred = r(mean)
su sdq_pro_p if wave ==2  & int_est_marker !=1
local actual = r(mean)
assert abs(`pred' -`actual') < 0.2
restore

* Fixed study, random slope
gsem (sdq_pro_p <- study_*  age_c M1[ID]@1 c.age_c#M2[ID])
preserve
predict_multiwave_gsem_uv pred,  out_wave(2) predictor_waves(0 -1)   int_est_marker(int_est_marker) random_slope
su pred  if wave ==2 & int_est_marker ==1
assert r(N)  ==0
su pred  if wave ==2 & int_est_marker !=1
local pred = r(mean)
su sdq_pro_p if wave ==2  & int_est_marker !=1
local actual = r(mean)
assert abs(`pred' -`actual') < 0.2
restore

