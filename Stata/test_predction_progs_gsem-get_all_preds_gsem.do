cd "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data" //. set to data and outputs 

use "pooled_vabs_long_complete.dta", clear // vabs data, fixed study


    qui do "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/R/prediction_for_autistic_children/Stata/prediction_progs_gsem.do"
    

*do prediction_progs_gsem.do

*********************** Validating ***********************

*Fixed study, random intercept, estimate
use "pooled_vabs_long_complete.dta", clear // vabs data, fixed study

    mkspline age_spline = age_c, nknots(3) cubic

       
get_all_preds_gsem study, ///
    pred_var_name(pred) ///
    model_code(gsem (vabs_soc_ae <- study_* age_spline1 age_spline2 ///
    base_vabs_dq base_sex ///
    c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
    c.age_spline1#c.base_vabs_dq_dec c.age_spline2#c.base_vabs_dq_dec ///
    base_vabs_dls_ae base_vabs_com_ae   M1[ID]@1)) ///
    intercept_est(estimate) ///
    model_options(nocons) ///
	predict_function(predict_multiwave_gsem_ri) ///
	predict_args(wave_var(wave) ///
	out_wave(2) ///
	predictor_waves(0 -1) ///
	id_var(ID))
	
	
	su pred*  if wave ==2
	local pred = r(mean)
	su vabs_soc_ae if wave ==2 
	local actual = r(mean)
	assert abs(`pred' -`actual') < 0.2
	
	
use "pooled_vabs_long_complete.dta", clear // vabs data, fixed study

    mkspline age_spline = age_c, nknots(3) cubic

 *Fixed study, random intercept, average

get_all_preds_gsem study, ///
    pred_var_name(pred) ///
    model_code(gsem (vabs_soc_ae <- study_* age_spline1 age_spline2 ///
    base_vabs_dq base_sex ///
    c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
    c.age_spline1#c.base_vabs_dq_dec c.age_spline2#c.base_vabs_dq_dec ///
    base_vabs_dls_ae base_vabs_com_ae   M1[ID]@1)) ///
    intercept_est(average) ///
    model_options(nocons) ///
	predict_function(predict_multiwave_gsem_ri) ///
	predict_args(wave_var(wave) ///
	out_wave(2) ///
	predictor_waves(0 -1) ///
	id_var(ID))
	
	su pred*  if wave ==2
	local pred = r(mean)
	su vabs_soc_ae if wave ==2 
	local actual = r(mean)
	assert abs(`pred' -`actual') < 0.2
    

*Random study, random intercept_est
use "pooled_sdq.dta", clear
    mkspline age_spline = age_c, nknots(3) cubic


get_all_preds_gsem study, ///
   pred_var_name(pred) ///
   model_code(gsem (sdq_cond_p <- age_spline* base_sex c.age_spline1#i.base_sex ///
   c.age_spline2#i.base_sex  base_sdq_emot_p  base_sdq_hyp_p base_sdq_pro_p ///
   base_sdq_peer_p  STUDY[study]@1 M1[ID]@1)) ///
         intercept_est(average) ///
         predict_function(predict_multiwave_gsem_ri) ///
         predict_args(wave_var(wave) ///
         out_wave(2) ///
         predictor_waves(0 -1) ///
         id_var(ID)) ///
        random_study
		
	su pred*  if wave ==2
	local pred = r(mean)
	su sdq_cond_p if wave ==2 
	local actual = r(mean)
	assert abs(`pred' -`actual') < 0.2

	
	
** Wanring - sloe to run **
	
*Random study, random slope
use "pooled_sdq.dta", clear
mkspline age_spline = age_c, nknots(3) cubic

get_all_preds_gsem study, ///
 pred_var_name(pred) ///
 model_code(gsem (sdq_emot_p <- age_spline* base_sex c.age_spline1#i.base_sex ///
 c.age_spline2#i.base_sex  base_sdq_cond_p  base_sdq_hyp_p base_sdq_pro_p ///
 base_sdq_peer_p  STUDY[study]@1 M1[ID]@1 c.age_c#M2[ID]@1)) ///
	 intercept_est(average) ///
	 predict_function(predict_multiwave_gsem_rs_uv) ///
	 predict_args(wave_var(wave) ///
	 out_wave(2) ///
	 predictor_waves(0 -1) ///
	 id_var(ID)) ///
	 random_study ///
	 simple_model(gsem (sdq_emot_p <- age_spline* base_sex c.age_spline1#i.base_sex ///
	 c.age_spline2#i.base_sex  base_sdq_cond_p  base_sdq_hyp_p base_sdq_pro_p ///
	 base_sdq_peer_p  STUDY[study]@1 M1[ID]@1 c.age_c#M2[ID]@1)) ///
	 simple_model_options(var(M2[ID]@0.01) cov(M1[ID]*M2[ID]@0))

su pred*  if wave ==2
local pred = r(mean)
su sdq_emot_p if wave ==2 
local actual = r(mean)
assert abs(`pred' -`actual') < 0.5


