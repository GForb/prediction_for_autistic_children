use "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data/pooled_vabs.dta", clear
one_hot_encode study

mkspline age_spline = age_c, nknots(3) cubic

tab study

EDX       
ELENA        
EpiTED       
Pathways    


gsem (vabs_dls_ae <- study_* c.age_spline1##i.base_sex c.age_spline2##i.base_sex   c.age_spline1##c.base_vabs_dq c.age_spline2##c.base_vabs_dq    base_vabs_soc_ae base_vabs_com_ae M1[ID]@1), nocons


local hold_out "EpiTED"

gsem (vabs_dls_ae <- study_* c.age_spline1##i.base_sex c.age_spline2##i.base_sex   c.age_spline1##c.base_vabs_dq c.age_spline2##c.base_vabs_dq    base_vabs_soc_ae base_vabs_com_ae M1[ID]@1) if study != "`hold_out'", nocons


cap drop pred*
fit_hold_out_gsem study, ///
	model_code(gsem (vabs_dls_ae <- study_* c.age_spline1##i.base_sex c.age_spline2##i.base_sex   c.age_spline1##c.base_vabs_dq c.age_spline2##c.base_vabs_dq    base_vabs_soc_ae base_vabs_com_ae M1[ID]@1)) ///
	hold_out_fold("`hold_out'") ///
	intercept_est("estimate") ///
	model_options("nocons") 

predict pred_est if study == "`hold_out'"

gsem (vabs_dls_ae <- study_* c.age_spline1##i.base_sex c.age_spline2##i.base_sex   c.age_spline1##c.base_vabs_dq c.age_spline2##c.base_vabs_dq    base_vabs_soc_ae base_vabs_com_ae M1[ID]@1) if study != "`hold_out'", nocons

fit_hold_out_gsem study, ///
	model_code(gsem (vabs_dls_ae <- study_* c.age_spline1##i.base_sex c.age_spline2##i.base_sex   c.age_spline1##c.base_vabs_dq c.age_spline2##c.base_vabs_dq    base_vabs_soc_ae base_vabs_com_ae M1[ID]@1)) ///
	hold_out_fold("`hold_out'") ///
	intercept_est("average") ///
	model_options("nocons") ///
	
	
predict pred_av if study == "`hold_out'"


gsem (vabs_dls_ae <- study_* c.age_spline1##i.base_sex c.age_spline2##i.base_sex   c.age_spline1##c.base_vabs_dq c.age_spline2##c.base_vabs_dq    base_vabs_soc_ae base_vabs_com_ae M1[ID]@1) if study !="`hold_out'", nocons

fit_hold_out_gsem study, ///
	model_code(gsem (vabs_dls_ae <- study_* c.age_spline1##i.base_sex c.age_spline2##i.base_sex   c.age_spline1##c.base_vabs_dq c.age_spline2##c.base_vabs_dq    base_vabs_soc_ae base_vabs_com_ae M1[ID]@1)) ///
	hold_out_fold("`hold_out'") ///
	intercept_est("value") ///
	model_options("nocons") ///
	intercept_value( -.3026454)
	
predict pred_est_val if study == "`hold_out'"


su pred*

preserve
qui get_all_preds_gsem study , ///
	model_code(gsem (vabs_dls_ae <- study_* c.age_spline1##i.base_sex c.age_spline2##i.base_sex   c.age_spline1##c.base_vabs_dq c.age_spline2##c.base_vabs_dq    base_vabs_soc_ae base_vabs_com_ae M1[ID]@1)) ///
	pred_var_name(pred_est) ///
	intercept_est("estiamte") ///
	model_options(nocons) ///
	predict_function(predict_multiwave_gsem_ri) ///
	predict_args(wave_var(wave) ///
	out_wave(1) ///
	predictor_waves(0 -1) ///
	id_var(ID))
	
su pred*
bysort study: su new_intercept* pred* vabs_dls_ae if wave == 1

restore	
	
preserve
qui get_all_preds_gsem study , ///
	model_code(gsem (vabs_dls_ae <- study_* c.age_spline1##i.base_sex c.age_spline2##i.base_sex   c.age_spline1##c.base_vabs_dq c.age_spline2##c.base_vabs_dq    base_vabs_soc_ae base_vabs_com_ae M1[ID]@1)) ///
	pred_var_name(pred_av) ///
	intercept_est("average") ///
	model_options(nocons) ///
	predict_function(predict_multiwave_gsem_ri) ///
	predict_args(wave_var(wave) ///
	out_wave(1) ///
	predictor_waves(0 -1) ///
	id_var(ID))
	
	
su pred*
bysort study: su new_intercept* pred* vabs_dls_ae if wave == 1

restore
