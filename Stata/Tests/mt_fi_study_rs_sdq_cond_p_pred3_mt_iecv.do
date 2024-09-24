use "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data/pooled_sdq_mi1_test.dta", clear    
   
    qui do "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/R/prediction_for_autistic_children/Stata/prediction_progs.do"
    
    cap drop age_spline1
 cap drop age_spline2
mkspline age_spline = age_c, nknots(3) cubic

*********************** Running model on whole populaiton ***********************
      mixed_first_gsem_rs , ///
	gsem_code(gsem (sdq_cond_p <- study_* age_spline1 age_spline2 ///
 base_sex ///
c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
 base_sdq_pro_p base_sdq_hyp_p base_sdq_emot_p  base_sdq_peer_p base_ld base_ethnicity base_maternal_education base_imd_decile base_maternal_mh  M1[ID]@1 c.age_c#M2[ID]@1)) ///
  gsem_options(nocons) ///
	mixed_code(mixed sdq_cond_p study_* age_spline1 age_spline2 ///
 base_sex ///
c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
 base_sdq_pro_p base_sdq_hyp_p base_sdq_emot_p  base_sdq_peer_p base_ld base_ethnicity base_maternal_education base_imd_decile base_maternal_mh, noconstant   || ID: age_c) ///
  mixed_model_options(cov(unstructured))

*********************** Validating ***********************
       
    get_all_preds study, ///
    model_code(gsem (sdq_cond_p <- study_* age_spline1 age_spline2 ///
 base_sex ///
c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
 base_sdq_pro_p base_sdq_hyp_p base_sdq_emot_p  base_sdq_peer_p base_ld base_ethnicity base_maternal_education base_imd_decile base_maternal_mh  M1[ID]@1 c.age_c#M2[ID]@1)) ///
    model_options(nocons) ///
  	intercept_est(average estimate estimate_cv) ///
  	out_wave(2) ///
  	predictor_waves(0 -1) ///
  	mixed_model(mixed sdq_cond_p study_* age_spline1 age_spline2 ///
 base_sex ///
c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
 base_sdq_pro_p base_sdq_hyp_p base_sdq_emot_p  base_sdq_peer_p base_ld base_ethnicity base_maternal_education base_imd_decile base_maternal_mh, noconstant   || ID: age_c) ///
  	mixed_model_options("cov(unstructured)") 
    

