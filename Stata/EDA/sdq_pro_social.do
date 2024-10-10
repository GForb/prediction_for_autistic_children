
 
 use "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data/pooled_sdq.dta", clear
 
keep if autism != "post baseline" 



  qui do "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/R/prediction_for_autistic_children/Stata/prediction_progs_gsem.do"
    
    mkspline age_spline = age_c, nknots(3) cubic

*********************** Running model on whole populaiton ***********************
      gsem (sdq_pro_p <- age_spline* base_sex c.age_spline1#i.base_sex c.age_spline2#i.base_sex  base_sdq_emot_p base_sdq_cond_p base_sdq_hyp_p  base_sdq_peer_p  STUDY[study]@1 M1[ID]@1 c.age_c#M2[ID]@1), var( M2[ID]@0.001) cov(M1[ID]*M2[ID]@0)
  mat simple = e(b)
  
   gsem (sdq_pro_p <- age_spline* base_sex c.age_spline1#i.base_sex c.age_spline2#i.base_sex  base_sdq_emot_p base_sdq_cond_p base_sdq_hyp_p  base_sdq_peer_p  STUDY[study]@1 M1[ID]@1 c.age_c#M2[ID]@1), from(simple) cov(M1[ID]*M2[ID]@0) iter(3) 
    mat simple2 = e(b)

  gsem (sdq_pro_p <- age_spline* base_sex c.age_spline1#i.base_sex c.age_spline2#i.base_sex  base_sdq_emot_p base_sdq_cond_p base_sdq_hyp_p  base_sdq_peer_p  STUDY[study]@1 M1[ID]@1 c.age_c#M2[ID]@1), from(simple)

*********************** Validating ***********************
       
    get_all_preds_gsem study, ///
    pred_var_name(pred) ///
    model_code(gsem (sdq_pro_p <- age_spline* base_sex c.age_spline1#i.base_sex c.age_spline2#i.base_sex  base_sdq_emot_p base_sdq_cond_p base_sdq_hyp_p  base_sdq_peer_p  STUDY[study]@1 M1[ID]@1 c.age_c#M2[ID]@1)) ///
  	intercept_est(average) ///
  	predict_function(predict_multiwave_gsem_rs_uv) ///
  	predict_args(wave_var(wave) ///
  	out_wave(2) ///
  	predictor_waves(0 -1) ///
  	id_var(ID)) ///
  	random_study ///
  	simple_model(gsem (sdq_pro_p <- age_spline* base_sex c.age_spline1#i.base_sex c.age_spline2#i.base_sex  base_sdq_emot_p base_sdq_cond_p base_sdq_hyp_p  base_sdq_peer_p  STUDY[study]@1 M1[ID]@1 c.age_c#M2[ID]@1)) ///
  	simple_model_options(var(M2[ID]@0.01) cov(M1[ID]*M2[ID]@0))
