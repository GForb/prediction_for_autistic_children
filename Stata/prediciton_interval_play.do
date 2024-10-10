    
 *cap log close results_log
  *log using "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Results/SDQ/Thesis/Logs/st_fi_study_sdq_peer_p_pred1_model_only.log", text replace name(results_log)
  
  use "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data/pooled_sdq_wide.dta"

  
  mkspline base_spline = base_sdq_peer_p, nknots(3) cubic

*********************** Running model on whole populaiton ***********************
capture confirm variable mi_m
if !_rc {
tempfile temp
save  `temp'
   mi import flong, id(mi_id) m(mi_m)
   mi estimate: regress out_sdq_peer_p study_*  base_spline* c.base_spline1#i.base_sex c.base_spline2#i.base_sex base_age out_age base_sdq_pro_p base_sdq_hyp_p base_sdq_emot_p base_sdq_cond_p  base_sex , nocons
}
else {
    regress out_sdq_peer_p study_*  base_spline* c.base_spline1#i.base_sex c.base_spline2#i.base_sex base_age out_age base_sdq_pro_p base_sdq_hyp_p base_sdq_emot_p base_sdq_cond_p  base_sex , nocons
 }

  mat A = r(table)
  matrix At =  A'	
  clear
 
  svmat2 At, names(col)  rnames(coef)
  gen N = `e(N)'

*  log close results_log


  
  predict se,    stdp  
  predict se_pred, stdf
