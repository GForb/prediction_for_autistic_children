use "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data/pooled_vabs.dta", clear
mkspline age_spline = age_c, nknots(3) cubic

gen base_vabs_dq_dec = base_vabs_dq/10
gen spl1_vabs_dq = age_spline1*base_vabs_dq

gen spl2_vabs_dq = age_spline2*base_vabs_dq
gen spl1_vabs_dq_dec = age_spline1*base_vabs_dq_dec

gen spl2_vabs_dq_dec = age_spline2*base_vabs_dq_dec

* Running mixed
mixed vabs_dls_ae study_* ///
	c.age_spline1##i.base_sex c.age_spline2##i.base_sex   c.age_spline1##c.base_vabs_dq c.age_spline2##c.base_vabs_dq  ///
	base_vabs_soc_ae base_vabs_com_ae, noconstant  || ID: age_c,  cov(unstructured)
	
	mixed vabs_dls_ae study_* ///
	c.age_spline1##i.base_sex c.age_spline2##i.base_sex   c.age_spline1#c.base_vabs_dq_dec c.age_spline2#c.base_vabs_dq_dec  base_vabs_dq ///
	base_vabs_soc_ae base_vabs_com_ae, noconstant  || ID: age_c,  cov(unstructured)
	
	
	mixed vabs_soc_ae study_* ///
	c.age_spline1##i.base_sex c.age_spline2##i.base_sex   c.age_spline1#c.base_vabs_dq_dec c.age_spline2#c.base_vabs_dq_dec  base_vabs_dq ///
	base_vabs_dls_ae base_vabs_com_ae, noconstant  || ID: age_c,  cov(unstructured)
	
	
	mixed_extract_prog
	
	mixed vabs_com_ae study_* ///
	c.age_spline1##i.base_sex c.age_spline2##i.base_sex   c.age_spline1##c.base_vabs_dq c.age_spline2##c.base_vabs_dq  ///
	base_vabs_soc_ae  base_vabs_dls_ae, noconstant  || ID: age_c,  cov(unstructured)

	
mixed vabs_soc_ae <- study_* age_spline1 age_spline2 ///
 base_vabs_dq base_sex ///
 c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
 c.age_spline1#c.base_vabs_dq_dec c.age_spline2#c.base_vabs_dq_dec ///
 base_vabs_dls_ae base_vabs_com_ae , noconstant   || ID: age_c, cov(unstructured)
	
*Running mixed without pathways
preserve
tab study
keep if study !=  "Pathways"

	mixed vabs_dls_ae study_* ///
	c.age_spline1##i.base_sex c.age_spline2##i.base_sex   c.age_spline1#c.base_vabs_dq_dec c.age_spline2#c.base_vabs_dq_dec  base_vabs_dq ///
	base_vabs_soc_ae base_vabs_com_ae, noconstant  || ID: age_c if study != "EDX" ,  cov(unstructured)  
	
	           mat A = r(table)
       local ncol  colsof(A)
       local ncol_rs = `ncol' - 3
      local ncol_ri = `ncol' - 2
      local ncol_cov = `ncol' - 1
       
       local var_ri = A[1,`ncol_ri']
       local var_rs = A[1,`ncol_rs']
       local cov_rs_ri = A[1,`ncol_cov']  
	   
	   di var_ri
	   di var_rs
	   di cov_rs_ri
	   
	    gsem (vabs_dls_ae <- study_* age_spline1 age_spline2 ///
			base_vabs_dq base_sex ///
			c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
			c.age_spline1#c.base_vabs_dq_dec c.age_spline2#c.base_vabs_dq_dec ///
			 base_vabs_soc_ae  base_vabs_com_ae  M1[ID]@1 c.age_c#M2[ID]@1) if study != "EDX", var(M1[ID]@`var_ri' M2[ID]@`var_rs') cov(M1[ID]*M2[ID]@`cov_rs_ri') nocons
	
	mixed vabs_soc_ae study_* ///
	c.age_spline1##i.base_sex c.age_spline2##i.base_sex   c.age_spline1#c.base_vabs_dq_dec c.age_spline2#c.base_vabs_dq_dec  base_vabs_dq ///
	base_vabs_dls_ae base_vabs_com_ae, noconstant  || ID: age_c,  cov(unstructured)
	
	mixed vabs_com_ae study_* ///
	c.age_spline1##i.base_sex c.age_spline2##i.base_sex   c.age_spline1#c.base_vabs_dq_dec c.age_spline2#c.base_vabs_dq_dec  base_vabs_dq ///
	base_vabs_soc_ae base_vabs_dls_ae, noconstant  || ID: age_c,  cov(unstructured)
	
	
restore

*Running gsem

	
 gsem (vabs_com_ae <- study_* age_spline1 age_spline2 ///
			base_vabs_dq base_sex ///
			c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
			c.age_spline1#c.base_vabs_dq_dec c.age_spline2#c.base_vabs_dq_dec ///
			 base_vabs_dls_ae  base_vabs_soc_ae  M1[ID]@1 c.age_c#M2[ID]@1), var(M1[ID] M2[ID]@0.001) cov(M1[ID]*M2[ID]@0) nocons
  mat simple = e(b)
  gsem (vabs_com_ae <- study_* age_spline1 age_spline2 ///
base_vabs_dq base_sex ///
c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
c.age_spline1#c.base_vabs_dq_dec c.age_spline2#c.base_vabs_dq_dec ///
 base_vabs_dls_ae  base_vabs_soc_ae  M1[ID]@1 c.age_c#M2[ID]@1), from(simple) nocons
 


	
 gsem (vabs_soc_ae <- study_* age_spline1 age_spline2 ///
			base_vabs_dq base_sex ///
			c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
			c.age_spline1#c.base_vabs_dq_dec c.age_spline2#c.base_vabs_dq_dec ///
			 base_vabs_dls_ae  base_vabs_com_ae  M1[ID]@1 c.age_c#M2[ID]@1), var(M1[ID] M2[ID]@0.001) cov(M1[ID]*M2[ID]@0) nocons
  mat simple = e(b)
  
   gsem (vabs_soc_ae <- study_* age_spline1 age_spline2 ///
			base_vabs_dq base_sex ///
			c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
			c.age_spline1#c.base_vabs_dq_dec c.age_spline2#c.base_vabs_dq_dec ///
			 base_vabs_dls_ae  base_vabs_com_ae  M1[ID]@1 c.age_c#M2[ID]@1), var(M1[ID] M2[ID]@0.003) cov(M1[ID]*M2[ID]@0) nocons
  mat simple = e(b)
  
  gsem (vabs_soc_ae <- study_* age_spline1 age_spline2 ///
		base_vabs_dq base_sex ///
		c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
		c.age_spline1#c.base_vabs_dq_dec c.age_spline2#c.base_vabs_dq_dec ///
		 base_vabs_dls_ae  base_vabs_com_ae  M1[ID]@1 c.age_c#M2[ID]@1), from(simple) nocons
		 
 	
 gsem (vabs_dls_ae <- study_* age_spline1 age_spline2 ///
			base_vabs_dq base_sex ///
			c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
			c.age_spline1#c.base_vabs_dq_dec c.age_spline2#c.base_vabs_dq_dec ///
			 base_vabs_soc_ae  base_vabs_com_ae  M1[ID]@1 c.age_c#M2[ID]@1), var(M1[ID] M2[ID]@0.001) cov(M1[ID]*M2[ID]@0) nocons
  mat simple = e(b)
  
  
  gsem (vabs_dls_ae <- study_* age_spline1 age_spline2 ///
base_vabs_dq base_sex ///
c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
c.age_spline1#c.base_vabs_dq_dec c.age_spline2#c.base_vabs_dq_dec ///
 base_vabs_soc_ae  base_vabs_com_ae  M1[ID]@1 c.age_c#M2[ID]@1), from(simple) nocons
 
 


preserve
tab study
keep if study !=  "Pathways"

	mixed vabs_dls_ae study_* ///
	c.age_spline1##i.base_sex c.age_spline2##i.base_sex   c.age_spline1#c.base_vabs_dq_dec c.age_spline2#c.base_vabs_dq_dec  base_vabs_dq ///
	base_vabs_soc_ae base_vabs_com_ae, noconstant  || ID: age_c,  cov(unstructured)

gsem (vabs_dls_ae <- study_* age_spline1 age_spline2 ///
			base_vabs_dq base_sex ///
			c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
			c.age_spline1#c.base_vabs_dq_dec c.age_spline2#c.base_vabs_dq_dec ///
			 base_vabs_soc_ae  base_vabs_com_ae  M1[ID]@1 c.age_c#M2[ID]@1), var(M1[ID]@0.4374036  M2[ID]@0.0153432  ) cov(M1[ID]*M2[ID]@0.0819217 ) nocons

// gsem (vabs_dls_ae <- study_* age_spline1 age_spline2 ///
// 			base_vabs_dq base_sex ///
// 			c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
// 			c.age_spline1#c.base_vabs_dq_dec c.age_spline2#c.base_vabs_dq_dec ///
// 			 base_vabs_soc_ae  base_vabs_com_ae  M1[ID]@1 c.age_c#M2[ID]@1), var(M1[ID] M2[ID]@0.001) cov(M1[ID]*M2[ID]@0) nocons
//   mat simple = e(b)
//  
  
//   gsem (vabs_dls_ae <- study_* age_spline1 age_spline2 ///
// base_vabs_dq base_sex ///
// c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
// c.age_spline1#c.base_vabs_dq_dec c.age_spline2#c.base_vabs_dq_dec ///
//  base_vabs_soc_ae  base_vabs_com_ae  M1[ID]@1 c.age_c#M2[ID]@1), from(simple) nocons

restore


count
gsem (vabs_dls_ae <- study_* c.age_spline1##i.base_sex c.age_spline2##i.base_sex   c.age_spline1##c.base_vabs_dq c.age_spline2##c.base_vabs_dq    base_vabs_soc_ae base_vabs_com_ae M1[ID]@1), nocons

su age if wave ==0
gen age_rebased = age - r(mean)
mkspline age_r_spline = age_rebased, nknots(3) cubic


gsem (vabs_dls_ae <- study_* ///
	c.age_r_spline1##i.base_sex c.age_r_spline2##i.base_sex   c.age_r_spline1##c.base_vabs_dq c.age_r_spline2##c.base_vabs_dq  ///
	base_vabs_soc_ae base_vabs_com_ae M1[ID]@1 c.age_rebased#M2[ID]@1), nocons 

	
gsem (vabs_dls_ae <- study_* ///
	c.age_spline1##i.base_sex c.age_spline2##i.base_sex   c.age_spline1##c.base_vabs_dq c.age_spline2##c.base_vabs_dq  ///
	base_vabs_soc_ae base_vabs_com_ae M1[ID]@1 c.age#M2[ID]@1), nocons 

predict pred_natural

twoway (scatter  vabs_dls_ae pred_natural)

twoway (scatter  vabs_dls_ae age)

twoway (scatter   pred_natural age) (scatter  vabs_dls_ae age)


gsem (vabs_dls_ae <- study_* c.age_spline1##i.base_sex c.age_spline2##i.base_sex base_vabs_soc_ae base_vabs_com_ae M1[ID]@1), nocons poisson
predict pred_natural_p


gsem (vabs_dls_ae <- study_* c.age_spline1##i.base_sex c.age_spline2##i.base_sex base_vabs_soc_ae base_vabs_com_ae M1[ID]@1 c.age#M2[ID]), nocons poisson


mixed vabs_dls_ae study_* c.age_spline1##i.base_sex c.age_spline2##i.base_sex base_vabs_soc_ae base_vabs_com_ae, noconstant  || ID:
est store A
cap drop pred_natural_ri
predict pred_natural_ri,  fitted 

mixed vabs_dls_ae study_* ///
	c.age_spline1##i.base_sex c.age_spline2##i.base_sex   c.age_spline1##c.base_vabs_dq c.age_spline2##c.base_vabs_dq  ///
	base_vabs_soc_ae base_vabs_com_ae, noconstant  || ID: age,  cov(unstructured)
	
	
est store B
cap drop pred_natural_rs
predict pred_natural_rs,  fitted 



lrtest A B

twoway scatter pred_natural_ri vabs_dls_ae, name(ri, replace)
twoway scatter pred_natural_rs vabs_dls_ae, name(rs, replace)

cap drop pred*
predict pred_fixed_rs

predict pred_all  if wave != 1
gen pred_random = pred_all - pred_fixed_rs
bysort ID: egen pred_random_out = mean(pred_random)

gen pred_out = pred_fixed_rs + pred_random_out if wave ==1

scatter vabs_dls_ae pred_out 


*Next model: With interaction
mixed vabs_dls_ae study_* c.age_spline1##i.base_sex c.age_spline2##i.base_sex  c.age_spline1##c.base_vabs_dls_ae c.age_spline2##c.base_vabs_dls_ae base_vabs_soc_ae base_vabs_com_ae, noconstant  || ID:
 est store ri_int

predict pred_natural_int_ri, fitted
scatter pred_natural_int_ri vabs_dls_ae

cap drop pred*
predict pred_fixed

predict pred_all  if wave == 0 | wave ==-1
gen pred_random = pred_all - pred_fixed
bysort ID: egen pred_random_out = mean(pred_random)

gen pred_out = pred_fixed + pred_random_out if wave ==1

sort pred_out
twoway (scatter vabs_dls_ae pred_out if wave == 1) (function y = x, range(0 20) ) , name(ri_int, replace)


mixed vabs_dls_ae ///
 study_* c.age_spline1##i.base_sex c.age_spline2##i.base_sex  c.age_spline1##c.base_vabs_dls_ae c.age_spline2##c.base_vabs_dls_ae base_vabs_soc_ae base_vabs_com_ae, noconstant ///
 || ID: age
 est store rs_int

predict pred_natural_int_ri, fitted
scatter pred_natural_int_ri vabs_dls_ae

cap drop pred*
predict pred_fixed

predict pred_all  if wave == 0 | wave ==-1
gen pred_random = pred_all - pred_fixed
bysort ID: egen pred_random_out = mean(pred_random)

gen pred_out = pred_fixed + pred_random_out if wave ==1

sort pred_out
twoway (scatter vabs_dls_ae pred_out if wave == 1) (function y = x, range(0 20) ), name(rs_int, replace)


lrtest rs_int ri_int



*Regression


use "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data/pooled_vabs_wide.dta", clear

one_hot_encode study

mkspline base_vabs_dls_spline = base_vabs_dls_ae, nknots(3) cubic

local model_code regress out_vabs_dls_ae study_* base_vabs_dls_spline* base_age age base_vabs_com_ae base_vabs_soc_ae base_sex 
`model_code'
get_all_preds_reg study, model_code(`model_code') pred_var_name("pred") intercept_est("estimate")

*Some plots


scatter out_vabbase_vabs_dls_ae base_vabs_dls_ae


