
use "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Derived data/pooled_vabs.dta", clear

mkspline age_spline = age_c, nknots(3) cubic
one_hot_encode study

tab study

keep if in_dls ==1  & vabs_dls_ae !=.


gsem (vabs_dls_ae <- ///
study_* c.age_spline1 c.age_spline2 c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
		c.age_spline1#c.base_vabs_dq c.age_spline2#c.base_vabs_dq ///
		base_vabs_dq base_sex base_vabs_soc_ae base_vabs_com_ae ///
		M1[ID]@1 c.age_c#M2[ID]), nocons 

 matrix b = e(b)

cap drop pred
predict_multiwave_gsem_rs_uv pred, predictor_waves(0 -1) out_wave(1) id_var(ID) wave_var(wave)


twoway (scatter vabs_dls_ae pred) (function y = x, range(0 20))

su pred



gsem (vabs_dls_ae <- ///
study_* c.age_spline1 c.age_spline2 c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
		c.age_spline1#c.base_vabs_dq c.age_spline2#c.base_vabs_dq ///
		base_vabs_dq base_sex base_vabs_soc_ae base_vabs_com_ae ///
		M1[ID]@1 c.age_c#M2[ID]) if study_Pathways !=1, nocons from(b)
		
		
mixed vabs_dls_ae study_* c.age_spline1 c.age_spline2 c.age_spline1#i.base_sex c.age_spline2#i.base_sex ///
		c.age_spline1#c.base_vabs_dq c.age_spline2#c.base_vabs_dq ///
		base_vabs_dq base_sex base_vabs_soc_ae base_vabs_com_ae, nocons  || ID: age_c if study_Pathways !=1
