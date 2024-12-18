*assumes:
/*
study var is study
wave var is wave
random intercept is M1[ID]
random slope is M2[ID]
random study is STUDY[study]
fixed studies are study_*
returns pred var as pred
*/

cap prog drop get_all_preds
prog define get_all_preds
syntax name, ///
	model_code(passthru) ///
	[ ///
	intercept_est(passthru)  ///
	single_time_point ///
	predictor_waves(passthru) out_wave(passthru) ///
	cross_validation n_cv_folds(integer 5) n_reps(integer 1) random_study random_slope ///
	model_options(passthru) ///
	intercept_value(passthru) ///
	simple_model(passthru) simple_model_options(passthru) ///
	mixed_model(passthru) mixed_model_options(passthru) mixed_model_extract_values(passthru) ///
	]
	
	tempfile data_with_preds
	
	local count = 1

	forvalues rep = 1 (1) `n_reps' {
		if "`cross_validation'" != "" {
			di "creating folds"
			cap drop fold_numeric
			cap drop fold_string
			splitsample, generate(fold_numeric)  cluster(ID) nsplit(`n_cv_folds')
	
			tostring fold_numeric, gen(fold_string)
			
			if `count' == 1 {
				gen fold = fold_string
			} 
			else {
				replace fold = fold_string
			}
			
			local random_split "random_split"
			local namelist fold
		}	
		cap drop validation_rep
		gen validation_rep = `rep'
	
		*For internal validaiton there are two variables: fold and study 
	
	
		qui levelsof  `namelist', local(folds)
		foreach fold in `folds' {

			di "Fold:  `fold'"
			preserve
			if "`single_time_point'" != "" {
				get_hold_out_pred_st `namelist', hold_out_fold(`fold')  ///
					`model_code' `model_options' `random_split' `intercept_est' `random_study'
			}
			else {
				get_hold_out_pred_gsem `namelist', hold_out_fold(`fold') `model_code' ///
					`pred_var_name' `predictor_waves' `out_wave' `model_options' ///
					`random_split' `intercept_est' `random_study' `random_slope' `simple_model' `simple_model_options' `mixed_model' `mixed_model_options' `mixed_model_extract_values'
			
			}
			
	
			di "completed hold out bit, saving data"
			keep if `namelist' == "`fold'"
			if `count' == 1 {
				save  `data_with_preds'
			}
			else {
				append using `data_with_preds'
				save `data_with_preds', replace
			}
			restore
			local count = `count' + 1
		}
	
	}
	
	
	di "loading data"
	use `data_with_preds', clear
end



*Fit model

cap prog drop get_hold_out_pred_st
prog define get_hold_out_pred_st
syntax varname, ///
	hold_out_fold(string) ///
	model_code(string) ///
	[ ///
	intercept_est(string) ///
	random_split   ///
	random_study ///
	model_options(string) ///
	intercept_value(passthru) ///
	]
	
	* Fit model
	if "`model_options'" != "" local model_options_c , `model_options'
	`model_code' if `varlist' != "`hold_out_fold'"  `model_options_c'
	local outcome `e(depvar)'

	est store model
	local model_command   `e(cmd) '
	if "`random_split'" != "" {
		local intercept_est "estimate"
		tempvar int_est_marker 
		gen `int_est_marker' = 1 if `varlist' != "`hold_out_fold'"
	
	
			di "making predictions for random split"
			est_intercept_get_pred_st `varlist', hold_out_fold(`hold_out_fold') ///
				 model_name(model) model_command(`model_command') ///
				 pred_var_name(pred_cv) int_est_marker(`int_est_marker') ///
				 `random_split' `random_study' ///
				 model_code(`model_code') model_options(`model_options') 
	}
	else {
		foreach method in `intercept_est' {
			di "Predicting using `method' intercepts"
			est restore model
			`model_command'
			
			if "`method'" == "estimate_cv" {
				est_intercept_get_pred_cv `varlist', hold_out_fold(`hold_out_fold') model_name(model) model_command(`model_command') ///
					`random_split' `random_study' `random_slope'  model_code(`model_code') model_options(`model_options') intercept_est(`method') `intercept_value' `predict_args' ///
					single_time_point
			}
			else {
				est_intercept_get_pred_st `varlist', hold_out_fold(`hold_out_fold') ///
					 model_name(model) model_command(`model_command') ///
					 pred_var_name(pred_`method') ///
					 `random_split' `random_study' ///
					 model_code(`model_code') model_options(`model_options') ///
					 intercept_est(`method') `intercept_value' 
			}
		}	
		local n_est_methods = wordcount(`"`intercept_est'"') 
		if `n_est_methods' == 1 {
			di "fixing pred name"
			rename pred_`intercept_est' pred
		}
	
	}
	
	

	
	gen actual = `outcome'
	



end





cap prog drop get_hold_out_pred_gsem
prog define get_hold_out_pred_gsem
syntax varname, ///
	hold_out_fold(string) ///
	model_code(string) ///
	predictor_waves(numlist) out_wave(integer) ///
	[ ///
		intercept_est(string) ///
		random_split random_study random_slope ///
		model_options(string) ///
		intercept_value(passthru) ///
		simple_model(string) ///
		simple_model_options(string) ///
		mixed_model(string) /// use mixed model to estimte random part of model
		mixed_model_options(string) ///
		mixed_model_extract_values(string) ///
	]
	

	if "`model_options'" != "" local model_options_c , `model_options'
	
	*fit the model using a simpler model for starting values
	if "`simple_model'" != "" {
		di "running simple model"
		if "`simple_model_options'" != "" local simple_model_options_c , `simple_model_options'

		`simple_model' if `varlist' != "`hold_out_fold'" `simple_model_options_c'
		di `"`e(cmdline)'"'
		 mat SIMPLE = e(b)
		 
		`model_code' if `varlist' != "`hold_out_fold'", from(SIMPLE)  `model_options'
		di `"`e(cmdline)'"'

	}
	else if "`mixed_model'" != "" {
	di "running mixed model"
		mixed_first_gsem_rs if `varlist' != "`hold_out_fold'", ///
			gsem_code(`model_code') ///
			gsem_options(`model_options') ///
			mixed_code(`mixed_model') ///
			mixed_model_options(`mixed_model_options')

	}
	else {
		`model_code' if `varlist' != "`hold_out_fold'"  `model_options_c'
	}
	local outcome `e(depvar)'

	gen converged =  e(converged) 
	est store model

	if "`random_split'" != "" {
		local intercept_est "estimate"
		tempvar int_est_marker 
		gen `int_est_marker' = 1 if `varlist' != "`hold_out_fold'"
	
		di "making predictions for random split"
		est_intercept_get_pred `varlist', hold_out_fold(`hold_out_fold') ///
			 predictor_waves(`predictor_waves') out_wave(`out_wave') ///
			 model_name(model)  ///
			 pred_var_name(pred_cv) int_est_marker(`int_est_marker') ///
			 random_split `random_study' ///
			 model_code(`model_code') model_options(`model_options')  ///
			 intercept_est(estimate)
	}
	else {
	
		foreach method in `intercept_est' {
			di "Predicting using `method' intercepts"
			est restore model
			gsem
			if "`method'" == "estimate_cv" {
			tab study
				est_intercept_get_pred_cv `varlist', hold_out_fold(`hold_out_fold') 	predictor_waves(`predictor_waves') out_wave(`out_wave') model_name(model) ///
					`random_split' `random_study' `random_slope'  model_code(`model_code') model_options(`model_options') intercept_est(`method') `intercept_value' 
			}
			else {
				est_intercept_get_pred `varlist', hold_out_fold(`hold_out_fold') 	predictor_waves(`predictor_waves') out_wave(`out_wave') model_name(model) pred_var_name(pred_`method') ///
					`random_split' `random_study' `random_slope' model_code(`model_code') model_options(`model_options') intercept_est(`method') `intercept_value' `predict_args'
			}

		}
			
		*renaming pred to pred if only one method passed.
		local n_est_methods = wordcount(`"`intercept_est'"') 
		if `n_est_methods' == 1 {
		rename pred_`intercept_est' pred
	}
	
	}
	
	
	*For fixed study estimate intercepts 

	
	gen actual = `outcome'

	keep if wave == `out_wave'


	di "finished getting hold out"
end

cap prog drop  est_intercept_get_pred_cv
prog est_intercept_get_pred_cv
syntax varname, hold_out_fold(string) model_name(passthru) ///
	[ ///
	random_split random_study random_slope ///
	model_code(string) model_options(string) ///
	intercept_est(string) intercept_value(passthru) ///
	predictor_waves(passthru) out_wave(passthru) ///
	single_time_point model_command(passthru) ///
	]
	
	tempfile data
	local count = 1

	splitsample if `varlist' == "`hold_out_fold'", generate(int_est_fold)  cluster(ID) nsplit(10)  // nsplit(10) defines the number of folds used in estimate_cv
	qui levelsof int_est_fold, local(folds) 
	foreach fold in `folds' {
		di "Intercept est fold: `fold'"
		
		preserve
		tempvar int_est_marker
		gen `int_est_marker' = 1 if int_est_fold != `fold' | `varlist' != "`hold_out_fold'"
		
			if "`single_time_point'" != "" {
				est_intercept_get_pred_st `varlist', hold_out_fold(`hold_out_fold') `model_name' pred_var_name(pred_estimate_cv) ///
					`random_split' `random_study'  model_code(`model_code') `model_command' model_options(`model_options') intercept_est(estimate) `intercept_value'  int_est_marker(`int_est_marker')	
			}
			else {
				est_intercept_get_pred `varlist', hold_out_fold(`hold_out_fold') 	`predictor_waves' `out_wave' `model_name' pred_var_name(pred_estimate_cv) ///
					`random_split' `random_study'  model_code(`model_code') model_options(`model_options') intercept_est(estimate) `intercept_value'  int_est_marker(`int_est_marker')	
			}

			
		keep if int_est_fold == `fold'
		if `count' == 1 {
			save  `data'
		}
		else {
			append using `data'
			save `data', replace
		}
		restore
		local count = `count' + 1
		
	}
	use `data', clear

	
end


cap prog drop  est_intercept_get_pred
prog est_intercept_get_pred
syntax varname, hold_out_fold(string) predictor_waves(numlist) out_wave(integer) model_name(name) pred_var_name(name) ///
	[random_split random_study random_slope model_code(string) model_options(string) intercept_est(string) intercept_value(passthru)  int_est_marker(passthru)]
	
	est restore `model_name'
	gsem
	
	if "`random_split'" == "" & "`random_study'" == "" {
		di "Fitting constrained gsem model for intercept estimation"
		fit_hold_out_gsem study, model_code(`model_code') hold_out_fold(`hold_out_fold') model_options(`model_options') intercept_est(`intercept_est') `intercept_value'  `int_est_marker'
	}
	di "making predictions"
	di "int est marker: `int_est_marker'"
	di "int est: `intercept_est'"

	predict_multiwave_gsem_uv `pred_var_name' if `varlist' == "`hold_out_fold'", intercept_est(`intercept_est')  `random_study' `random_slope' `int_est_marker' predictor_waves(`predictor_waves') out_wave(`out_wave') 

end

cap prog drop  est_intercept_get_pred_st
prog est_intercept_get_pred_st
syntax varname, hold_out_fold(string)  model_name(name) pred_var_name(name) model_command(string) ///
	[random_split random_study random_slope model_code(string) model_options(string) intercept_est(string) intercept_value(passthru)  int_est_marker(passthru)]
	
	est restore `model_name'
	qui `model_command'
	
	if  "`random_study'" == "" {
		di "Predictions for fixed study, outcome modelled with regression"
		est_intercept_get_pred_reg `varlist', hold_out_fold(`hold_out_fold') pred_var_name(`pred_var_name') intercept_est(`intercept_est') `random_split'  `intercept_value' `int_est_marker'	
	}
	else {
		est_intercept_get_pred_gsem_st `varlist', hold_out_fold(`hold_out_fold') pred_var_name(`pred_var_name') intercept_est(`intercept_est') `random_split'  `intercept_value' `int_est_marker'	
	}

end







cap prog drop predict_multiwave_gsem_uv // to make multivariate loop over outcomes. This is a postestimation command for a gsem.
prog define predict_multiwave_gsem_uv
*predictions for univariate random slope models
syntax name (name = pred_var) [if/], predictor_waves(numlist) out_wave(integer)   [intercept_est(string) random_study random_slope int_est_marker(varname)]
	di "`pred_var'"
	if "`if'" != "" {
		local extra_if & `if'
	}
	
	if "`int_est_marker'" != "" {
		local not_int_est  & `int_est_marker' != 1 // excluding data used in estiamting intercepts from predictions
		local if_int_est   if `int_est_marker' == 1 // this is used as the only if statement so includes if 
	}

	
	local predictor_waves_csv = subinstr("`predictor_waves'", " ", ", ",.)
	di "`predictor_waves_csv'"
	
	
	tempname fixed_outcome rand_int rand_int_all rand_slope rand_slope_all study_intercept study_intercept_all

	*Fixed part
	predict `fixed_outcome' if wave == `out_wave' `not_int_est' `extra_if', conditional(fixedonly)

	di "Fixed prediction"
	su `fixed_outcome'
	
	*Random intercept
	di "predicting random intercept values"

	predict `rand_int' if  inlist(wave, `predictor_waves_csv') `extra_if' `not_int_est' , latent(M1[ID]) 
	bysort ID: egen `rand_int_all' = mean(`rand_int') 
	
	gen `pred_var' = `fixed_outcome' + `rand_int_all' 
	di "Predicted outcome"
	su `pred_var'

	*Random slope
	if "`random_slope'" != "" {
	di "predicting random slope values"
		predict `rand_slope'  if inlist(wave, `predictor_waves_csv') `not_int_est' `extra_if', latent(M2[ID]) 
		bysort ID: egen `rand_slope_all' = mean(`rand_slope') 
		replace `pred_var' = `pred_var' + `rand_slope_all'  if `pred_var' !=.
	
	}
	
	*Study intercept
		// if intercept_est = average - do not estimate study part - treat as zero
		// if intercept = estimate - estimate on intercept_est_data
	if "`intercept_est'" != "average" & "`random_study'" != "" { 
		di "Estimating random study intercept"
		predict `study_intercept'   `if_int_est', latent(STUDY[study]) // added truism 1==1 so there is always a n if statement for 
	    bysort study: egen `study_intercept_all' = mean(`study_intercept') 
		replace `pred_var' = `pred_var' + `study_intercept_all' if `pred_var' !=.
	}

	*drop  `rand_int' `rand_int_all' `fixed_outcome'  `rand_slope' `rand_slope_all'
	
	
end



* This command is about intercept estimation
* It is a post estimation command for gsem.
* It is to be used when fixed intercepts are used.


* Uses hold out data to estimate intercept_est
cap prog drop fit_hold_out_gsem // only works for univariate case at the moment
prog define fit_hold_out_gsem
syntax varname, model_code(string) hold_out_fold(string)  intercept_est(string) [intercept_value(real 0) model_options(string) int_est_marker(varname)]
		* This code extracts the model parameters and then sets constraints for each of the variables in the model

	tempname A
	mat `A' = r(table)
	local n_params = colsof(`A')
	local max_constaint_n = `n_params' +  e(k_dv) 
	
	qui levelsof `varlist', local(folds)
	if !inlist("`intercept_est'", "estimate", "average", "value") {
		di "Intercept est must be one of estimate, average or value"
	}	



	di `n_params'
	local col_eq_names:  coleq `A'
	local colnames:  colnames `A'
	
	constraint drop _all

	*Non hold out folds:
	forvalues i = 1 (1) `n_params' { 
		local colname = word("`colnames'", `i')
		local col_fold = subinstr("`colname'", "`varlist'_", "",.)
		local col_fold = subinstr("`col_fold'", "o.", "",.)
		local eq_name = word("`col_eq_names'", `i')
		local constraint_name _b[`eq_name':`colname']
		
		if "`col_fold'" != "`hold_out_fold'" { //  &  regexm("`col_fold'", "^var\(e\." ) != 1
			local a = `A'[1,`i']
			di "constraint `i' `constraint_name' = `a'"
			constraint `i' `constraint_name' = `a'
		}
	}
	* Handling the hold out fold:
	* if intercept_est is value then intercept_value is taken

	if "`intercept_est'" == "average" {
			qui levelsof `varlist', local(folds)
			local n_folds = wordcount(`"`folds'"') 
			local intercept_total = 0
			local weight_total = 0
			forvalues i = 1 (1) `n_folds' {
				local est = `A'[1,`i']
				if `est' != 0 {
					local weight = 1/(`A'[2,`i'])^2 // inverse variance weight
					local intercept_total = `intercept_total' + `est'*`weight'
					local weight_total = `weight_total' + `weight' // note one study will be ommited and take value 0
				}
			}
			local intercept_value = `intercept_total'/`weight_total' 
		}
		
	if "`intercept_est'" == "average" | "`intercept_est'" == "value" {
		local i = 1
		foreach eq_name in `e(eqnames) ' {	// looping over eqnames for multiple outcomes		
			local	constraint_name_hold_out _b[`eq_name':`varlist'_`hold_out_fold']
			local con_no = `n_params' + `i'
			di `"constraint `con_no'  `constraint_name_hold_out' = `intercept_value' "'

			constraint `con_no'  `constraint_name_hold_out' = `intercept_value'
			local i = `i' + 1
		}
		
		`model_code', constraints(1 (1) `max_constaint_n') `model_options' noestimate  // use this model for predicting

	}
	else { // This model is run if intercept est is "estimate - estiamtes the intercept for the hodl out fold, as this is the only fold that is unconstrained"
		if "`int_est_marker'" != "" {
		 local if_int_est if `int_est_marker'  == 1
		}
		`model_code' `if_int_est', constraints(1 (1) `max_constaint_n')   `model_options'  // use this model for predicting
	}




	


end




cap prog drop est_intercept_get_pred_reg
prog define est_intercept_get_pred_reg
syntax varname, ///
	hold_out_fold(string) ///
	pred_var_name(string) ///
	[ ///
		intercept_est(string) ///
		random_split ///
		predict_function(string) ///
		intercept_value(real 0) ///
		int_est_marker(varname) ///
	]

	if !inlist("`intercept_est'", "estimate", "average", "value") {
		di `"`intercept_est'"'
		di "Intercept est must be one of estimate, average or value"
	}	

	if "`int_est_marker'" != "" {
		local and_int_est & `int_est_marker' ==1
	}
	
	local outcome `e(depvar)'


	tempname A
	mat `A' = r(table)
	
	* make predictions in hold out fold
	predict `pred_var_name' if `varlist' == "`hold_out_fold'" 
	
	if "`random_split'" == "" {
		
		di `"INTERCEPT EST: `intercept_est'"'

		if "`intercept_est'" == "estimate" {
			
			di "estimating intercept"
			su `pred_var_name' if `varlist' == "`hold_out_fold'" `and_int_est'
			local p_mean = r(mean)
			su `e(depvar)' if `varlist' == "`hold_out_fold'" `and_int_est'
			local o_mean = r(mean)
			local new_intercept = `o_mean' - `p_mean'
			
			replace `pred_var_name' = `pred_var_name' + `new_intercept' if `varlist' == "`hold_out_fold'" 
		}
		
		
		if "`intercept_est'" == "average" {
			qui levelsof `varlist', local(folds)
			local n_folds = wordcount(`"`folds'"') 
			local intercept_total = 0
			local weight_total = 0
			forvalues i = 1 (1) `n_folds' {
				local est = `A'[1,`i']
				if `est' != 0 {
					local weight = 1/(`A'[2,`i'])^2 // inverse variance weight
					local intercept_total = `intercept_total' + `est'*`weight'
					local weight_total = `weight_total' + `weight' // note one study will be ommited and take value 0
				}
			}
			local intercept_value = `intercept_total'/`weight_total' 
				
		}
		if "`intercept_est'" == "value" | "`intercept_est'" == "average" {
			di "Adding new intercept: `intercept_value'"
			replace `pred_var_name' = `pred_var_name' + `intercept_value'
		}

	}

	
	
end

cap prog drop est_intercept_get_pred_gsem_st 
prog define est_intercept_get_pred_gsem_st 

syntax varname, ///
	hold_out_fold(string) ///
	pred_var_name(string) ///
	[ ///
		intercept_est(string) ///
		random_split ///
		predict_function(string) ///
		int_est_marker(varname) ///
	]
	
	if !inlist("`intercept_est'", "estimate", "average", "") {
		di `"`intercept_est'"'
		di "Intercept est must be one of estimate or average"
	}
	

	if "`int_est_marker'" != "" {
		local if_int_est if `int_est_marker' ==1
	}
	* Predict fixed
	predict `pred_var_name' , conditional(fixedonly)
	
	
	
	* Predict intercept
	if "`intercept_est'" == "estimate"  {
		di "Estimating random study intercept"
		tempvar study_intercept study_intercept_all
		predict `study_intercept' `if_int_est', latent(STUDY[study]) 
	    bysort study: egen `study_intercept_all' = mean(`study_intercept') 
		replace `pred_var_name' = `pred_var_name' + `study_intercept_all' if `pred_var_name' !=.
				
	}
	
	
	
end



cap prog drop mixed_first_gsem_rs
prog define mixed_first_gsem_rs
syntax [if],  gsem_code(string) mixed_code(string) [gsem_options(string) mixed_model_options(string)]
   if "`mixed_model_options'" != "" local mixed_model_options_c , `mixed_model_options'
	di `"`mixed_code' `if' `mixed_model_options_c'"'
	`mixed_code' `if' `mixed_model_options_c'
	mixed_extract_prog
	
	`gsem_code' `if', `gsem_options' var(M1[ID]@`r(var_ri)' M2[ID]@`r(var_rs)') cov(M1[ID]*M2[ID]@`r(cov_rs_ri)')


end

 cap prog drop mixed_extract_prog
  prog define mixed_extract_prog, rclass
    mat A = r(table)
	estat sd
	mat B = r(table)
	di colsof(A)
	di colsof(B)
	
    local ncol  colsof(A)
    local ncol_rs = `ncol' - 3
    local ncol_ri = `ncol' - 2
    local ncol_cov = `ncol' - 1
	
	local cov_rs_ri = A[1,`ncol_cov']  
	local corr_rs_ri = B[1,`ncol_cov']  
		di "corr: `corr_rs_ri'"

	if `corr_rs_ri' >= 0.999 {
		di "Adjusting covariance between random slope and intercept as estimated correlation great or equal than 1"
		local cov_rs_ri =  `cov_rs_ri'*0.99

	}

         
     return local var_ri = A[1,`ncol_ri']
     return local var_rs = A[1,`ncol_rs']
     return local cov_rs_ri = A[1,`ncol_cov']  
end
