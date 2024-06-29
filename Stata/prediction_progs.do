cap prog drop one_hot_encode
prog define one_hot_encode
syntax varname

qui levelsof `varlist', local(levels)

foreach level in `levels' {
	gen `varlist'_`level' = 0
	replace `varlist'_`level' = 1 if `varlist' == "`level'"
}


end


cap prog drop get_all_preds
prog define get_all_preds
syntax varname, model_code(passthru) pred_var_name(passthru) intercept_est(passthru)  [ random_split predict_function(passthru) predict_args(passthru) model_options(passthru) intercept_value(passthru)]
	
	tempfile data_with_preds
	

	
	qui levelsof  `varlist', local(folds)
	local count = 1
	foreach fold in `folds' {

		di "Fold:  `fold'"
		preserve
		get_hold_out_pred `varlist', hold_out_fold(`fold') `model_code' `pred_var_name' `predict_args' `predict_function' `model_options' `random_split' `intercept_est'
		keep if `varlist' == "`fold'"
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
	di "loading data"
	use `data_with_preds', clear
	

	
end



cap prog drop get_hold_out_pred
prog define get_hold_out_pred
syntax varname, ///
	hold_out_fold(string) ///
	model_code(passthru) ///
	pred_var_name(string) ///
	intercept_est(passthru) ///
	[ ///
		random_split ///
		predict_function(string) ///
		predict_args(string) ///
		model_options(passthru) ///
		intercept_value(passthru) ///
	]
	
	di "`predict_function'"
	if "`predict_function'" == "" {
		local predict_function predict
	}
	
	if "`random_split'" == "" fit_hold_out_gsem `varlist', `model_code' hold_out_fold(`hold_out_fold') `intercept_est' `intercept_value' `model_options'
	if "`random_split'" != "" {
		if "`model_options'" != "" local model_options_c ,`model_options'
		`model_code' if `varlist' != "`hold_out_fold'"  `model_options_c'
	}
	if "`predict_args'"  != "" local predict_args_c , `predict_args'
	`predict_function' `pred_var_name' if `varlist' == "`hold_out_fold'" `predict_args_c'
	
end

cap prog drop fit_hold_out_gsem // only works for univariate case at the moment
prog define fit_hold_out_gsem
syntax varname, model_code(string) hold_out_fold(string) intercept_est(string) [intercept_value(real 0) model_options(string)]
	qui levelsof `varlist', local(folds)
	if !inlist("`intercept_est'", "estimate", "average", "value") {
		di "Intercept est must be one of estimate, average or value"
	}
	
	if "`model_options'" != "" local model_options_c ,`model_options' 
	
	
	*Show eq names
	`model_code' if `varlist' != "`hold_out_fold'",  noestimate     `model_options'
	
	`model_code' if `varlist' != "`hold_out_fold'"  `model_options_c'
	di  `"'`e(cmdline)'"'
	
	

	* This code extracts the model parameters and then sets constraints for each of the variables in the model
	tempname A
	mat `A' = r(table)
	local n_params = colsof(`A')
	local max_constaint_n = `n_params' +  e(k_dv)

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
		
		if "`col_fold'" != "`hold_out_fold'" {
			local a = `A'[1,`i']
			di "constraint `i' `constraint_name' = `a'"
			constraint `i' `constraint_name' = `a'
		}
	}
	* Handling the hold out fold:
	* if intercept_est is value then intercept_value is taken

	if "`intercept_est'" == "average" {
		local n_folds = wordcount(`"`folds'"') 
		local intercept_total = 0
		forvalues i = 1 (1) `n_folds' {
			local intercept_total = `intercept_total' + `A'[1,`i'] // note one study will be ommited and take value 0
		}
		local intercept_value = `intercept_total'/(`n_folds' -1) // this is taking a crude average - may need weighting if appropriate.
	}
	if "`intercept_est'" == "average" | "`intercept_est'" == "value" {
		local i = 1
		foreach eq_name in `e(eqnames) ' {			
			local	constraint_name_hold_out _b[`eq_name':`varlist'_`hold_out_fold']
			local con_no = `n_params' + `i'
			di `"constraint `con_no'  `constraint_name_hold_out' = `intercept_value' "'

			constraint `con_no'  `constraint_name_hold_out' = `intercept_value'
			local i = `i' + 1
		}
		
		`model_code', constraints(1 (1) `max_constaint_n') `model_options' noestimate  // use this model for predicting

	}
	else {
		`model_code', constraints(1 (1) `max_constaint_n') `model_options'  // use this model for predicting
	}


* The constraint for the hold out fold throws an error. This leads to it being unconstrained. The estimate from this model is the same as the residual from the fixed part - ie. the intercept, estimated using the test data.
	


end



cap prog drop predict_multiwave_gsem_ri

prog define predict_multiwave_gsem_ri
syntax name (name = pred_var) [if/], predictor_waves(numlist) out_wave(integer) id_var(varname)  wave_var(varname)
	di "`pred_var'"
	if "`if'" != "" {
		local extra_if & `if'
	}
	
	local predictor_waves_csv = subinstr("`predictor_waves'", " ", ", ",.)
	di "`predictor_waves_csv'"
	
	local outcomes `e(eqnames)'
	local n_outcomes = wordcount("`outcomes'")
	
	tempname all random_part fixed_part fixed_outcome  random_part_all

	predict `all'* if inlist(`wave_var', `predictor_waves_csv') `extra_if'
	predict `fixed_part'* if inlist(`wave_var', `predictor_waves_csv') `extra_if',  ///
		conditional(fixedonly)
	
	predict `fixed_outcome'* if `wave_var' == `out_wave' `extra_if', conditional(fixedonly)
	

	forvalues i = 1 (1) `n_outcomes' {
		gen `random_part'`i' = `all'`i' - `fixed_part'`i'
		
		bysort `id_var': egen `random_part_all'`i' = mean(`random_part'`i') 
	
	
		local outcome_name = word("`outcomes'", `i')
		gen `pred_var'_`outcome_name' = `fixed_outcome'`i' + `random_part_all'`i'
	}

	drop `all'* `random_part'* `fixed_part'* `fixed_outcome'*  `random_part_all'*
	
	keep if `wave_var' == `out_wave'
	
end

cap prog drop predict_multiwave_gsem_rs_uv
prog define predict_multiwave_gsem_rs_uv
*predictions for univariate random slope models
syntax name (name = pred_var) [if/], predictor_waves(numlist) out_wave(integer) id_var(varname)  wave_var(varname)
	di "`pred_var'"
	if "`if'" != "" {
		local extra_if & `if'
	}
	
	local predictor_waves_csv = subinstr("`predictor_waves'", " ", ", ",.)
	di "`predictor_waves_csv'"
	

	
	tempname fixed_outcome rand_int rand_int_all rand_slope rand_slope_all

	predict `rand_int' if  inlist(`wave_var', `predictor_waves_csv') `extra_if' , latent(M1[`id_var']) 
	bysort `id_var': egen `rand_int_all' = mean(`rand_int') 

	predict `rand_slope'  if inlist(`wave_var', `predictor_waves_csv') `extra_if', latent(M2[`id_var']) 
	bysort `id_var': egen `rand_slope_all' = mean(`rand_slope') 


	predict `fixed_outcome' if `wave_var' == `out_wave' `extra_if', conditional(fixedonly)
	gen `pred_var' = `fixed_outcome' + `rand_int_all' + `rand_slope_all' 
	
	

	*drop  `rand_int' `rand_int_all' `fixed_outcome'  `rand_slope' `rand_slope_all'
	
	keep if `wave_var' == `out_wave'
	
end


