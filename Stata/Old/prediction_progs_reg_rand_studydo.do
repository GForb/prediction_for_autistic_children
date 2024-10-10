cap prog drop get_all_preds_ri
prog define get_all_preds_ri
syntax varname, model_code(passthru) pred_var_name(passthru) intercept_est(passthru)  [ random_split  model_options(passthru) intercept_value(passthru)]
	
	tempfile data_with_preds
	

	qui levelsof  `varlist', local(folds)
	local count = 1
	foreach fold in `folds' {

		di "Fold:  `fold'"
		preserve
		get_hold_out_pred_ri `varlist', hold_out_fold(`fold') `model_code' `pred_var_name' `predict_args' `predict_function' `model_options' `random_split' `intercept_est'
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
	gen converged = 1 // regression doesn't return a converged value - always assume convergence	

	
end

cap prog drop get_hold_out_pred_ri
*Fit model

*If not random split: estimate intercept for hold out folds

* make predictions // can be in the predict fucntion...


prog define get_hold_out_pred_ri
syntax varname, ///
	hold_out_fold(string) ///
	model_code(string) ///
	pred_var_name(string) ///
	intercept_est(string) ///
	[ ///
		random_split ///
		predict_function(string) ///
		predict_args(string) ///
		model_options(string) ///
		intercept_value(real 0) ///
	]

	if !inlist("`intercept_est'", "estimate", "average", "value") {
		di `"`intercept_est'"'
		di "Intercept est must be one of estimate, average or value"
	}	

	if "`model_options'" != "" local model_options_c , `model_options'

	
	*fit the model
	`model_code' if `varlist' != "`hold_out_fold'"  `model_options_c'
	local outcome `e(depvar)'

	if "`random_split'" == "" {
		di `"INTERCEPT EST: `intercept_est'"'

		if "`intercept_est'" == "estimate" {
			predict `pred_var_name'
		}
		
		
		if "`intercept_est'" == "average" {
			predict `pred_var_name', fixedonly

		}
	}
	di "Outcome macro is: `outcome'"
	gen actual = `outcome'
	
end



