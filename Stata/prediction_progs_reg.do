
cap prog drop get_all_preds_reg
prog define get_all_preds_reg
syntax varname, model_code(passthru) pred_var_name(passthru) intercept_est(passthru)  [ random_split  model_options(passthru) intercept_value(passthru)]
	
	tempfile data_with_preds
	

	qui levelsof  `varlist', local(folds)
	local count = 1
	foreach fold in `folds' {

		di "Fold:  `fold'"
		preserve
		get_hold_out_pred_reg `varlist', hold_out_fold(`fold') `model_code' `pred_var_name' `predict_args' `predict_function' `model_options' `random_split' `intercept_est'
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

cap prog drop get_hold_out_pred_reg
*Fit model

*If not random split: estimate intercept for hold out folds

* make predictions // can be in the predict fucntion...


prog define get_hold_out_pred_reg
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


	tempname A
	mat `A' = r(table)
	
	* make predictions in hold out fold
	predict `pred_var_name' if `varlist' == "`hold_out_fold'" 
	
	if "`random_split'" == "" {
		
		di `"INTERCEPT EST: `intercept_est'"'

		if "`intercept_est'" == "estimate" {
			
			di "estimating intercept"
			su `pred_var_name' if `varlist' == "`hold_out_fold'" 
			local p_mean = r(mean)
			su `e(depvar)' if `varlist' == "`hold_out_fold'" 
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
	di "Outcome macro is: `outcome'"
	gen actual = `outcome'
	
end



