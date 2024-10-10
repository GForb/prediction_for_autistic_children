cap prog drop predict_multiwave_gsem

prog define predict_multiwave_gsem
syntax name (name = pred_var) [if/], predictor_waves(numlist) out_wave(integer) id_var(varname)  wave_var(varname)
	di "`pred_var'"
	if "`if'" != "" {
		local extra_if & `if'
	}
	
	
	local predictor_waves_csv = subinstr("`predictor_waves'", " ", ", ",.)
	di "`predictor_waves_csv'"
	
	local outcomes `e(eqnames)'
	local n_outcomes = wordcount("`outcomes'")
	
	 
	
	tempname all random_part fixed_part fixed_outcome sd_random random_part_all

	predict `all'* if inlist(`wave_var', `predictor_waves_csv') `extra_if'
	predict `fixed_part'* if inlist(`wave_var', `predictor_waves_csv') `extra_if',  ///
		conditional(fixedonly)
	
	predict `fixed_outcome'* if `wave_var' == `out_wave' `extra_if', conditional(fixedonly)
	

	forvalues i = 1 (1) `n_outcomes' {
		gen `random_part'`i' = `all'`i' - `fixed_part'`i'

		bysort `id_var': egen `sd_random'`i' = sd(`random_part'`i')
		
		if wordcount("`predictor_waves'") > 1{
			di "Checking variablily of random part within IDs (should be v close to zero)"
			su `sd_random'`i'
			if r(sd) > 0.0001  {
			di as error "prediction of random part of the model varies within ids"
			error
			}
		}
		
		bysort `id_var': egen `random_part_all'`i' = mean(`random_part'`i') 
	
	
		local outcome_name = word("`outcomes'", `i')
		gen `pred_var'_`outcome_name' = `fixed_outcome'`i' + `random_part_all'`i'
	}

	

	
	
	drop `all'* `random_part'* `fixed_part'* `fixed_outcome'* `sd_random'* `random_part_all'*
	

	
end
