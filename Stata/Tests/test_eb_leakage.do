/*
Test for leakage of unseen outcome values into empirical Bayes (EB) predictions made by predict_multiwave_gsem_uv.

predict_multiwave_gsem_uv predicts random intercepts/slopes with
	predict ... if inlist(wave, predictor_waves), latent(M1[ID])
while the held out rows still contain observed outcome values at the outcome wave (and at other non-predictor waves, e.g. wave 1).
If gsem uses all of a person's rows (not just those satisfying the if) to compute the EB posterior, the outcome leaks into its own prediction.

Method: for each model, predictions are made twice for the held out data:
	A: data as is
	B: outcome set to missing in every held out row that is not a predictor wave
If gsem only uses the predictor waves, A and B are identical. Any difference = leakage.

All comparisons are run, then the do file asserts that no leakage was detected.
*/

clear all
set seed 8461

local outcome vabs_com_ae
local hold_out EDX // study held out, mimicking one IECV fold. EDX has observations at all waves, including wave 1.
global leak_tol 1e-8
global n_leak = 0

qui do "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/R/prediction_for_autistic_children/Stata/prediction_progs.do"

cd "/Users/k1811974/Library/CloudStorage/OneDrive-King'sCollegeLondon/PhD/WP4 IPD Meta Analysis/Data and Outputs/Publication/Derived Data"


* Helper programs ----

* Compares two prediction variables and records whether they differ
cap prog drop check_no_leakage
prog define check_no_leakage
syntax varlist(min=2 max=2) [if], label(string)
	tokenize `varlist'
	tempvar diff
	gen double `diff' = abs(`1' - `2') `if'
	qui su `diff'
	local n = r(N)
	local max_diff = r(max)
	di as text _n "`label': compared `n' predictions, max abs difference = " as result `max_diff'
	if `n' == 0 {
		di as error "`label': no predictions to compare - check the test set up"
		global n_leak = $n_leak + 1
	}
	else if `max_diff' > $leak_tol {
		di as error "`label': LEAKAGE DETECTED - predictions change when unseen outcome values are removed"
		global n_leak = $n_leak + 1
	}
	else {
		di as text "`label': no leakage"
	}
end

* Runs predict_multiwave_gsem_uv with and without unseen outcome values and compares the predictions
* holdout() is the condition defining the rows being predicted
cap prog drop run_leak_test
prog define run_leak_test
syntax, label(string) outcome(varname) holdout(string asis) [pred_options(string)]
	* A: outcome present in all rows
	predict_multiwave_gsem_uv pred_a if `holdout', out_wave(2) predictor_waves(0 -1) `pred_options'

	* B: outcome removed from held out rows that are not predictor waves
	clonevar outcome_keep = `outcome'
	replace `outcome' = . if `holdout' & !inlist(wave, 0, -1)
	predict_multiwave_gsem_uv pred_b if `holdout', out_wave(2) predictor_waves(0 -1) `pred_options'
	replace `outcome' = outcome_keep
	drop outcome_keep

	check_no_leakage pred_a pred_b if wave == 2 & `holdout', label(`label')
	drop pred_a pred_b
end


* Data ----
use "pooled_vabs.dta", clear
keep if base_all_complete == 1 & out_all_complete == 1 & all_complete == 1

* gsem latent variables need numeric group variables. String versions are kept for if conditions.
* The string copy of study must not start with study_ or it is picked up by the study_* dummies.
rename ID ID_str
encode ID_str, gen(ID)
rename study studyname
encode studyname, gen(study)

tab studyname wave


* 1. Raw gsem check, independent of prediction_progs.do ----
gsem (`outcome' <- study_* age_c M1[ID]@1) if studyname != "`hold_out'", nocons

predict ri_a if studyname == "`hold_out'" & inlist(wave, 0, -1), latent(M1[ID])
clonevar outcome_keep = `outcome'
replace `outcome' = . if studyname == "`hold_out'" & !inlist(wave, 0, -1)
predict ri_b if studyname == "`hold_out'" & inlist(wave, 0, -1), latent(M1[ID])
replace `outcome' = outcome_keep
drop outcome_keep
check_no_leakage ri_a ri_b, label("Raw gsem predict latent(M1[ID])")
drop ri_a ri_b


* 2. Fixed study, random intercept (IECV) ----
gsem (`outcome' <- study_* age_c M1[ID]@1) if studyname != "`hold_out'", nocons
run_leak_test, label("Fixed study, random intercept, IECV") outcome(`outcome') ///
	holdout(studyname == "`hold_out'")


* 3. Fixed study, random slope (IECV) ----
gsem (`outcome' <- study_* age_c M1[ID]@1 c.age_c#M2[ID]@1) if studyname != "`hold_out'", nocons iterate(50) // estimates need not converge for this test
run_leak_test, label("Fixed study, random slope, IECV") outcome(`outcome') ///
	holdout(studyname == "`hold_out'") pred_options(random_slope)


* 4. Random study, random intercept, average intercept (IECV) ----
gsem (`outcome' <- age_c STUDY[study]@1 M1[ID]@1) if studyname != "`hold_out'"
run_leak_test, label("Random study, random intercept, average, IECV") outcome(`outcome') ///
	holdout(studyname == "`hold_out'") pred_options(random_study intercept_est(average))


* 5. Random study, random intercept, estimate_cv intercept (IECV) ----
* Study intercept is estimated from rows with int_est_marker == 1 (training studies + 9/10 of the held out study);
* predictions are made for the remaining 1/10, whose unseen outcomes are removed in run B.
splitsample if studyname == "`hold_out'", generate(int_est_fold) cluster(ID) nsplit(10)
gen int_est_marker = 1 if int_est_fold != 1 | studyname != "`hold_out'"
run_leak_test, label("Random study, random intercept, estimate_cv, IECV") outcome(`outcome') ///
	holdout(studyname == "`hold_out'" & int_est_marker != 1) ///
	pred_options(random_study intercept_est(estimate) int_est_marker(int_est_marker))
drop int_est_fold int_est_marker


* 6. Fixed study, random intercept (random CV) ----
splitsample, generate(fold) cluster(ID) nsplit(5)
gen int_est_marker = 1 if fold != 1
gsem (`outcome' <- study_* age_c M1[ID]@1) if fold != 1, nocons
run_leak_test, label("Fixed study, random intercept, random CV") outcome(`outcome') ///
	holdout(fold == 1) pred_options(int_est_marker(int_est_marker))
drop fold int_est_marker


* Result ----
di _n "Number of comparisons with leakage or set up problems: $n_leak"
assert $n_leak == 0
