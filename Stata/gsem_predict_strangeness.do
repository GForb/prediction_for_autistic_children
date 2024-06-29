clear
local n_obs 4000 
set obs 4000
gen latent = rnormal(0,1)
gen shared_error = rnormal(0, 0.5)
gen study = round(_n/(`n_obs'/4))
gen id = _n
gen latent1 = rnormal(0,1)
gen latent2 = rnormal(0,1)
gen latent3 = rnormal(0,2)
gen rand_slope1 = latent1/4 + rnormal(0, 0.25)

expand 4
bysort id: gen time = _n
gen age = time + rnormal(0, 0.25)

gen y1 = latent  + latent1 +age*1 -study/2 + rnormal(0, 1)
gen y2 = latent  + latent2 + age*0.5 + 0.5 -study/4 + rnormal(0, 1)
gen y3 = latent  + latent3 + age*0.25 + 2 - study  + rnormal(0, 1)


gsem (y1 <- i.study age M1[id]@1) if study != 4

predict all
predict fixed, conditional(fixedonly)
gen random = all - fixed

gen res_all = all - y1
gen res_fixed = fixed - y1
gen res_random = random - y1

bysort study: su res* random


* What would I like to see:


*What am I seeing: Using predict on gsem sets the study variable to zero. This leads to errors from the fixed prediction for the hold out study being equal to the study parameter - in this case 2. This error is compensated for in the random part of the model. This leads to estimated random intercepts having mena not equal to zero. At the end of the do file I calculate, for a simple random intercept model, what the empirical bayes prediction of this part of the model will be - it is equal to the mean of the random intercept model. I am concerned that this could lead to error in estimation of random slope parameters as when the random slope is correlated with the random intercept, the random intercept value will feed into the slope. Bundling the study intercept into the random intercept could cause bias in slope estimates and a loss of predictive performance. It remains to be seen if this happens. It will take some digging to figure this out.

*This problem will be greatest when there are few obs/person, the correlation between intercept and slopes is strong, and the variance of the random slope parameter is large.

* Could other model miscalibration be delt with in this way? What if slopes take into account unmodelled between study hetrogeneity in treatment effect? This is wild.

*In the simplest case empirical bayes prediciton involves estimating the fixed part of the model, calculating the residual for that person, then offsetting 

* What about when predicting an unseen outcome - the the empirical bayes prediciton for the study interept (and indvidual) is based soley on the observed data - ie. we have an observed outcome for that individual (baseline).

*Is this fine?
* I don't think so. The test data is being used to estimate the intercept (in the random intercept)

* Is this using data from multiple individuals - answer no - see the example where I restrict predictions to indiviudal 3500 - they are the same as predictions made from everyone.

* I want to make a prediciton for an unseen outcome, in an individual in a new study, for which I have observed no data. First I collect some data on that indivual - baseline obs. I then use these to estimate a intercept for that indivual. If I've set study to zero, this will include any information on the intercept. Similarly, if I have engouh observations to estimate a slope, this will give info on the slope.

* Push on as is. Think about what to do with study for the new data. 

*prediction for single individual in study 4
predict all_1 if id ==3500

su all* if id ==3500

* Does it change using less info per person

predict all_t3 if time == 3
predict fixed_t3 if time == 3, conditional(fixedonly)

gen res3 = all_t3 - y1
gen res_fixed3 = fixed - y1

bysort study: su res_all res3 res_fixed res_fixed3 if time ==3

* could this cause problems with random slopes... If the random slope is estiamted to be correlated with intercepts then ... 

gsem (y1 <- i.study age M1[id]@1 c.age#M2[id]) if study != 4

cap drop all* res* fixed* random*


predict all
predict fixed, conditional(fixedonly)
gen random = all - fixed

gen res_all = all - y1
gen res_fixed = fixed - y1
gen res_random = random - y1

bysort study: su res* random


* What would I like to see:


*What am I seeing: Using predict on gsem sets the study variable to zero. This leads to errors from the fixed prediction for the hold out study being equal to the study parameter - in this case 2. This error is compensated for in the random part of the model. This leads to estimated random intercepts having mena not equal to zero.

*Is this fine?
* I don't think so. The test data is being used to estimate the intercept (in the random intercept)

* Is this using data from multiple individuals

*prediction for single individual in study 4
predict all_1 if id ==3500

su all* if id ==3500

* Does it change using less info per person

predict all_t3 if time == 3
predict fixed_t3 if time == 3, conditional(fixedonly)

gen res3 = all_t3 - y1
gen res_fixed3 = fixed - y1

bysort study: su res_all res3 res_fixed res_fixed3 if time ==3

* What is going on:

* when making predictions gsem is able to incorperate some information about the residual into the random part of the model. I can look at this algebraicly with a random intercept model. EB = error*some sort of icc 

*using mixed to estiamte ICC:

mixed y1 i.study age || id:
*ICC of 0.75
* penalisation value R of 

*Calculating R
mat A = r(table)
scalar sigma2_u = A[1,8]
scalar sigma2_e = A[1,9]
di sigma2_u
di sigma2_e

scalar n_obs = 4

scalar R = sigma2_u/(sigma2_u + sigma2_e/n_obs)

di R
di R*2

di (1-R)*2

su res* random if study == 4

* WHen a single observation is used:
scalar n_obs = 1
scalar R = sigma2_u/(sigma2_u + sigma2_e/n_obs)

di R
di R*2

di (1-R)*2

estat icc

*

regress y1 i.study  if study !=4
cap drop pred_reg
predict pred_reg 
su pred_reg if study == 4
su pred_reg if study == 0

* stata automaticly sets values to zero for unseen catagories - good to know!


* Making gsem do what I want - this is the solution. You can make empirical bayes predictions with any value for the new study intercept. Below I estimate it from the data.
* How to automate???

 cap drop study?
forvalues i = 1(1) 4{
	gen study`i' = 0
	replace study`i' = 1 if study == `i'
}

gsem (y1 <- study1 study2 study3 age M1[id]@1) if study != 4
mat A = r(table)

cap drop all_orig fixed fixed_res
predict all_orig
predict fixed, conditional(fixedonly)
gen fixed_res = fixed - y1
su fixed_res if study ==4
local est_intercept = r(mean)


gsem (y1 <- study1 study2 study3 study4 age@ M1[id]), noestimate

	
forvalues i = 1(1)8 {
	local a`i' = A[1,`i']
	di `a`i''
}
constraint 1 _b[y1:study1] = `a1'
constraint 2 _b[y1:study2] = `a2'
constraint 3 _b[y1:study3] = `a3'
constraint 4 _b[y1:study4] = `est_intercept'
constraint 5 _b[y1:age]  = `a4'
constraint 6 _b[y1:M1[id]]  = 1
constraint 7 _b[y1:_cons]  = `a6'
constraint 8 _b[/var(M1[id])]  = `a7'
constraint 9 _b[/var(e.y1)]  = `a8'

gsem (y1 <- study1 study2 study3 study4 age M1[id]), constraints(1 2 3 4 5 6 7 8 9)
cap drop all_new 
cap drop fixed_new
predict all_new
predict fixed_new, conditional(fixedonly)
gen fixed_new_res = fixed_new -y1

bysort study: su all_new all_orig fixed fixed_res fixed_new*



gsem (y1 <- study1 study2 study3 study4 age M1[id]@1) if study != 4, noestimate


local hold_out_fold = 4
local intercept_value = 0


* It is necessary to split study (rather than using the factor i.study to ensure all studies are included)
gsem (y1 <- study1 study2 study3 study4 age M1[id]@1) if study != `hold_out_fold' // this is the model

* This code extracts the model parameters and then sets constraints for each of the variables in the model
mat A = r(table)
local n_params = rowsof(A)
di `n_params'
local col_eq_names:  coleq A
local colnames:  colnames A

forvalues i = 1 (1) `n_params' { 
	di `i'
	local colname = word("`colnames'", `i')
	local eq_name = word("`col_eq_names'", `i')
	local constraint_name _b[`eq_name':`colname']
	
	if `i' != `hold_out_fold' {
			local a = A[1,`i']
	}
	else {
		local a = `intercept_value'
	}
	di "constraint `i' `constraint_name': `a'"

	constraint `i' `constraint_name' = `a'
}

* The constraint for the hold out fold throws an error. This leads to it being unconstrained. The estimate from this model is the same as the residual from the fixed part - ie. the intercept, estimated using the test data.
gsem (y1 <- study1 study2 study3 study4 age M1[id]@1), constraints(1 (1) `n_params') // use this model for predicting







