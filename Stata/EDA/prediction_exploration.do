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

gen y1 = latent  + latent1 +age*rand_slope1 -study/2 + rnormal(0, 1)
gen y2 = latent  + latent2 + age*0.5 + 0.5 -study/4 + rnormal(0, 1)
gen y3 = latent  + latent3 + age*0.25 + 2 - study  + rnormal(0, 1)


gsem (y1 <- i.study age M1[id]@1)
cap drop pred_uv_multi3*
predict_multiwave_gsem pred_uv_multi3, predictor_waves(1 2 3) out_wave(4) id_var(id)  wave_var(time)
su pred_uv_multi3*

cap drop pred_iecv_uv*
get_all_preds study, ///
	model_code(gsem (y1 <- i.study age M1[id]@1)) ///
	pred_var_name(pred_iecv_uv) ///
	predict_function(predict_multiwave_gsem) ///
	predict_args(predictor_waves(1 2 3) out_wave(4) id_var(id)  wave_var(time))

cap drop res_uv

gen res_uv = pred_iecv_uv - y1

* Expect for outcome 1, means to be 0, 0.5, 1, 1.5, 2, 
*            outcome 2: 0, 0.25, 0.5, 0.75, 1
*            outcome 4: 0, 1, 2, 3, 4, 5
bysort study: su res_uv*

gsem (y1 <- i.study age M1[id]@1) (y2 <- i.study age M2[id]@1) (y3 <- i.study age M3[id]@1)
cap drop pred_mv_multi*
predict_multiwave_gsem pred_mv_multi3, predictor_waves(1 2 3) out_wave(4) id_var(id)   wave_var(time)
su pred_mv_multi3* y* if time == 4

predict_multiwave_gsem pred_mv_multi1, predictor_waves(3) out_wave(4) id_var(id)  wave_var(time)
su pred_mv_multi1* y* if time == 4

cap drop pred_iecv*
get_all_preds study, ///
	model_code(gsem (y1 <- i.study age M1[id]@1) (y2 <- i.study age M2[id]@1) (y3 <- i.study age M3[id]@1)) ///
	pred_var_name(pred_iecv) ///
	predict_function(predict_multiwave_gsem) ///
	predict_args(predictor_waves(3) out_wave(4) id_var(id)  wave_var(time))

forvalues i = 1 (1) 3 {
	gen res`i' = pred_iecv_y`i' - y`i'
}

* Expect for outcome 1, means to be 0, 0.5, 1, 1.5, 2, 
*            outcome 2: 0, 0.25, 0.5, 0.75, 1
*            outcome 4: 0, 1, 2, 3, 4, 5
bysort study: su res*

* Exploring predict functions
predict pred_default*
su pred_default* if study ==4


predict pred_marginal*, marginal
su pred_marginal*

predict pred_fixed*, conditional(fixedonly)
su pred_fixed* if study ==4



predict pred_latent*, latent
su pred_latent*

forvalues i = 1 (1) 3 {
	gen my_predict`i' = pred_fixed`i' + pred_latent
	gen check`i' = my_predict`i' - pred_default`i'
	gen error`i' = pred_default`i' - y`i'

 }
 
bysort study: su pred_default* my* check*

bysort study: su error*

get_hold_out_pred study, ///
	hold_out_study(4) ///
	model_code(gsem (y1 <- i.study M1@1) (y2 <- i.study M1@1) (y3 <- i.study M1@1)) ///
	pred_var_name(pred_ado)

get_all_preds study, ///
	model_code(gsem (y1 <- i.study M1@1) (y2 <- i.study M1@1) (y3 <- i.study M1@1)) ///
	pred_var_name(pred_all)
	
bysort study: su pred_ado* pred_all* pred_default* 




*Q does the inclusion of a study intercept change the value of the latent variable
