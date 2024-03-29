

lsac_wave_1_raw <- haven::read_sas(file.path(raw_data, "LSAC/General Release/Survey data/SAS/lsacgrk4.sas7bdat")) 
lsac_wave_2 <- haven::read_sas(file.path(raw_data, "LSAC/General Release/Survey data/SAS/lsacgrk6.sas7bdat"))
lsac_wave_2.5 <- haven::read_sas(file.path(raw_data, "LSAC/General Release/Survey data/SAS/lsacgrk7.sas7bdat"))
lsac_wave_3 <- haven::read_sas(file.path(raw_data, "LSAC/General Release/Survey data/SAS/lsacgrk8.sas7bdat"))
lsac_wave_3.5 <- haven::read_sas(file.path(raw_data, "LSAC/General Release/Survey data/SAS/lsacgrk9.sas7bdat"))
lsac_wave_4 <- haven::read_sas(file.path(raw_data, "LSAC/General Release/Survey data/SAS/lsacgrk10.sas7bdat"))
lsac_wave_5 <- haven::read_sas(file.path(raw_data, "LSAC/General Release/Survey data/SAS/lsacgrk12.sas7bdat"))
lsac_wave_6 <- haven::read_sas(file.path(raw_data, "LSAC/General Release/Survey data/SAS/lsacgrk14.sas7bdat"))
lsac_wave_7 <- haven::read_sas(file.path(raw_data, "LSAC/General Release/Survey data/SAS/lsacgrk16.sas7bdat"))
# lsac_wave_8 <- haven::read_sas(file.path(raw_data, "LSAC/General Release/Survey data/SAS/lsacgrk18.sas7bdat"))
# lsac_wave_9.1 <- haven::read_sas(file.path(raw_data, "LSAC/General Release/Survey data/SAS/lsacgrk20.sas7bdat"))
# lsac_wave_9.2 <- haven::read_sas(file.path(raw_data, "LSAC/General Release/Survey data/SAS/lsacgrk21.sas7bdat"))


lsac_wave_1 <- lsac_wave_1_raw |> #they start to ask for asd only at wave 4 (at 11...)
  rename(ID = hicid,
         peabody_pic_vocab = cppvt,
         peabody_pic_vocab_int = cppvt2, 
         who_am_i = cwai,
         sex = zf02m1,
         age = cf03m1, 
         sdq_emot_p = caemot, 
         sdq_hyp_p = cahypr, 
         sdq_peer_p = capeer, 
         sdq_cond_p = caconda, 
         sdq_pro_p = capsoc, 
         sdq_tot_p = casdqta, 
         sdq_emot_t = ctemot, 
         sdq_cond_t = ctconda, 
         sdq_hyp_t = cthypr, 
         sdq_peer_t = ctpeer, 
         sdq_pro_t = ctpsoc, 
         sdq_tot_t = ctsdqta) |> 
  mutate(wave = 1) |> 
  select_analysis_variables()

lsac_wave_2 <- lsac_wave_2 |> 
  rename(ID = hicid, 
         peabody_pic_vocab = dppvtd,
         peabody_pic_vocab_int = dppvt2,
         matrix = dmatreas,
         age = df03m1, 
         sex = zf02m1,
         sdq_emot_p = daemot,
         sdq_cond_p = dacondb, 
         sdq_hyp_p = dahypr,
         sdq_peer_p = dapeer, 
         sdq_pro_p = dapsoc, 
         sdq_tot_p = dasdqtb, 
         sdq_emot_t = dtemot,
         sdq_cond_t = dtcondb,
         sdq_hyp_t = dthypr, 
         sdq_peer_t = dtpeer,
         sdq_pro_t = dtpsoc,
         sdq_tot_t = dtsdqtb) |> 
  mutate(wave = 2) |> 
  select_analysis_variables()


lsac_wave_3 <- lsac_wave_3 |> 
  rename(ID = hicid, 
         peabody_pic_vocab = eppvt,
         peabody_pic_vocab_int = eppvt2,
         matrix = ematreas,
         age = ef03m1, 
         sex = zf02m1,
         sdq_emot_dev_1 = ese11a,
         sdq_emot_dev_2 = ese11b,
         sdq_emot_p = eaemot,
         sdq_cond_p = eacondb, 
         sdq_hyp_p = eahypr,
         sdq_peer_p = eapeer, 
         sdq_pro_p = eapsoc, 
         sdq_tot_p = easdqtb, 
         sdq_emot_t = etemot,
         sdq_cond_t = etcondb,
         sdq_hyp_t = ethypr, 
         sdq_peer_t = etpeer,
         sdq_pro_t = etpsoc,
         sdq_tot_t = etsdqtb) |> 
  mutate(wave = 3) |> 
  select_analysis_variables()


lsac_wave_4 <- lsac_wave_4 |> 
  rename(ID = hicid, 
         autism = fhs17w,
         matrix = fmatreas,
         age = ff03m1, 
         sex = zf02m1,
         sdq_emot_p = faemot,
         sdq_cond_p = facondb, 
         sdq_hyp_p = fahypr,
         sdq_peer_p = fapeer, 
         sdq_pro_p = fapsoc, 
         sdq_tot_p = fasdqtb, 
         sdq_emot_t = ftemot,
         sdq_cond_t = ftcondb,
         sdq_hyp_t = fthypr, 
         sdq_peer_t = ftpeer,
         sdq_pro_t = ftpsoc,
         sdq_tot_t = ftsdqtb, 
         sdq_emot_c = fcemot, 
         sdq_cond_c = fccondb, 
         sdq_hyp_c = fchypr,
         sdq_peer_c = fcpeer,
         sdq_pro_c = fcpsoc, 
         sdq_tot_c = fcsdqtb, 
         sdq_emot_ple = fpemot, 
         sdq_cond_ple = fpcondb,
         sdq_hyp_ple = fphypr,
         sdq_peer_ple = fppeer,
         sdq_pro_ple = fppsoc,
         sdq_tot_ple = fpsdqtb,
         sdq_emot_p2 = fbemot, 
         sdq_cond_p2 = fbcondb,
         sdq_hyp_p2 = fbhypr,
         sdq_peer_p2 = fbpeer,
         sdq_pro_p2 = fbpsoc,
         sdq_tot_p2 = fbsdqtb,
         sdq_emot_m = fmemot, 
         sdq_cond_m = fmcondb,
         sdq_hyp_m = fmhypr,
         sdq_peer_m = fmpeer,
         sdq_pro_m = fmpsoc,
         sdq_tot_m = fmsdqtb,
         sdq_emot_f = ffemot, 
         sdq_cond_f = ffcondb,
         sdq_hyp_f = ffhypr,
         sdq_peer_f = ffpeer,
         sdq_pro_f = ffpsoc,
         sdq_tot_f = ffsdqtb) |> 
  mutate(wave = 4) |> 
  select_analysis_variables()


lsac_wave_5 <- lsac_wave_5 |> 
  rename(ID = hicid,
         autism = ghs17w,
         age = gf03m1, 
         sex = zf02m1,
         sdq_emot_p = gaemot,
         sdq_cond_p = gacondb, 
         sdq_hyp_p = gahypr,
         sdq_peer_p = gapeer, 
         sdq_pro_p = gapsoc, 
         sdq_tot_p = gasdqtb, 
         sdq_emot_t = gtemot,
         sdq_cond_t = gtcondb,
         sdq_hyp_t = gthypr, 
         sdq_peer_t = gtpeer,
         sdq_pro_t = gtpsoc,
         sdq_tot_t = gtsdqtb, 
         sdq_emot_c = gcemot, 
         sdq_cond_c = gccondb, 
         sdq_hyp_c = gchypr,
         sdq_peer_c = gcpeer,
         sdq_pro_c = gcpsoc, 
         sdq_tot_c = gcsdqtb, 
         sdq_emot_ple = gpemot, 
         sdq_cond_ple = gpcondb,
         sdq_hyp_ple = gphypr,
         sdq_peer_ple = gppeer,
         sdq_pro_ple = gppsoc,
         sdq_tot_ple = gpsdqtb,
         sdq_emot_p2 = gbemot, 
         sdq_cond_p2 = gbcondb,
         sdq_hyp_p2 = gbhypr,
         sdq_peer_p2 = gbpeer,
         sdq_pro_p2 = gbpsoc,
         sdq_tot_p2 = gbsdqtb,
         sdq_emot_m = gmemot, 
         sdq_cond_m = gmcondb,
         sdq_hyp_m = gmhypr,
         sdq_peer_m = gmpeer,
         sdq_pro_m = gmpsoc,
         sdq_tot_m = gmsdqtb,
         sdq_emot_f = gfemot, 
         sdq_cond_f = gfcondb,
         sdq_hyp_f = gfhypr,
         sdq_peer_f = gfpeer,
         sdq_pro_f = gfpsoc,
         sdq_tot_f = gfsdqtb) |> 
  mutate(wave = 5) |> 
  select_analysis_variables()


lsac_wave_6 <- lsac_wave_6 |> #academic vars badly documented so might not be correct 
  rename(ID = hicid,
         autism = hhs17w,
         visual_attention_speed = hlc16c1a,
         visual_attention_comp = hlc16c1i,
         visual_attention_integrity = hlc16c1j,
         working_memory_speed = hlc16c2a,
         working_memory_comp = hlc16c2i,
         working_memory_integrity = hlc16c2j,
         exec_func_tot_errors = hlc16c3c,
         exec_func_comp = hlc16c3i,
         exec_func_integrity = hlc16c3j,
         age = hf03m1, 
         sex = zf02m1,
         sdq_emot_p = haemot,
         sdq_cond_p = hacondb, 
         sdq_hyp_p = hahypr,
         sdq_peer_p = hapeer, 
         sdq_pro_p = hapsoc, 
         sdq_tot_p = hasdqtb, 
         sdq_emot_t = htemot,
         sdq_cond_t = htcondb,
         sdq_hyp_t = hthypr, 
         sdq_peer_t = htpeer,
         sdq_pro_t = htpsoc,
         sdq_tot_t = htsdqtb, 
         sdq_emot_c = hcemot, 
         sdq_cond_c = hccondb, 
         sdq_hyp_c = hchypr,
         sdq_peer_c = hcpeer,
         sdq_pro_c = hcpsoc, 
         sdq_tot_c = hcsdqtb, 
         sdq_emot_ple = hpemot, 
         sdq_cond_ple = hpcondb,
         sdq_hyp_ple = hphypr,
         sdq_peer_ple = hppeer,
         sdq_pro_ple = hppsoc,
         sdq_tot_ple = hpsdqtb,
         sdq_emot_p2 = hbemot, 
         sdq_cond_p2 = hbcondb,
         sdq_hyp_p2 = hbhypr,
         sdq_peer_p2 = hbpeer,
         sdq_pro_p2 = hbpsoc,
         sdq_tot_p2 = hbsdqtb,
         sdq_emot_m = hmemot, 
         sdq_cond_m = hmcondb,
         sdq_hyp_m = hmhypr,
         sdq_peer_m = hmpeer,
         sdq_pro_m = hmpsoc,
         sdq_tot_m = hmsdqtb,
         sdq_emot_f = hfemot, 
         sdq_cond_f = hfcondb,
         sdq_hyp_f = hfhypr,
         sdq_peer_f = hfpeer,
         sdq_pro_f = hfpsoc,
         sdq_tot_f = hfsdqtb) |> 
  mutate(wave = 6) |> 
  select_analysis_variables()


lsac_wave_7 <- lsac_wave_7 |> #academic performance vars are badly documented so it might not be correct
  rename(ID = hicid,
         autism = ihs17w,
         visual_attention_speed = ilc16a1a,
         visual_attention_comp = ilc16a1i,
         visual_attention_integrity = ilc16a1j,
         working_memory_speed = ilc16a2a,
         working_memory_comp = ilc16a2i,
         working_memory_integrity = ilc16a2j, 
         exec_func_tot_errors = ilc16a3c,
         exec_func_comp = ilc16a3i,
         exec_func_integrity = ilc16a3j,
         age = if03m1, 
         sex = zf02m1,
         sdq_emot_p = iaemot,
         sdq_cond_p = iacondb, 
         sdq_hyp_p = iahypr,
         sdq_peer_p = iapeer, 
         sdq_pro_p = iapsoc, 
         sdq_tot_p = iasdqtb, 
         sdq_emot_c = icemot, 
         sdq_cond_c = iccondb, 
         sdq_hyp_c = ichypr,
         sdq_peer_c = icpeer,
         sdq_pro_c = icpsoc, 
         sdq_tot_c = icsdqtb, 
         sdq_emot_ple = ipemot, 
         sdq_cond_ple = ipcondb,
         sdq_hyp_ple = iphypr,
         sdq_peer_ple = ippeer,
         sdq_pro_ple = ippsoc,
         sdq_tot_ple = ipsdqtb,
         sdq_emot_p2 = ibemot, 
         sdq_cond_p2 = ibcondb,
         sdq_hyp_p2 = ibhypr,
         sdq_peer_p2 = ibpeer,
         sdq_pro_p2 = ibpsoc,
         sdq_tot_p2 = ibsdqtb,
         sdq_emot_m = imemot, 
         sdq_cond_m = imcondb,
         sdq_hyp_m = imhypr,
         sdq_peer_m = impeer,
         sdq_pro_m = impsoc,
         sdq_tot_m = imsdqtb,
         sdq_emot_f = ifemot, 
         sdq_cond_f = ifcondb,
         sdq_hyp_f = ifhypr,
         sdq_peer_f = ifpeer,
         sdq_pro_f = ifpsoc,
         sdq_tot_f = ifsdqtb)  |> 
  mutate(wave = 7) |> 
  select_analysis_variables()




lsac_all <- bind_rows(lsac_wave_1, lsac_wave_2, lsac_wave_3, lsac_wave_4, lsac_wave_5, lsac_wave_6, lsac_wave_7)





lsac_all <- lsac_all |> 
  arrange(ID, wave) |> 
  mutate(autism = case_when(autism ==1 ~ 1,
                            is.na(autism) ~ NA,
                                  autism == 0 ~ 0,
                                  autism == -9 ~ NA),
         age = na_if(age, -9))

lsac_k_data <- lsac_all |> 
  add_autistic_any_wave() |> 
  mutate(study = "lsac_k", 
         country = "Australia")

lsac_k_data <- lsac_k_data |> 
  arrange(ID, wave) |> 
  mutate(ID = as.character(ID))

check_values(lsac_k_data)

save(lsac_k_data, file = file.path(derived_data, "lsac_k.Rdata"))

