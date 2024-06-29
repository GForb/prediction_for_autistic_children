data_folder <- here::here(raw_data, "TEDS")

# Import ADOS data

ados_data_raw  <- haven::read_sav(here::here(data_folder, "ADOS - clean version to use for analyses - 10.06.10.sav"))
                    
ados_data_m1_nw   <- ados_data_raw |> 
  select(Family_ID,
         Twin_ID, 
         ados_module = ADOS_Module,
         ados_sa_rrb_raw = M1NO_WORDS_SOCAFFECTANDRRBTOTAL,
         ados_sa_raw = M1NO_WORDS_SOCIALAFFECTTOTAL,
         ados_rrb_raw = M1NO_WORDS_RRBTOTAL
        ) |> 
  mutate(module = 1,
         words = 0) |> 
  filter(!is.na(ados_sa_rrb_raw) | !is.na(ados_sa_raw) | !is.na(ados_rrb_raw))

ados_data_m1_sw   <- ados_data_raw |> 
  select(Family_ID,
         Twin_ID, 
         ados_module = ADOS_Module,
         ados_sa_rrb_raw = M1_SOME_WORDS_SOCAFFECTANDRRBTOTAL,
         ados_sa_raw = M1SOME_WORDS_SOCIALAFFECTTOTAL,
         ados_rrb_raw = M1SOME_WORDS_RRBTOTAL,
  ) |> 
  mutate(module = 1,
         words = 1) |> 
  filter(!is.na(ados_sa_rrb_raw) | !is.na(ados_sa_raw) | !is.na(ados_rrb_raw))

ados_data_m2 <- ados_data_raw |> 
  select(Family_ID,
         Twin_ID, 
         ados_module = ADOS_Module,
         ados_sa_rrb_raw = M2SOCIALAFFECTANDRRBTOTAL,
         ados_sa_raw = M2SOCIALAFFECTTOTAL,
         ados_rrb_raw = M2RRBTOTAL
  ) |> 
  mutate(module = 2,
         words = 1)  |> 
  filter(!is.na(ados_sa_rrb_raw) | !is.na(ados_sa_raw) | !is.na(ados_rrb_raw))                                 

ados_data_m3 <- ados_data_raw |> 
  select(Family_ID,
         Twin_ID, 
         ados_module = ADOS_Module,
         ados_sa_rrb_raw = M3SOCIALRESTOTAL_NEW,
         ados_sa_raw = M3SOCIALAFFECTTOTAL_NEW,
         ados_rrb_raw = M3RESREPBEHAVIOUR_NEW
  ) |> 
  mutate(module = 3,
         words = 1) |> 
  filter(!is.na(ados_sa_rrb_raw) | !is.na(ados_sa_raw) | !is.na(ados_rrb_raw))

ados_data <- bind_rows(ados_data_m1_nw, ados_data_m1_sw, ados_data_m2, ados_data_m3)

n_ados = sum(!is.na(ados_data_raw$ADOS_Module))
check <- nrow(ados_data) - n_ados

if(check != 0){
  stop("Some rows have been lost")
}                       


ados_data