select_analysis_variables <- function(data) {
  data |> select(any_of(c("ID", "wave", "sex", "age", "autism", 
                          "sdq_emot_p", "sdq_hyp_p", "sdq_peer_p", "sdq_cond_p", "sdq_pro_p",
                          "matrix", # iq measure in lsac,
                          "learning_disability",
                          "q_autism"
                          )
                        )
                 )
}