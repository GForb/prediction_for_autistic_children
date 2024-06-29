
dl_raw_scores <- data |> select(VABS9age, starts_with("VABS9d")) |> 
  mutate(VABS9d_raw = VABS9dprw + VABS9ddrw+ VABS9dcrw) |> 
  filter(VABS9age > 161, VABS9age < 168)


plot(dl_raw_scores$VABS9d_raw, dl_raw_scores$VABS9dss)