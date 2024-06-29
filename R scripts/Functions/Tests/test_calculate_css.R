# RRB
calculate_css_rrb(rrb_raw_score = 2, module = 1,words = 0) == 4
calculate_css_rrb(rrb_raw_score = 2, module = 1,words = 1) == 6
calculate_css_rrb(rrb_raw_score = 2, module = 2,words = 0) == 6
calculate_css_rrb(rrb_raw_score = 2, module = 3,words = 0) == 7


calculate_css_rrb(rrb_raw_score = 2:4, module = rep(1, 3),words = 0) 
calculate_css_rrb(rrb_raw_score = 2:4, module = rep(1, 3),words = 1)
calculate_css_rrb(rrb_raw_score = 2:4, module = rep(2, 3),words = 0)
calculate_css_rrb(rrb_raw_score = 2:4, module = rep(3, 3),words = 0)


calculate_css_rrb(rrb_raw_score = 6, module = 1,words = 0) == 8
calculate_css_rrb(rrb_raw_score = 6, module = 1,words = 1) == 9
calculate_css_rrb(rrb_raw_score = 6, module = 2,words = 0) == 9
calculate_css_rrb(rrb_raw_score = 6, module = 3,words = 0) == 10


calculate_css_rrb(rrb_raw_score = 8, module = 1,words = 0) == 10
calculate_css_rrb(rrb_raw_score = 8, module = 1,words = 1) == 10
calculate_css_rrb(rrb_raw_score = 8, module = 2,words = 0) == 10
calculate_css_rrb(rrb_raw_score = 8, module = 3,words = 0) == 10

# SA
calculate_css_sa(sa_raw_score = 2, module = 1,words = 0) == 1
calculate_css_sa(sa_raw_score = 2, module = 1,words = 1) == 2
calculate_css_sa(sa_raw_score = 2, module = 2,words = 0) == 2
calculate_css_sa(sa_raw_score = 2, module = 3,words = 0) == 2

calculate_css_sa(sa_raw_score = 10, module = 1,words = 0) == 4
calculate_css_sa(sa_raw_score = 10, module = 1,words = 1) == 6
calculate_css_sa(sa_raw_score = 10, module = 2,words = 0) == 6
calculate_css_sa(sa_raw_score = 10, module = 3,words = 0) == 8

calculate_css_sa(sa_raw_score = 18, module = 1,words = 0) == 8
calculate_css_sa(sa_raw_score = 18, module = 1,words = 1) == 9
calculate_css_sa(sa_raw_score = 18, module = 2,words = 0) == 10
calculate_css_sa(sa_raw_score = 18, module = 3,words = 0) == 10
