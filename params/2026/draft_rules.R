f_act <- 9
f_bench <- 1
d_act <- 4
d_bench <- 1
g_act <- 1
g_bench <- 1
masse_salariale <- 95000000

draft <- c("WP","XR","ZC","CAR","LG","SF","CC","AR","PM","FG","RT","EP","MB","AP","JPL","VMS")

draft_order <- rep(
  list(draft, rev(draft)),
  length.out = 17
) |>
  unlist()

n_part <- length(draft)

f_tot <- f_act + f_bench
d_tot <- d_act + d_bench
g_tot <- g_act + g_bench
n_tot <- f_tot + d_tot + g_tot