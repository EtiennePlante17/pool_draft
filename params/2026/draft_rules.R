f_act <- 9
f_bench <- 1
d_act <- 4
d_bench <- 1
g_act <- 1
g_bench <- 1
masse_salariale <- 95000000

draft <- c("EP","XR","PM","VMSO","SF","CC","LM","ZC","MB","LG","RT","JPL","AR","FG","WP","AP")

draft_order <- rep(
  list(draft, rev(draft)),
  length.out = (f_act + f_bench + d_act + d_bench + g_act + g_bench)
  ) |>
  unlist()

n_part <- length(draft)

f_tot <- f_act + f_bench
d_tot <- d_act + d_bench
g_tot <- g_act + g_bench
n_tot <- f_tot + d_tot + g_tot