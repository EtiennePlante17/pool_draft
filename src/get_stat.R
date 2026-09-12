library(dplyr)

roster_status <- function(data, participant) {
  data |>
    filter(roster == participant) |>
    summarise(salaire_restant = masse_salariale - sum(salaire),
             n_F = sum(Position == "F", na.rm = TRUE),
             n_D = sum(Position == "D", na.rm = TRUE),
             n_G = sum(Position == "G", na.rm = TRUE),
             sal_moy_res = salaire_restant / (n_tot - sum(roster == participant, na.rm = TRUE)),
             by = participant) |>
    select(salaire_restant, n_F, n_D, n_G, sal_moy_res)
}

pool_status <- function(data) {
  disponible <- data |>
    filter(available == 0) |>
    count(Position)
  
  requis <- data.frame(
    Position = c("F", "D", "G"),
    requis = n_part * c(
      f_act + f_bench,
      d_act + d_bench,
      g_act + g_bench
    )
  )
  
  requis |>
    left_join(disponible, by = "Position") |>
    mutate(restant = requis - ifelse(is.na(n),0, n))
}
