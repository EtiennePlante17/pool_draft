library(dplyr)

roster_status <- function(data, participant) {
  data |>
    filter(roster == participant) |>
      mutate(salaire_restant = masse_salariale - sum(salaire),
             n_F = sum(Position == "F", na.rm = TRUE),
             n_D = sum(Position == "D", na.rm = TRUE),
             n_G = sum(Position == "G", na.rm = TRUE),
             sal_moy_res = salaire_restant / (n_tot - sum(roster == participant, na.rm = TRUE))) |>
    select(salaire_restant, n_F, n_D, n_G, sal_moy_res)
}
