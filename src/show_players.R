library(dplyr)

show_players <- function(data) {
  #defenseurs
  def <- (data |>
         filter(available == 1 & Position == "D" & (an1 > 20 | mean_histo > 40 | Rookie == "R") &
                  ((FPts > 50 & pts_salaire <175000) | 
                     (FPts > 20 & pts_salaire < 100000))))
  
  # Goalers
  goalies <- (data |>
         filter(available == 1 & Position == "G" & (an1 > 20 | mean_histo > 40 | Rookie == "R") &
                  ((FPts > 50 & pts_salaire <150000) | 
                     (FPts > 20 & pts_salaire < 100000))))
  
  ### Attaquants
  forwards <- (data |>
         filter(available == 1 & Position == "F" & (an1 > 20 | mean_histo > 40 | Rookie == "R") &
                  ((FPts > 50 & pts_salaire <175000) | 
                     (FPts > 20 & pts_salaire < 100000))))
  
  tot <- rbind(def, goalies, forwards) |> arrange(pts_salaire, FPts) 
  tot
}

score_players <- function(data, w_pts = 0.25, w_cout = 0.25, w_renta = 0.25, 
                          w_rare = 0.25, 
                          participant = recruteur) {

  # Trouver le nombre de joueurs par position encore requis
  requis_perso <- data.frame(
    Position = c("F", "D", "G"),
    requis = c(f_act + f_bench, d_act + d_bench, g_act + g_bench)) %>% 
    left_join(data %>% filter(Status == participant)  %>%  count(Position)
              , by = "Position") %>% 
    mutate(restant = requis - ifelse(is.na(n),0, n), 
           besoin_perso = restant/requis)
  
salaire_reste <- tryCatch(
  {
    roster_status(data, participant)$salaire_restant
  },
  error = function(e) {
    masse_salariale
  }
)
  
  # Enlever les positions qui ne sont plus nécessaires à drafter
  # Enlever les joueurs ou il sera impossible de rester dans la masse salariale

# au lieu d'utiliser show_players, il faut borner les filtres pour avoir assez de chaque position
# si les criteres changent par position, je crois que ca biaise le score de besoin
  data_mod <- data %>% filter(available == 1 & FPts > 0
                              #& (
    #(FPts > 30 & pts_salaire <250000) | (FPts > 15 & pts_salaire < 175000))
  ) %>%    
    inner_join(requis_perso  %>% filter(restant > 0) %>% select(Position), 
               by = "Position") %>% 
    filter(salaire_reste > salaire + 1000000*sum(requis_perso$restant))
  
  requis_pool <- data_mod |>
    count(Position) %>% 
    left_join(pool_status(data) %>% select(Position, restant), by = "Position") %>% 
    mutate(ratio = n / restant, score_rarete = 1 - ratio) %>% 
    select(Position, score_rarete)
  
  score_prelim <- data_mod %>%
    left_join(requis_pool, by = "Position") %>%
    left_join(requis_perso %>% select(Position, besoin_perso), by = "Position") %>%
    mutate(
      score_fpts = (FPts - min(FPts, na.rm = TRUE)) /
        (max(FPts, na.rm = TRUE) - min(FPts, na.rm = TRUE)),
      
      score_cout = (max(salaire, na.rm = TRUE) - salaire) /
        (max(salaire, na.rm = TRUE) - min(salaire, na.rm = TRUE)),
      
      score_renta = (max(pts_salaire, na.rm = TRUE) - pts_salaire) /
        (max(pts_salaire, na.rm = TRUE) - min(pts_salaire, na.rm = TRUE))
    ) %>%
    mutate(score_prelim = w_pts * score_fpts + w_cout * score_cout + 
             w_renta * score_renta) %>%
    arrange(desc(score_prelim))
  
  data_mod %>%
    left_join(requis_pool, by = "Position") %>%
    left_join(requis_perso %>% select(Position, besoin_perso), by = "Position") %>%
    mutate(
      score_fpts = (FPts - min(FPts, na.rm = TRUE)) /
        (max(FPts, na.rm = TRUE) - min(FPts, na.rm = TRUE)),

      score_cout = (max(salaire, na.rm = TRUE) - salaire) /
        (max(salaire, na.rm = TRUE) - min(salaire, na.rm = TRUE)),
      
      score_renta = (max(pts_salaire, na.rm = TRUE) - pts_salaire) /
        (max(pts_salaire, na.rm = TRUE) - min(pts_salaire, na.rm = TRUE)),

      score_besoin = percent_rank(besoin_perso/(1-score_rarete))
    ) %>%
    mutate(score = w_pts * score_fpts + w_cout * score_cout + 
             w_renta * score_renta + w_rare * score_besoin) %>%
    arrange(desc(score))
  
}
