library(dplyr)

show_players <- function(data) {
  #defenseurs
  def <- (data |>
         filter(available == 1 & Position == "D" & (an1 > 20 | mean_histo > 40 | Rookie == "R") &
                  ((FPts > 50 & pts_salaire <175000) | 
                     (FPts > 20 & pts_salaire < 100000))) |>
         select(ID, Player, Team, Position, Rookie, FPts, Salary, pts_salaire, an1, an2, an3, mean_histo)
  )
  
  # Goalers
  goalies <- (data |>
         filter(available == 1 & Position == "G" & (an1 > 20 | mean_histo > 40 | Rookie == "R") &
                  ((FPts > 50 & pts_salaire <150000) | 
                     (FPts > 20 & pts_salaire < 100000))) |>
         select(ID, Player, Team, Position, Rookie, FPts, Salary, pts_salaire, an1, an2, an3, mean_histo)
  )
  
  ### Attaquants
  forwards <- (data |>
         filter(available == 1 & Position == "F" & (an1 > 20 | mean_histo > 40 | Rookie == "R") &
                  ((FPts > 50 & pts_salaire <175000) | 
                     (FPts > 20 & pts_salaire < 100000))) |>
         select(ID, Player, Team, Position, Rookie, FPts, Salary, pts_salaire, an1, an2, an3, mean_histo)
  )
  
  tot <- rbind(def, goalies, forwards) |> arrange(pts_salaire, FPts) 
  
}

score_players <- function(data, w_pts = 0.5, w_eco = 0.3, w_rare = 0.2) {

  data_mod <- show_players(data)
  
  requis_pool <- data_mod |>
    count(Position) %>% 
    left_join(pool_status(data) %>% select(Position, restant), by = "Position") %>% 
    mutate(ratio = n / restant, score_rarete = 1 - ratio) %>% 
    select(Position, score_rarete)
  
  requis_perso <- data.frame(
    Position = c("F", "D", "G"),
    requis = c(f_act + f_bench, d_act + d_bench, g_act + g_bench)) %>% 
    left_join(data %>% filter(roster == recruteur)  %>%  count(Position)
              , by = "Position") %>% 
    mutate(restant = requis - ifelse(is.na(n),0, n), 
           besoin_perso = restant/requis) %>% 
    select(Position, besoin_perso)
  
  data_mod %>% 
    left_join(requis_pool, by = "Position") %>%
    left_join(requis_perso, by = "Position") %>%
    mutate(
      score_fpts = (FPts - min(FPts, na.rm = TRUE)) /
        (max(FPts, na.rm = TRUE) - min(FPts, na.rm = TRUE)),
      
      score_cout = (max(pts_salaire, na.rm = TRUE) - pts_salaire) /
        (max(pts_salaire, na.rm = TRUE) - min(pts_salaire, na.rm = TRUE)),
      
      score_besoin = percent_rank(besoin_perso/(1-score_rarete))
    ) %>% 
    mutate(score = w_pts * score_fpts + w_eco * score_cout + w_rare * score_besoin) %>% 
    arrange(desc(score))
  
}
