library(dplyr)

score_players <- function(data, w_pts = 0.25, w_cout = 0.25, w_renta = 0.25, 
                          w_rare = 0.25, participant = recruteur) {
  
  salaire_min <- data %>%
    filter(available == 1 & FPts > 0) %>%
    group_by(Position) %>%
    arrange(salaire, .by_group = TRUE) %>%
    mutate(
      n = row_number(),
      salaire_cumulatif = cumsum(salaire)
    ) %>%
    ungroup %>%
    select(Position, n, salaire_cumulatif)
  
  # Trouver le nombre de joueurs par position encore requis
  requis_perso <- data.frame(
    Position = c("F", "D", "G"),
    requis = c(f_act + f_bench, d_act + d_bench, g_act + g_bench)) %>% 
    left_join(data %>% filter(Status == participant)  %>%  count(Position)
              , by = "Position") %>% 
    mutate(restant = requis - ifelse(is.na(n),0, n), 
           besoin_perso = restant/requis,
           restant_1 = pmax(0, restant - 1)) %>%
    left_join(salaire_min, by = c("Position", "restant" = "n")) %>%
    left_join(salaire_min, by = c("Position", "restant_1" = "n"))
  
  # Assigner un budget min a chacune des positions
  budget_min <- tibble(Position = character(), budget_min = numeric())
  
  for (pos in c("F", "D", "G")) {
    
    budget <- 
      requis_perso %>% filter(Position == pos) %>% 
      summarise(budget = sum(coalesce(salaire_cumulatif.y, 0))) %>% 
      pull(budget) +
      (requis_perso %>% filter(Position != pos) %>% summarise(
            budget = sum(coalesce(salaire_cumulatif.x, 0))) %>% pull(budget))
    
    budget_min <- bind_rows(budget_min, 
                            tibble(Position = pos, budget_min = budget))
    }
  
  requis_perso <- requis_perso %>%
    left_join(budget_min, by = "Position")
    
  # Trouver le salaire restant du participant
  salaire_reste <- tryCatch(
    {
      roster_status(data, participant)$salaire_restant
    },
    error = function(e) {
      masse_salariale
    }
  )
  
  #vider le cash
  if ((requis_perso %>% summarise(restant = sum(restant)))$restant == 1) {
    w_pts <- 1
    w_cout <- 0
    w_renta <- 0
    w_rare <- 0
  }

  # Enlever les positions qui ne sont plus nécessaires à drafter
  # Enlever les joueurs ou il sera impossible de rester dans la masse salariale
  data_mod <- data %>% filter(available == 1 & FPts > 0) %>%    
    inner_join(requis_perso  %>% filter(restant > 0) %>% 
                 select(Position, besoin_perso, budget_min), 
               by = "Position") %>% 
    filter(salaire_reste - salaire >= budget_min)
  
  if (nrow(data_mod) == 0) {
    warning(
      paste0(
        "Aucun joueur admissible pour ", participant,
        " avec ", format(salaire_reste, big.mark = ","),
        "$ restant."
      )
    )
    return(data.frame())
  }
  
  score_prelim <- data_mod %>%
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
  
  requis_pool <- data_mod %>% 
    count(Position) %>% 
    left_join(pool_status(data) %>% select(Position, restant), by = "Position") %>% 
    mutate(ratio = n / restant)
  
  # je veux plutot voir la différence dans les x prochains pour chaque position
  # Prendre en compte qqn qui a deja pris tous ces requis d'une position 
  
  #  déterminer dans combien de pick ca revient a la personne
  pick_current <- nrow(proj %>% filter(available == 0)) + 1
  
  # si dernier pick, pas regardé plus loin
  if ((requis_perso %>% summarise(restant = sum(restant)))$restant == 1) {
    next_pick <- 1
  } else {
    
    positions_participant <- which(
      draft_order == participant &
        seq_along(draft_order) > pick_current
    )
    
    next_pick <- ifelse(
    length(positions_participant) > 0,
    positions_participant[1] - pick_current,
    1
  )
  }
 
  # Vecteur du nombre de picks par status
  nb_picks_status <- tibble(
    Status = draft_order[(pick_current+1):(pick_current+next_pick-1)]) %>%
    count(Status, name = "nb_picks") 
 
  # Nombre de picks maximal pour chaque position avant le prochain tour
  besoin_status <- expand.grid( Status = unique(nb_picks_status$Status),
                                Position = c("F", "D", "G")) %>%
    as_tibble() %>%
    left_join(data %>%
                filter(Status != participant, available == 0) %>%
                count(Status, Position, name = "deja_pris"),
              by = c("Status", "Position")) %>%
    mutate(deja_pris = ifelse(is.na(deja_pris), 0, deja_pris),
           requis = case_when(
             Position == "F" ~ f_act + f_bench,
             Position == "D" ~ d_act + d_bench,
             Position == "G" ~ g_act + g_bench),
           restant = pmax(requis - deja_pris, 0)) %>%
    left_join(nb_picks_status, by = "Status") %>%
    mutate(nb_picks_position = pmin(nb_picks, restant)) %>%
    summarise(n_pick = sum(nb_picks_position), .by = "Position")
    
  #amélioration: pourrait avoir une vision un peu plus long terme que juste next round
  # Considère la perte de score prélim pour le nombre par position possible
  # Calcul du coût d'opportunité
  cout_opportunite <- score_prelim %>%
    left_join(besoin_status, by = "Position") %>%
    group_by(Position) %>%
    arrange(desc(score_prelim), .by_group = TRUE) %>%
    summarise(
      n_pick = first(n_pick),
      meilleur_score = first(score_prelim),
      score_apres_picks = ifelse(
        n_pick > 0 & n_pick < n(),
        score_prelim[n_pick + 1],
        first(score_prelim)
      ),
      nb_disponibles = n(),
      .groups = "drop"
    )
  
  besoin_normalise <- requis_perso %>%
    select(Position, restant) %>%
    mutate(
      besoin_normalise = if (max(restant) == min(restant)) {
        1
      } else {
        ifelse(
          (Position == "F" & restant <= f_bench) |
            (Position == "D" & restant <= d_bench) |
            (Position == "G" & restant <= g_bench),
          0,
          0.25 + 0.75 * 
            (restant - min(restant)) / (max(restant) - min(restant))
        )
      }
    )
  
  score_prelim %>%
    left_join(cout_opportunite, by = "Position") %>%
    left_join(besoin_normalise, by = "Position") %>%
    mutate(
      score_urgence = (score_prelim - score_apres_picks) /
        (max(meilleur_score)- min(score_apres_picks)),
      
      score = score_prelim * (1 + w_rare * score_urgence * besoin_normalise)
    ) %>%
    arrange(desc(score))
  
}
