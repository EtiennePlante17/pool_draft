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
