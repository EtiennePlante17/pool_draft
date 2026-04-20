library(dplyr)

find_player <- function(data, team, position, first_letter) {
  #Ajouter des conditions pour pouvoir laisser des arguments vides
  
  data |>
    filter(Team == team, Position == position, 
           substr(Player, 1, 1) == first_letter) |>
    select(Player, available)
}
