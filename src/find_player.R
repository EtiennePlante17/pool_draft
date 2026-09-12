library(dplyr)
library(stringr)

find_player <- function(data, name, row_tokeep) {
  #Ajouter des conditions pour pouvoir laisser des arguments vides
  
  result <- data |>
    filter(available == 1 & str_detect(Player, regex(name, ignore_case = TRUE))) |>
    select(Player, available, Position, ID)
  
  # Stocker le ID de la ligne choisie
  player_id <- result$ID[row_tokeep]
  
  # Afficher le résultat
  print(result)
  
  # Retourner le ID
  return(player_id)
}

# find_player <- function(data, team, position, first_letter) {
#   #Ajouter des conditions pour pouvoir laisser des arguments vides
#   
#   data |>
#     filter(Team == team, Position == position, 
#            substr(Player, 1, 1) == first_letter) |>
#     select(Player, available, ID)
# }
