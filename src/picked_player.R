library(dplyr)

picked_player <- function(data, player, participant) {
  #Ajouter un warning
  if (!(player %in% data$Player)) {
    stop(paste0("Error: '", player, "' is not a valid value in the Participant column."))
  }
  
  data |>
    mutate(available = if_else(Player == player, 0, available),
           roster = if_else(Player == player, participant, roster))
}
