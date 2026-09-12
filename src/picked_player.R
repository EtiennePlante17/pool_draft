library(dplyr)

picked_player <- function(data, id_player) {
  #Ajouter un warning
  if (!(id_player %in% data$ID)) {
    stop(paste0("Error: '", id_player, "' is not a valid value in the Participant column."))
  }

  pick_number <- data |> filter(available == 0) |> nrow() + 1
  
  data |>
    mutate(available = if_else(ID == id_player, 0, available),
           roster = if_else(ID == id_player, draft_order[pick_number], roster))
  
}
