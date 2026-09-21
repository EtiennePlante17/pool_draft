library(dplyr)

picked_player <- function(data, id_player) {
  #Ajouter un warning
  if (!(id_player %in% data$ID)) {
    stop(paste0("Error: '", id_player, "' is not a valid value in the Participant column."))
  }

  pick_number <- data |> filter(available == 0) |> nrow() + 1
  
  data |>
    mutate(available = if_else(ID == id_player, 0, available),
           Status = if_else(ID == id_player, draft_order[pick_number], Status),
           Round = if_else(ID == id_player, ceiling(pick_number / 16), Round),
           Pick = if_else(ID == id_player, ((pick_number - 1) %% 16) + 1, Pick),
           Ov.Pick = if_else(ID == id_player, pick_number, Ov.Pick))
  
}
