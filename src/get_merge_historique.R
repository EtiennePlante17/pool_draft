library(dplyr)

merge_historique <- function(data, history_year = 3) {
  
  # Base transformations
  data <- data |>
    mutate(
      pts_salaire = round(salaire / FPts, 0),
      available = 1,
      roster = ""
    )
  
  # Loop over history years
  for (i in seq_len(history_year)) {
    
    # Read file dynamically
    histo <- read.csv2(
      file.path("data", year_draft, paste0("fantrax_exp_", year_draft - i, ".csv")),
      sep = ","
    )
    
    # Conserver colonne et renommer
    histo <- histo |>
      select(ID, FPts) |>
      rename(!!paste0("an", i) := FPts)
    
    # Merge
    data <- data |>
      left_join(histo, by = "ID")
  }
  
  data <- data %>%
    mutate(mean_histo = round(rowMeans(across(starts_with("an")), 
                                       na.rm = TRUE),0))
  
  return(data)
}
