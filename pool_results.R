
# Avoir les vrais résultats, prendre la somme du max par semaine
# justifier les mauvais gestionnaires de pool
# Trouver ceux qui ont 

library(dplyr)
library(purrr)

read_periods <- function(year_draft, n_periods = 28) {
  
  map_dfr(1:n_periods, function(i) {
    
    read.csv2(
      file.path( 
        "data", year_draft, 
        paste0("reel_period/Fantrax-Players-Pool Promutuel_", i, ".csv")
      ),
      sep = ","
    ) %>%
      select(ID, Player, Team, Position, Status, Roster.Status, FPts)|>
      mutate(period = i)
    
  })
}

results <- read_periods(2025, 28)

results %>%
  filter(Roster.Status == "Active") %>%
  group_by(Status) %>%
  summarise(
    FPts = sum(FPts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(-FPts)

predictions <- read.csv2(
  file.path("data", year_draft, "fantrax_proj.csv"), 
  sep = ";")

predictions <- predictions %>%
  select(ID, FPts) %>%
  rename(pred := FPts)
  
results <- results %>%
  left_join(predictions, by = "ID") %>%
  filter(Status != "FA")

results %>%
  group_by(Status) %>%
  summarise(
    FPts = sum(FPts, na.rm = TRUE),
    pred = sum(pred, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(ecart = FPts - pred) %>%
  arrange(-FPts)
