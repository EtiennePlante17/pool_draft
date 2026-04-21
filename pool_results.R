## Faire un quarto pour les résultats du pool versus le prédit
# mettre la citation de Mike en entré
# Faire une section meilleur DG pour le repechage
# Faire une section coach pour la gestion du banc


library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)

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
      mutate(Period = i)
    
  })
}

results <- read_periods(2025, 28)
projections <- read.csv2(
  file.path("data", 2025, "fantrax_projections.csv"), 
  sep = ",")

### Perte points par gestion des actifs
points_opt <- results |>
  group_by(Status, Period, Position) |>
  mutate(
    rank_pos = rank(FPts, ties.method = "first"),
    n_pos = n()
  ) |>
  filter(rank_pos > 1) |> 
  group_by(Status, Period) |>
  summarise(points_optimaux = sum(FPts, na.rm = TRUE), .groups = "drop")

points_reels <- results |>
  group_by(Status, Period) |>
  summarise(
    points_reels = sum(FPts[Roster.Status == "Active"], na.rm = TRUE),
    .groups = "drop"
  )

coach_perf <- points_reels |>
  left_join(points_opt, by = c("Status", "Period")) |>
  group_by(Status) |>
  summarise(
    total_reel = sum(points_reels),
    total_optimal = sum(points_optimaux),
    efficiency = total_reel / total_optimal,
    ecart_optimal = total_optimal - total_reel
  ) |>
  arrange((total_reel), desc(efficiency))

ordre_status <- coach_perf |>
  arrange(total_reel) |>   # 👈 ordre croissant
  pull(Status)

coach_perf_long <- coach_perf |>
  select(Status, total_reel, ecart_optimal) |>
  pivot_longer(
    cols = c(total_reel, ecart_optimal),
    names_to = "type",
    values_to = "points"
  ) |>
  mutate(Status = factor(Status, levels = ordre_status))

labels_eff <- coach_perf |>
  mutate(
    label = paste0(round(efficiency * 100, 1), "%")
  )

ggplot(coach_perf_long, aes(x = Status, y = points, fill = type)) +
  geom_col() +
  coord_flip() +
  
  geom_text(
    data = labels_eff,
    aes(x = Status, y = total_optimal, label = label),
    inherit.aes = FALSE,   # 👈 IMPORTANT
    hjust = -0.1,
    size = 4,
    fontface = "bold"
  ) +
  
  scale_fill_manual(values = c(
    "total_reel" = "steelblue",
    "ecart_optimal" = "firebrick"
  )) +
  
  labs(
    title = "Performance vs optimal (efficacité)",
    x = "Participant",
    y = "Points",
    fill = ""
  ) +
  
  expand_limits(y = max(coach_perf$total_optimal) * 1.1)

### Points dans le temps
points_temps <- results |>
  filter(Roster.Status == "Active") |>
  group_by(Status, Period) |>
  summarise(points = sum(FPts, na.rm = TRUE), .groups = "drop")

ggplot(points_temps, aes(x = Period, y = points, color = Status)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Points par participant par période",
    x = "Période",
    y = "Points"
  ) +
  theme_minimal()

points_temps %>% 
  filter(points == max(points_temps$points))

points_temps |>
  filter(!Period %in% c(19, 20,28)) |>
  slice_min(points, n = 1)

### Points cumulés
points_cum <- points_temps |>
  arrange(Status, Period) |>
  group_by(Status) |>
  mutate(points_cum = cumsum(points)) |>
  ungroup()

ggplot(points_cum, aes(x = Period, y = points_cum, color = Status)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Points cumulés par participant",
    x = "Période",
    y = "Points cumulés"
  ) +
  theme_minimal()

# 1. Identifier le gagnant (dernier cumul)
winner <- points_cum |>
  group_by(Status) |>
  summarise(final_points = max(points_cum)) |>
  arrange(desc(final_points)) |>
  slice(1) |>
  pull(Status)

# 2. Calculer le rang à chaque période
points_rank <- points_cum |>
  group_by(Period) |>
  mutate(rank = rank(-points_cum, ties.method = "min")) |>
  ungroup()

# 3. % du temps où le gagnant est 1er
pct_first <- points_rank |>
  filter(Status == winner) |>
  summarise(
    pct_1er = mean(rank == 1) * 100
  )

paste0(winner, " a assert sa dominance dans ", pct_first,"% des périodes!")

### Comparaison du prédit et du réel optimal
projections |>
  group_by(Status, Position) |>
  mutate(
    rank_pos = rank(FPts, ties.method = "first"),
    n_pos = n()
  ) |>
  filter(rank_pos > 1) |>
  group_by(Status, Position) |>
  summarise(points_projetes = sum(FPts, na.rm = TRUE), .groups = "drop") |>
  left_join(
    results |>
  group_by(Status, Position) |>
  mutate(
    rank_pos = rank(FPts, ties.method = "first"),
    n_pos = n()
  ) |>
  filter(rank_pos > 1) |> 
  group_by(Status, Position) |>
  summarise(points_optimaux = sum(FPts, na.rm = TRUE), .groups = "drop"),
  by = c("Status", "Position")
  )

df_plot <- projections |>
  filter(Status != "FA") |>
  group_by(Status, Position) |>
  mutate(
    rank_pos = rank(FPts, ties.method = "first")
  ) |>
  filter(rank_pos > 1) |>
  summarise(points_projetes = sum(FPts), .groups = "drop") |>
  left_join(
    results |>
      group_by(Status, Period, Position) |>
      mutate(
        rank_pos = rank(FPts, ties.method = "first"),
        n_pos = n()
      ) |>
      filter(rank_pos > 1) |> 
      group_by(Status, Position) |>
      summarise(points_optimaux = sum(FPts, na.rm = TRUE), .groups = "drop"),
    by = c("Status", "Position")
  ) |>
  mutate(ecart = points_optimaux - points_projetes,
         ratio = points_optimaux / points_projetes)



ggplot(df_plot, aes(x = Position, y = Status, fill = ratio)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "yellow", high = "blue", midpoint = 1) +
  labs(
    title = "Heatmap performance vs projections",
    fill = "Écart"
  )

