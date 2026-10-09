graph_hii <- function(data_reel, group, periode = c("cumulatif", "semaine") ) {
  
  periode <- match.arg(periode)
  
  # Filtrer sur la semaine en cours si demandé
  if (periode == "semaine") {
    
    data_reel <- data_reel %>%
      filter(Period == week_obs)
  }
  
  hhi_status <- data_reel |>
    filter(Roster.Status == "Active") |>
    summarise(points_joueur = sum(FPts, na.rm = TRUE), .by = c(Status, Player)) |>
    summarise(
      points_total = sum(points_joueur),
      HHI = sum((points_joueur / points_total)^2),
      .by = all_of(group)
    ) |>
    arrange(desc(HHI))
  
  ggplot(hhi_status, aes(x = reorder(Status, HHI), y = HHI)) +
    geom_col(fill = "#2878b5", width = 0.7) +
    geom_text(
      aes(label = number(HHI, accuracy = 0.001)),
      hjust = -0.15,
      size = 3.5
    ) +
    coord_flip() +
    scale_y_continuous(
      labels = label_number(accuracy = 0.01),
      expand = expansion(mult = c(0, 0.15))
    ) +
    labs(
      title = "Concentration des points",
      x = NULL,
      y = "Herfindahl Index (HHI)",
    ) +
    theme_minimal(base_size = 12)
}
