graph_pts_salaire <- function(data) {
  fpts_salaire <- data %>%
    group_by(Status, Position, Period) %>%
    summarise(
      FPts = sum(FPts, na.rm = TRUE),
      masse_salariale = sum(salaire, na.rm = TRUE),
      nb_joueurs = n(),
      .groups = "drop"
    ) %>%
    group_by(Status, Position) |> 
    summarise(
      FPts = sum(FPts, na.rm = TRUE),
      masse_salariale = mean(masse_salariale, na.rm = TRUE),
      nb_joueurs = n(),
      .groups = "drop"
    )
  
  p <- ggplot(
    fpts_salaire,
    aes(
      x = masse_salariale,
      y = FPts,
      colour = Position,
      text = paste0(
        "Status : ", Status,
        "<br>Position : ", Position,
        "<br>FPts : ", comma(FPts),
        "<br>Masse salariale : $", comma(masse_salariale),
        "<br>Nombre de joueurs : ", nb_joueurs,
        "<br>FPts / $M : ",
        round(FPts / masse_salariale * 1000000, 1)
      )
    )
  ) +
    geom_point(
      size = 3,
      alpha = 0.8
    ) +
    scale_x_continuous(
      labels = dollar_format(scale = 1e-6, suffix = " M$"),
      expand = expansion(mult = 0.05)
    ) +
    scale_y_continuous(
      labels = comma,
      expand = expansion(mult = 0.05)
    ) +
    labs(
      x = "Masse salariale",
      y = "Points cumulatifs",
      colour = "Position"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )
  
  ggplotly(
    p,
    tooltip = "text"
  )
}
