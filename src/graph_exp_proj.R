graph_exp_proj <- function(data) {

  graph_data <- data %>% 
    mutate( 
      points_proj = ifelse(Period <= week_obs, 0, points_proj),
      points_reels = ifelse(Period <= week_obs & !is.na(points_reels), points_reels, 0),
      points_tot = points_proj + points_reels
    ) %>%
    summarise(points_proj = sum(points_proj), points_reels = sum(points_reels),
              points_tot = sum(points_tot), .by = "Status") %>%
    arrange(-points_tot)
  
  moyenne <- mean(graph_data$points_tot)
  
  graph_data %>%
    mutate(Status = factor(Status, levels = Status)) %>%
    pivot_longer(
      cols = c(points_reels, points_proj),
      names_to = "type",
      values_to = "points"
    ) %>%
    ggplot(aes(x = Status, y = points, fill = type)) +
    geom_col() +
    geom_text(
      aes(label = ifelse(points > 0, round(points, 0), "")),
      position = position_stack(vjust = 0.5),
      size = 3
    ) +
    geom_text(
      data = graph_data,
      aes(
        x = Status,
        y = points_tot,
        label = round(points_tot, 0)
      ),
      vjust = -0.5,
      inherit.aes = FALSE,
      fontface = "bold"
    ) +
    geom_hline(
      yintercept = moyenne,
      linetype = "dashed"
    ) +
    scale_fill_manual(
      values = c(
        points_reels = "steelblue",
        points_proj = "lightblue"
      ),
      labels = c(
        points_reels = "Points réels",
        points_proj = "Points projetés"
      )
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.08))
    ) +
    labs(
      x = NULL,
      y = "Points",
      fill = NULL
    ) +
    theme_bw() +
    theme(
      legend.position = "top"
    )
}
