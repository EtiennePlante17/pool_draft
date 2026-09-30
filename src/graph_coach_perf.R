graph_coach_perf <- function(data) {
  
  coach_perf <- data
  
  ordre_status <- coach_perf |>
    arrange(points_reels) |>   
    pull(Status)
  
  coach_perf_long <- coach_perf |>
    select(Status, points_reels, ecart_optimal) |>
    pivot_longer(
      cols = c(points_reels, ecart_optimal),
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
      aes(x = Status, y = points_optim, label = label),
      inherit.aes = FALSE,
      hjust = -0.1,
      size = 4,
      fontface = "bold"
    ) +
    
    scale_fill_manual(values = c(
      "points_reels" = "steelblue",
      "ecart_optimal" = "firebrick"
    )) +
    
    labs(
      title = "Performance vs optimal (efficacité)",
      x = "Participant",
      y = "Points",
      fill = ""
    ) +
    
    expand_limits(y = max(coach_perf$points_optim) * 1.1)
}
