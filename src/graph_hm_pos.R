graph_hm_pos <- function(data_proj, data_reel, group, periode) {

  df_ecart_pos <- comp_reelvsproj(data_proj, data_reel, group, periode)
  
  ggplot(df_ecart_pos, aes(x = Position, y = Status, fill = ecart)) +
    geom_tile() +
    scale_y_discrete(limits = rev) +
    scale_fill_gradient2(low = "red", mid = "lightyellow", high = "green", midpoint = 0) +
    labs(
      title = "Heatmap performance vs projections",
      fill = "Écart"
    ) +
    geom_text(aes(label = round(ecart, 2)), size = 3)
}
