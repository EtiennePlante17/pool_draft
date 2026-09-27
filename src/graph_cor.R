graph_corr <- function(data, metrique1, metrique2) {
  
  x <- rlang::as_name(rlang::ensym(metrique1))
  y <- rlang::as_name(rlang::ensym(metrique2))
  
  p <- ggplot(data,  aes(x = {{ metrique1 }}, y = {{ metrique2 }}, color = Status)) +
    geom_point() +
    geom_smooth(
      aes(group = 1),
      method = "lm",
      formula = y ~ x,
      se = TRUE,
      color = "black"
    ) +
    theme_bw()
  
  ggplotly(p)
}
