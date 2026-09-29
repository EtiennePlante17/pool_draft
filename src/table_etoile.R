table_etoile <- function(data, position) {

  data %>%
    filter(Position == position) %>%
    gt::gt() %>%
    gt::cols_label(
      pos = "⭐",
      Player = "Joueur",
      Status = "Pool",
      FPts = "Points"
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#FFD700"),
        gt::cell_text(weight = "bold")
      ),
      locations = gt::cells_body(rows = pos == 1)
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#C0C0C0"),
        gt::cell_text(weight = "bold")
      ),
      locations = gt::cells_body(rows = pos == 2)
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#CD7F32"),
        gt::cell_text(weight = "bold")
      ),
      locations = gt::cells_body(rows = pos == 3)
    ) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = Player)
    ) %>%
    gt::fmt_number(
      columns = FPts,
      decimals = 1
    ) %>%
    gt::tab_options(
      table.font.size = "small",
      table.width = gt::pct(100)
    )
}
