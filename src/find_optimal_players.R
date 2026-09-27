find_optimal <- function(data, group, optimal_name = "optimal") {
  data %>%
    group_by(across(all_of(group))) %>%
    arrange(desc(FPts), .by_group = TRUE) %>%
    mutate(
      nb_act = case_when(
        Position == "F" ~ f_act,
        Position == "D" ~ d_act,
        Position == "G" ~ g_act
      ),
      "{optimal_name}" := if_else(row_number() > nb_act, 0, 1)
    ) %>%
    ungroup()
}
