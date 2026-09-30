comp_reelvsproj <- function(data_proj, data_reel, group,
                            periode = c("cumulatif", "semaine") ) {
  
  periode <- match.arg(periode)
  
  # Filtrer sur la semaine en cours si demandé
  if (periode == "semaine") {
    
    data_proj <- data_proj %>%
      filter(Period == week_obs)
    
    data_reel <- data_reel %>%
      filter(Period == week_obs)
  }
  
  data_proj %>%
    summarise(points_proj = sum(FPts), .by = c("Position", "Status", "Period")) %>%
    inner_join(
      data_reel %>%
        summarise(points_reels = sum(FPts), .by = c("Position", "Status", "Period")
        ),
      by = c("Position", "Status", "Period")
    ) %>%
    summarise(points_proj = sum(points_proj), points_reels = sum(points_reels),
              .by = all_of(group)
    ) %>%
    mutate(ecart = round(points_reels - points_proj, 0),
           rang_reel = rank(-points_reels, ties.method = "min"),
           rang_proj = rank(-points_proj, ties.method = "min"),
           ecart_rang = rang_reel - rang_proj)
}

exp_projete <- function(data_proj, data_reel, group) {
  
  data_proj %>%
    filter(optimal_proj == 1) %>%
    summarise(points_proj = sum(FPts), .by = c("Status", "Period")) %>%
    left_join(
      data_reel %>%
        filter(Roster.Status == "Active") %>%
        summarise(points_reels = sum(FPts), .by = c("Status", "Period")),
      by = c("Status", "Period")
    ) %>%
    summarise(points_proj = sum(points_proj), points_reels = sum(points_reels),
              .by = all_of(group)
    ) %>%
    mutate(ecart = round(points_reels - points_proj, 0),
           rang_reel = rank(-points_reels, ties.method = "min"),
           rang_proj = rank(-points_proj, ties.method = "min"),
           ecart_rang = rang_reel - rang_proj)
}

data_reel_optim <- function(data_reel, group,
                            periode = c("cumulatif", "semaine")) {
  
  periode <- match.arg(periode)
  
  # Filtrer sur la semaine en cours si demandé
  if (periode == "semaine") {
    
    data_reel <- data_reel %>%
      filter(Period == week_obs)
  }
  
  data_reel %>%
    group_by(.data[[group]]) %>%
    summarise(points_reels = sum(FPts[Roster.Status == "Active"], na.rm = TRUE),
              .groups = "drop") %>%
    inner_join(
      data_reel %>%
        group_by(.data[[group]]) %>%
        summarise(points_optim = sum(FPts[optimal_reel == 1], na.rm = TRUE),
                  .groups = "drop"),
      by = all_of(group)
    ) %>%
    mutate(
      efficiency = points_reels / points_optim,
      ecart_optimal = points_optim - points_reels
    ) %>%
    arrange((points_reels), desc(efficiency))
  
}
