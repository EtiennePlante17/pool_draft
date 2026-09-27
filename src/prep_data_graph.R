comp_reelvsproj <- function(data_proj, data_reel, group) {
  
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
