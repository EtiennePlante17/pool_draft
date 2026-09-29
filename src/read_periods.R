read_periods_proj <- function(year_draft, n_periods = 28) {
  
  map_dfr(1:n_periods, function(i) {
    
    read.csv2(here(file.path( 
        "data", year_draft, 
        paste0("projections/proj_week", i, ".csv"))),
        sep = ",", dec = ".") %>%
      mutate(Period = i)
    
  })
}

read_periods_reel <- function(year_draft, n_periods = 28) {
  
  map_dfr(1:n_periods, function(i) {
    
    read.csv2(here(file.path( 
      "data", year_draft, 
      paste0("reel/reel_week", i, ".csv"))),
      sep = ",", dec = ".") %>%
      mutate(Period = i)
    
  })
}
