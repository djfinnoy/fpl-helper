# Function for generating data used by the shiny app
# ------------------------------------------------------------------------------
gen_shiny_data <- function(data_path = "..", refresh = F) {
  require(dplyr)
  require(jsonlite)
  
  shiny_data_path <- paste0(
    data_path, "/../shiny/shinyapps/fpl-helper/data/"
  )
  
  # Refresh data sources if instructed to do so
  if(refresh) {
    source(paste0(data_path, "/modelling/func.R"))
    gen_eval_latest(data_path, refresh = T)
  }
  
  # Load data
  eval_latest <- fromJSON(paste0(data_path, "/modelling/eval_latest.json"))
  grand_unified <- fromJSON(paste0(data_path, "/modelling/grand_unified_latest.json"))
  elo <- fromJSON(paste0(data_path, "/clubelo.com/elo_data_latest.json"))
  fpl <- fromJSON(paste0(data_path, "/fpl/fpl_player_data_latest.json"))
  
  # Player history dataset (used by `upcomingPlayerFixturesPlotsUI`)
  player_history_latest <- grand_unified %>% 
    filter(xseason_id %in% unique(eval_latest$xseason_id)) %>% 
    select(
      xseason_id, name, position, season, kickoff_time, team_name, 
      opponent_team, gameweek, minutes, total_points
    )
  
  # Fixture difficulty dataset (used by `upcomingPlayerFixturesPlotsUI`)
  source(paste0(data_path, "/modelling/func.R"))
  fixture_difficulty_latest <- fpl %>% 
    group_by(team_name) %>% 
    summarize(fixtures = first(fixtures)) %>% 
    ungroup() %>% 
    mutate(fixtures = map(fixtures, function(x) {
      x %>% 
        mutate(
          team_name = ifelse(is_home, team_h, team_a),
          opponent_team = ifelse(is_home, team_a, team_h),
          home = as.integer(is_home)
        ) %>% 
        select(gameweek, team_name, opponent_team)
    })) %>% 
    pull(fixtures) %>% 
    map_dfr(., ~bind_rows(.x)) %>% 
    mutate(
      opponent_elo_strength = map_dbl(
        opponent_team, 
        ~elo_value_lookup(elo, .x, lubridate::today()))
    )   
  
  # Save files
  saveRDS(eval_latest, paste0(shiny_data_path, "eval_latest.RDS"))
  saveRDS(player_history_latest, paste0(shiny_data_path, "player_history_latest.RDS"))
  saveRDS(fixture_difficulty_latest, paste0(shiny_data_path, "fixture_difficulty_latest.RDS"))
}