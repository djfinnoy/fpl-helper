# Function for generating the dataset that will be used by our model
# -----------------------------------------------------------------------------
gen_goal_model_data <- function(data_path = "../../data") {
  require(dplyr)
  require(tidyr)
  
  # Load required home-made functions
  func_path <- paste0(data_path, "/../models/func.R")
  source(func_path)
  
  # Load data
  player_fixtures <- 
    readRDS(paste0(data_path, "/aggregated/player_fixtures_complete.RDS"))
  
  # Feature engineering
  player_features <- player_fixtures %>% 
    filter(position != "GK") %>% 
    mutate(
      position = factor(position),
      elo_strength_diff = team_elo_strength - opponent_elo_strength
    ) %>% 
    arrange(player_code, kickoff_time) %>% 
    group_by(player_code) %>% 
    # Drop players with less than 3 recorded fixtures
    filter(sum(finished) >= 3) %>% 
    mutate(
      goal = as.integer(goals_scored > 0),
      # Smoothing
      goals_smooth = holt_exp_smoothing(threat),
      bps2_smooth = holt_exp_smoothing(bps2),
      creativity_smooth = holt_exp_smoothing(creativity),
      influence_smooth = holt_exp_smoothing(influence),
      ict_index_smooth = holt_exp_smoothing(ict_index),
      minutes_smooth = holt_exp_smoothing(minutes)
    ) %>% 
    ungroup() %>% 
    select(
      # Helper variables
      player_code, name, position, finished, kickoff_time, 
      minutes, team, opponent_team, season, round, 
      # Features
      goal, contains("elo_"), ends_with("_smooth")
    ) %>% 
    drop_na(-goal, -minutes)  # Not known for upcoming fixtures
  
  team_features <- player_fixtures %>% 
    filter(sum(finished) >= 3) %>% 
    group_by(team, kickoff_time) %>% 
    summarize(
      team_goals_scored = sum(goals_scored),
      team_goals_conceded = sum(goals_conceded),
      team_threat = sum(threat),
      team_ict_index = sum(ict_index),
      team_bps2 = sum(bps2)
    ) %>% 
    arrange(team, kickoff_time) %>% 
    group_by(team) %>% 
    mutate(
      team_goals_scored_smooth = holt_exp_smoothing(team_goals_scored),
      team_goals_conceded_smooth = holt_exp_smoothing(team_goals_conceded),
      team_threat_smooth = holt_exp_smoothing(team_threat),
      team_ict_index_smooth = holt_exp_smoothing(team_ict_index),
      team_bps2_smooth = holt_exp_smoothing(team_bps2)
    ) %>% 
    ungroup() %>% 
    select(team, kickoff_time, ends_with("smooth")) %>% 
    drop_na()
  
  model_data <- player_features %>% 
    left_join(team_features, by = c("team", "kickoff_time")) %>% 
    left_join(
      team_features %>% 
        select(
          kickoff_time, opponent_team = team,
          opponent_goals_conceded_smooth = team_goals_conceded_smooth,
          opponent_ict_index_smooth = team_ict_index_smooth,
          opponent_bps2_smooth = team_bps2_smooth
        ),
      by = c("opponent_team", "kickoff_time")
    ) %>% 
    filter(is.na(minutes) | minutes > 0) %>% 
    select(-minutes)
  
  return(model_data)
}