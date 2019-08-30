# Function for generating the dataset that will be used by our model
# -----------------------------------------------------------------------------
gen_cs_model_data <- function(data_path = "../../data") {
  require(dplyr)
  require(tidyr)
  
  # Load required home-made functions
  func_path <- paste0(data_path, "/../models/func.R")
  source(func_path)
  
  # Load data
  player_fixtures <- 
    readRDS(paste0(data_path, "/aggregated/player_fixtures_complete.RDS"))
  
  # Load goal predictions
  goal_model_path <- paste0(data_path, "/../models/predict-goal/")
  source(paste0(goal_model_path, "gen_goal_model_data.R"))
  source(paste0(goal_model_path, "gen_goal_model_predictions.R"))
  goal_pred <- gen_goal_model_data(data_path) %>% gen_goal_model_predictions()
 
  # Feature engineering
  team_features <- player_fixtures %>% 
    group_by(team, round, season) %>% 
    summarize(
      opponent_team = first(opponent_team),
      kickoff_time = first(kickoff_time),
      finished = first(finished),
      team_elo_strength = first(team_elo_strength),
      opponent_elo_strength = first(opponent_elo_strength),
      elo_strength_diff = team_elo_strength - opponent_elo_strength,
      goals_scored = sum(goals_scored),
      goals_conceded = max(goals_conceded),
      clean_sheet = (goals_conceded == 0) %>% as.integer,
      was_home = first(was_home) %>% as.integer,
      threat = sum(threat),
      influence = sum(influence),
      creativity = sum(creativity),
      ict_index = sum(ict_index),
      bps2 = sum(bps2)
    ) %>% 
    arrange(team, kickoff_time) %>% 
    group_by(team) %>% 
    filter(sum(finished) >= 3) %>% 
    mutate(
      goals_scored_smooth = holt_exp_smoothing(goals_scored),
      goals_conceded_smooth = holt_exp_smoothing(goals_conceded),
      threat_smooth = holt_exp_smoothing(threat),
      influence_smooth = holt_exp_smoothing(influence),
      creativity_smooth = holt_exp_smoothing(creativity),
      ict_index_smooth = holt_exp_smoothing(ict_index),
      bps2_smooth = holt_exp_smoothing(bps2)
    ) %>% 
    ungroup() %>% 
    select(
      team, opponent_team, kickoff_time, round, season,
      clean_sheet, was_home, finished, ends_with("smooth"), contains("elo_")
    ) %>% 
    drop_na(-clean_sheet)
  
  goal_prob_features <- goal_pred %>% 
    group_by(team, kickoff_time) %>% 
    summarize(
      goal_probabilities = list(goal_prob)
    ) %>% 
    ungroup() %>% 
    mutate(
      # Median probability of top quartile
      agg_goal_prob = map_dbl(goal_probabilities, function(x) {
        top_quartile <- quantile(x, 0.75) %>% as.numeric()
        x[x >= top_quartile] %>% median()
      })
    ) %>% 
    select(-goal_probabilities)
  
  model_data <- team_features %>% 
    left_join(goal_prob_features, by = c("team", "kickoff_time")) %>% 
    left_join(
      {. -> x; x %>% 
        select(
          kickoff_time, opponent_team = team,
          opponent_goals_smooth = goals_scored_smooth,
          opponent_threat_smooth = threat_smooth,
          opponent_influence_smooth = influence_smooth,
          opponent_creativity_smooth = creativity_smooth,
          opponent_ict_index_smooth = ict_index_smooth,
          opponent_bps2_smooth = bps2_smooth,
          opponent_agg_goal_prob = agg_goal_prob
        )},
      by = c("opponent_team", "kickoff_time")
    )
 
  return(model_data)
}