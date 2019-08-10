# Function for generating the dataset that will be used to create predictions
# -----------------------------------------------------------------------------
gen_assist_model_data <- function(data_path = "../../data") {
  require(dplyr)
  require(tidyr)
  require(jsonlite)
 
  # Load required functionality
  source(paste0(data_path, "/../functions/holt_exp_smoothing.R"))
  
  # Load data
  grand_unified <- 
    fromJSON(paste0(data_path, "/modelling/grand_unified_latest.json"))
  upcoming_fixtures <- 
    fromJSON(paste0(data_path, "/modelling/upcoming_player_fixtures_latest.json"))
  
  # Combine data on historical and upcoming player fixtures
  grand_unified$set <- "train"
  upcoming_fixtures$set <- "test"
  data <- bind_rows(grand_unified, upcoming_fixtures)
  
  # Feature engineering for individual players
  individuals <- data %>% 
    filter(position != "GK") %>% 
    mutate(
      position = factor(position),
      elo_strength_diff = team_elo_strength - opponent_elo_strength
    ) %>% 
    arrange(xseason_id, kickoff_time) %>% 
    group_by(xseason_id) %>% 
    filter(n() >= 5) %>%   # 3 from upcoming, minimum 2 from grand unified
    mutate(
      assist = as.integer(assists > 0),
      assists_smooth = holt_exp_smoothing(assists),
      goals_scored_smooth = holt_exp_smoothing(goals_scored),
      key_passes_smooth = holt_exp_smoothing(key_passes),
      threat_smooth = holt_exp_smoothing(threat),
      creativity_smooth = holt_exp_smoothing(creativity),
      selected_smooth = holt_exp_smoothing(selected),
      dribbles_smooth = holt_exp_smoothing(dribbles)
    ) %>% 
    ungroup() %>% 
    select(
      xseason_id, name, set, kickoff_time, team_name,
      opponent_team, season, gameweek, minutes, 
      position, assist, was_home, contains("elo_"), ends_with("_smooth")
    ) %>% 
    drop_na(-minutes, -assist)  # Not known for upcoming fixtures
  
  # Feature engineering for teams
  teams <- data %>% 
    group_by(team_name, kickoff_time) %>% 
    summarize(
      team_goals_scored = sum(goals_scored),
      team_goals_conceded = max(goals_conceded),
      team_threat = sum(threat),
      team_ict_index = sum(ict_index)
    ) %>% 
    arrange(team_name, kickoff_time) %>% 
    group_by(team_name) %>% 
    filter(n() >= 5) %>% 
    mutate(
      team_goals_scored_smooth = holt_exp_smoothing(team_goals_scored),
      team_goals_conceded_smooth = holt_exp_smoothing(team_goals_conceded),
      team_threat_smooth = holt_exp_smoothing(team_threat),
      team_ict_index_smooth = holt_exp_smoothing(team_ict_index)
    ) %>% 
    ungroup() %>% 
    select(team_name, kickoff_time, ends_with("smooth")) %>% 
    drop_na()
  
  # Combine individual and team datasets
  model_data <- 
    left_join(individuals, teams, by = c("team_name", "kickoff_time")) %>% 
    left_join(., 
      teams %>% 
        select(
          kickoff_time, opponent_team = team_name,  
          opponent_goals_conceded_smooth = team_goals_conceded_smooth,
          opponent_ict_index_smooth = team_ict_index_smooth,
          opponent_threat_smooth = team_threat_smooth
        ),
      by = c("opponent_team", "kickoff_time")
    ) %>% 
    filter(minutes %>% is.na | minutes > 0) %>% 
    select(-minutes)
  
  model_data
}


# Function for generating a player assist 'probabilities' table
# -----------------------------------------------------------------------------
gen_assist_model_predictions <- function(model_data) {
  require(dplyr)
  require(mlr)
  require(xgboost)
  
  # Variables used by the model
  features <- c(
    "assist",
    "team_elo_strength", "elo_strength_diff",
    "assists_smooth", "key_passes_smooth", "threat_smooth",
    "creativity_smooth", "selected_smooth", "dribbles_smooth",
    "team_goals_scored_smooth", "team_ict_index_smooth", "opponent_threat_smooth"
  )
  
  # Dataset for training the model
  train <- model_data %>% 
    filter(set == "train") %>% 
    select(features) %>% 
    as.data.frame()
  
  # Dataset to predict using trained model
  test <- model_data %>% 
    filter(set == "test") %>% 
    select(features) %>% 
    as.data.frame()
 
  # `mlr` task to simplify training
  train_task <- makeClassifTask(
    data = train,
    target = "assist",
    positive = 1
  )
  
  # Define a learner (model) with pretuned hyperparameters
  tuned_xgb_learner <- makeLearner(
    "classif.xgboost",
    predict.type = "prob",
    eval_metric = "auc",
    objective = "binary:logistic",
    eta = 0.0336,
    nrounds = 645,
    max_depth = 4,
    colsample_bytree = 0.539,
    min_child_weight = 19.5,
    subsample = 0.822,
    alpha = 6.094763,
    lambda = 6.042612,
    gamma = 4.822022,
    early_stopping_rounds = 24
  )
  
  # Train the model
  model <- train(tuned_xgb_learner, train_task)
  
  # Generate the predictions
  preds <- predict(model, newdata = test)
  
  # Output an organized table
  model_data %>% 
    filter(set == "test") %>% 
    mutate(assist_prob = as.data.frame(preds)$prob.1) %>% 
    select(
      assist_prob, xseason_id, name, team_name, opponent_team, gameweek
    ) %>% 
    arrange(gameweek, -assist_prob)
}
