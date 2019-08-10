# Function for generating the dataset that will be used to create predictions
# -----------------------------------------------------------------------------
gen_goal_model_data <- function(data_path = "../../data") {
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
      goal = as.integer(goals_scored > 0),
      goals_scored_smooth = holt_exp_smoothing(goals_scored),
      threat_smooth = holt_exp_smoothing(threat),
      creativity_smooth = holt_exp_smoothing(creativity),
      influence_smooth = holt_exp_smoothing(influence),
      ict_index_smooth = holt_exp_smoothing(ict_index),
      minutes_smooth = holt_exp_smoothing(minutes),
      selected_smooth = holt_exp_smoothing(selected)
    ) %>% 
    ungroup() %>% 
    select(
      xseason_id, name, set, kickoff_time, team_name,
      opponent_team, season, gameweek, minutes, position,
      goal, contains("elo_"), ends_with("_smooth")
    ) %>% 
    drop_na(-minutes, -goal)  # Not known for upcoming fixtures
  
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


# Function for generating a player goal scoring 'probabilities' table
# -----------------------------------------------------------------------------
gen_goal_model_predictions <- function(model_data) {
  require(dplyr)
  require(mlr)
  require(xgboost)
  
  # Variables used by the model
  features <- c(
    "goal",
    "opponent_elo_strength", "elo_strength_diff", "goals_scored_smooth",
    "threat_smooth", "ict_index_smooth", "selected_smooth", 
    "team_threat_smooth", "team_ict_index_smooth",
    "opponent_goals_conceded_smooth", "opponent_threat_smooth"
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
    target = "goal",
    positive = 1
  )
  
  # Define a learner (model) with pretuned hyperparameters
  tuned_xgb_learner <- makeLearner(
    "classif.xgboost",
    predict.type = "prob",
    eval_metric = "auc",
    objective = "binary:logistic",
    eta = 0.2224558,
    nrounds = 350,
    max_depth = 6,
    colsample_bytree = 0.5143675,
    min_child_weight = 7.677459,
    subsample = 0.887328,
    alpha = 2.409472,
    lambda = 2.00181,
    gamma = 8.981761,
    early_stopping_rounds = 17
  )
  
  # Train the model
  model <- train(tuned_xgb_learner, train_task)
  
  # Generate the predictions
  preds <- predict(model, newdata = test)
  
  # Output an organized table
  model_data %>% 
    filter(set == "test") %>% 
    mutate(goal_prob = as.data.frame(preds)$prob.1) %>% 
    select(
      goal_prob, xseason_id, name, team_name, opponent_team, gameweek
    ) %>% 
    arrange(gameweek, -goal_prob)
}
