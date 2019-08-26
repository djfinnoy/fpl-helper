# Function for generating players assist 'probability' table
# -----------------------------------------------------------------------------
gen_assist_model_predictions <- function(model_data) {
  require(dplyr)
  require(mlr)
  require(xgboost)
  
  # Variables used by the model
  features <- c(
    "assist",
    "elo_strength_diff", 
    "assists_smooth", "bps2_smooth", "creativity_smooth", 
    "influence_smooth", "minutes_smooth", 
    "team_goals_scored_smooth", "team_bps2_smooth", 
    "opponent_goals_conceded_smooth", "opponent_ict_index_smooth", 
    "opponent_bps2_smooth"
  )
  
  # Dataset for training the model
  train <- model_data %>% 
    filter(finished) %>% 
    select(features) %>% 
    as.data.frame()
  
  # Dataset to predict using the trained model
  # Predicting all observations, including those in train set
  test <- model_data %>% 
    select(features) %>% 
    as.data.frame()
  
  # Define `mlr` task
  train_task <- makeClassifTask(
    data = train,
    target = "assist",
    positive = 1
  )
  
  tuned_xgb_learner <- makeLearner(
    "classif.xgboost",
    predict.type = "prob",
    eval_metric = "auc",
    objective = "binary:logistic",
    eta = 0.0258,
    nrounds = 118,
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
    mutate(assist_prob = as.data.frame(preds)$prob.1) %>% 
    select(
      assist_prob, player_code, name, team, opponent_team, 
      finished, kickoff_time, round, season
    ) %>% 
    arrange(kickoff_time, -assist_prob)
}