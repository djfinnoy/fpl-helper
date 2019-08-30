# Function for generating a team clean sheet 'probability' table
# -----------------------------------------------------------------------------
gen_cs_model_predictions <- function(model_data) {
  require(dplyr)
  require(mlr)
  require(xgboost)
  
  # Variables used by the model
  features <- c(
    "clean_sheet", "was_home", "elo_strength_diff", "opponent_agg_goal_prob"
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
    target = "clean_sheet",
    positive = 1
  )
  
  # Define a learner with pre-tuned hyperparameters
  tuned_xgb_learner <- makeLearner(
    "classif.xgboost",
    predict.type = "prob",
    eval_metric = "auc",
    objective = "binary:logistic",
    eta = 0.026,
    nrounds = 247,
    max_depth = 3,
    colsample_bytree = 1,
    min_child_weight = 7,
    subsample = 1,
    alpha = 4.33,
    lambda = 4.59,
    gamma = 6.25,
    early_stopping_rounds = 19
  )
  
  # Train the model
  model <- train(tuned_xgb_learner, train_task)
  
    # Generate the predictions
  preds <- predict(model, newdata = test)
  
  # Output an organized table
  model_data %>% 
    mutate(cs_prob = as.data.frame(preds)$prob.1) %>% 
    select(
      cs_prob, team, opponent_team, 
      finished, kickoff_time, round, season
    ) %>% 
    arrange(kickoff_time, -cs_prob)
}