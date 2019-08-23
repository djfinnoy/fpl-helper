# Function for generating a player goal scoring 'probabilities' table
# -----------------------------------------------------------------------------
gen_goal_model_predictions <- function(model_data) {
  require(dplyr)
  require(mlr)
  require(xgboost)
  
  # Variables used by the model
  features <- c()
  
  # Dataset for training the model
  train <- model_data %>% 
    filter(finished) %>% 
    select(features) %>% 
    as.data.frame()
  
  # Dataset to predict using the trained model
  test <- model_data %>% 
    filter(!finished) %>% 
    select(features) %>% 
    as.data.frame()
  
  # Define `mlr` task
  train_task <- makeClassifTask(
    data = train,
    target = "goal",
    positive = 1
  )
  
  # Define a learner with pre-tuned hyperparameters
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
    filter(!finished) %>% 
    mutate(goal_prob = as.data.frame(preds)$prob.1) %>% 
    select(
      goal_prob, player_code, name, team, opponent_team, round
    ) %>% 
    arrange(round, -goal_prob)
}