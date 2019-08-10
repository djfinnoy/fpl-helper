# Function for generating the dataset that will be used to create predictions
# -----------------------------------------------------------------------------
gen_cs_model_data <- function(data_path = "../../data") {
  require(dplyr)
  require(tidyr)
  require(purrr)
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

  # Feature engineering for teams
  teams <- data %>% 
    group_by(team_name, opponent_team, kickoff_time, gameweek) %>% 
    summarize(
      set = first(set),
      team_elo_strength = first(team_elo_strength),
      opponent_elo_strength = first(opponent_elo_strength),
      clean_sheet = max(clean_sheets),
      goals_scored = sum(goals_scored),
      goals_conceded = max(goals_conceded),
      was_home = first(was_home),
      threat = sum(threat),
      influence = sum(influence),
      creativity = sum(creativity),
      ict_index = sum(ict_index),
      bps = sum(bps),
      big_chances_created = sum(big_chances_created),
      completed_passes = sum(completed_passes)
    ) %>% 
    arrange(team_name, kickoff_time) %>% 
    group_by(team_name) %>% 
    filter(n() >= 5) %>%   # 3 observations from upcoming, + minumum 2 required
    mutate(
      goals_scored_smooth = holt_exp_smoothing(goals_scored),
      goals_conceded_smooth = holt_exp_smoothing(goals_conceded),
      threat_smooth = holt_exp_smoothing(threat),
      influence_smooth = holt_exp_smoothing(influence),
      creativity_smooth = holt_exp_smoothing(creativity),
      ict_index_smooth = holt_exp_smoothing(ict_index),
      bps_smooth = holt_exp_smoothing(bps),
      big_chances_created_smooth = holt_exp_smoothing(big_chances_created),
      completed_passes_smooth = holt_exp_smoothing(completed_passes)
    ) %>% 
    ungroup() %>% 
    mutate(
      clean_sheet = factor(clean_sheet),
      elo_strength_diff = team_elo_strength - opponent_elo_strength
    ) %>% 
    select(
      set, team_name, opponent_team, kickoff_time, gameweek,
      clean_sheet, was_home, ends_with("_smooth"), contains("elo_")
    ) %>% 
    drop_na(-clean_sheet)  # Not known for upcoming fixtures

  # Feature engineering for each team's defensive players
  defense <- data %>% 
    group_by(team_name, kickoff_time) %>% 
    nest() %>% 
    mutate(
      defense = map(data, function(x) {
        x %>% 
          filter(position %in% c("GK", "DEF") & (minutes > 0 | is.na(minutes))) %>% 
          mutate(temp = 1) %>% 
          group_by(temp) %>% 
          summarize(
            defense_cbi = sum(clearances_blocks_interceptions),
            defense_tackles = sum(tackles),
            defense_tackled = sum(tackled),
            defense_fouls = sum(fouls),
            defense_errors = sum(errors_leading_to_goal, errors_leading_to_goal_attempt),
            defense_recoveries = sum(recoveries),
            defense_saves = sum(saves),
            defense_selected = sum(selected)
          ) %>% 
          select(-temp)
      })
    ) %>%
    select(-data) %>% 
    unnest() %>% 
    arrange(team_name, kickoff_time) %>% 
    group_by(team_name) %>% 
    filter(n() >= 5) %>%  # 3 observations from upcoming, + minumum 2 required
    mutate(
      defence_cbi_smooth = holt_exp_smoothing(defense_cbi),
      defense_tackles_smooth = holt_exp_smoothing(defense_tackles),
      defense_tackled_smooth = holt_exp_smoothing(defense_tackled),
      defense_fouls_smooth = holt_exp_smoothing(defense_fouls),
      defense_errors_smooth = holt_exp_smoothing(defense_errors),
      defense_recoveries_smooth = holt_exp_smoothing(defense_recoveries),
      defense_saves_smooth = holt_exp_smoothing(defense_saves),
      defense_selected = holt_exp_smoothing(defense_selected)
    ) %>% 
    select(team_name, kickoff_time, ends_with("smooth")) %>% 
    drop_na()
  
  # Combine the above
  model_data <- left_join(teams, defense, by = c("team_name", "kickoff_time")) %>% 
    mutate(
      temp = paste(
        ifelse(was_home, team_name, opponent_team),
        "vs",
        ifelse(was_home, opponent_team, team_name),
        kickoff_time
      )
    ) %>% 
    arrange(kickoff_time, temp, was_home) %>% 
    mutate(
      opponent_goals_scored_smooth = {
        . -> x; x %>% 
          arrange(kickoff_time, temp, -was_home) %>% 
          pull(goals_scored_smooth)
      },
      opponent_threat_smooth = {
        . -> x; x %>% 
          arrange(kickoff_time, temp, -was_home) %>% 
          pull(threat_smooth)
      },
      opponent_ict_index_smooth = {
        . -> x; x %>% 
          arrange(kickoff_time, temp, -was_home) %>% 
          pull(ict_index_smooth)     
      },
      opponent_bps_smooth = {
        . -> x; x %>% 
          arrange(kickoff_time, temp, -was_home) %>% 
          pull(bps_smooth)     
      }
    ) %>% 
    select(-temp) %>% 
    arrange(kickoff_time, team_name)

  # Return the processed dataset
  model_data
}


# Function for generating a team clean sheet 'probabilities' table
# -----------------------------------------------------------------------------
gen_cs_model_predictions <- function(model_data) {
  require(dplyr)
  require(mlr)
  require(xgboost)
  
  features <- c(
    "clean_sheet",
    "team_elo_strength", "elo_strength_diff",
    "was_home", "goals_conceded_smooth", "threat_smooth",
    "bps_smooth", "defense_tackles_smooth",
    "defense_tackled_smooth", "defense_saves_smooth",
    "opponent_goals_scored_smooth", "opponent_threat_smooth", 
    "opponent_ict_index_smooth"
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
    target = "clean_sheet",
    positive = 1
  )
  
  # Define a learner (model) with pretuned hyperparameters
  tuned_xgb_learner <- makeLearner(
    "classif.xgboost",
    predict.type = "prob",
    eval_metric = "auc",
    objective = "binary:logistic",
    eta = 0.2214595,
    nrounds = 168,
    max_depth = 1,
    colsample_bytree = 0.4367378,
    min_child_weight = 1.676057,
    subsample = 0.3964952,
    alpha = 2.721789,
    lambda = 4.282457,
    gamma = 4.45842,
    early_stopping_rounds = 17
  )
  
  # Train the model
  model <- train(tuned_xgb_learner, train_task)
  
  # Generate the predictions
  preds <- predict(model, newdata = test)
  
  # Output an organized table
  model_data %>% 
    filter(set == "test") %>% 
    mutate(cs_prob = as.data.frame(preds)$prob.1) %>% 
    select(cs_prob, team_name, opponent_team, gameweek) %>% 
    arrange(gameweek, -cs_prob)
}