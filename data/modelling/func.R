# Function for downloading the latest version of source datasets
# ------------------------------------------------------------------------------
update_source_data <- function(data_path = "..") {
  # FPL api data
  source(paste0(data_path, "/fpl/func.R"))
  gen_fpl_player_data_latest(data_path = data_path)
  
  # clubelo.com data
  source(paste0(data_path, "/clubelo.com/func.R"))
  gen_elo_data_latest(data_path = data_path)
}


# Function for retrieving elo scores for a given team on a given date
# ------------------------------------------------------------------------------
elo_value_lookup <- function(elo_data, team, date) {
  require(dplyr)
  require(purrr)
  require(lubridate)
  
  # Get the index of the desired value
  index <- which(
    elo_data$team_name == team & 
    date %within% interval(elo_data$from, elo_data$to)
  )
  
  # In case we don't have an elo score for a future fixture, use latest score
  if (length(index) == 0) {
    index <- which(elo_data$team_name == team) %>% max()
  }
  
  # Return the score
  elo_data$elo[index]
}


# Function for getting a `season` string from a date value
# ------------------------------------------------------------------------------
get_season <- function(date) {
  require(dplyr)
  require(lubridate)
  
  year <- year(date)
  month <- month(date)
  
  case_when(
    month %in% c(1:7) ~ paste(year - 1, str_sub(year, 3, 4), sep = "-"),
    month %in% c(8:12) ~ paste(year, str_sub(year + 1, 3, 4), sep = "-")
  )
}


# Function for combining multiple data sources
# ------------------------------------------------------------------------------
compile_grand_unified_dataset <- function(data_path = "..") {
  require(dplyr)
  require(tidyr)
  require(purrr)
  require(readr)
  require(lubridate)
  
  # Load the data
  vaastav <- 
    fromJSON(paste0(data_path, "/vaastav/previous_seasons_player_fixtures.json"))
  fpl <- 
    fromJSON(paste0(data_path, "/fpl/fpl_player_data_latest.json"))
  elo <- 
    fromJSON(paste0(data_path, "/clubelo.com/elo_data_latest.json"))
  
  # Team names need to be added to `fpl`
  teams <- fpl %>%
    select(team_code, team_name) %>% 
    distinct()
  
  # xseason_id needs to be added to `fpl`
  xseason_table <- vaastav %>% 
    select(xseason_id, name) %>% 
    distinct()
  
  new_players <- fpl %>% 
    left_join(., xseason_table, by = "name") %>% 
    select(xseason_id, name) %>% 
    distinct() %>% 
    filter(is.na(xseason_id)) %>% 
    arrange(name) %>% 
    mutate(xseason_id = max(xseason_table$xseason_id) + row_number())
  
  xseason_table <- rbind(xseason_table, new_players) %>% 
    arrange(xseason_id)
  
  # Start with data from previous seasons
  vaastav %>% 
    # Add the `history` datasets from fpl
    bind_rows(.,
              fpl %>%
                mutate(
                  xseason_id = map_int(name, function(x) {
                    which(xseason_table$name == x) %>% xseason_table$xseason_id[.]
                  }),
                  history = map(history, function(x) {
                    x %>%
                      # Prepare `fpl` style data for merging with vaastav
                      mutate(
                        season = map_chr(kickoff_time, ~get_season(as.Date(.x))),
                        fixture_id = paste(fixture, season, sep = "/"),
                        was_home = as.integer(was_home),
                        influence = as.numeric(influence),
                        creativity = as.numeric(creativity),
                        threat = as.numeric(threat),
                        ict_index = as.numeric(ict_index)
                      ) %>%
                      select(
                        season, gameweek = round, kickoff_time, fixture_id,
                        opponent_team, total_points, value, was_home,
                        minutes, assists, clean_sheets, saves, bonus,
                        starts_with("penalties"), ends_with("cards"), contains("goals"),
                        bps, influence, creativity, threat, ict_index, selected,
                        starts_with("transfers"), contains("_score")
                      )
                  })
                ) %>% 
                select(
                  player_id, xseason_id, name, position, team_code, team_name, history
                ) %>% 
                unnest()       
    ) %>% 
    # Add `elo_strength`, and `opponent_elo_strength`
    group_by(team_name, opponent_team, kickoff_time) %>% 
    nest() %>% 
    mutate(
      date = as.Date(kickoff_time),
      team_elo_strength = 
        map2_dbl(team_name, date, ~elo_value_lookup(elo, .x, .y)),
      opponent_elo_strength = 
        map2_dbl(opponent_team, date, ~elo_value_lookup(elo, .x, .y))     
    ) %>% 
    # Finishing up
    unnest() %>% 
    ungroup() %>% 
    select(
      xseason_id, player_id, name, position, season, kickoff_time, 
      team_name, opponent_team, gameweek, everything(),
      -date, -id, -team_a_score, -team_h_score
    )
}


# Save grand unified dataset to disk
# ------------------------------------------------------------------------------
gen_grand_unified_dataset_latest <- function(data_path = "..") {
  require(dplyr)
  require(readr)
  require(jsonlite)
  
  compile_grand_unified_dataset(data_path = data_path) %>% 
    toJSON(., "rows") %>% 
    write_file(., paste0(data_path, "/modelling/grand_unified_latest.json"))
}


# Save upcoming fixtures dataset to disk
# ------------------------------------------------------------------------------
gen_upcoming_fixtures_latest <- function(data_path = "..") {
  require(dplyr)
  require(readr)
  require(jsonlite)
  
  # Load required datasets
  fpl <- fromJSON(paste0(data_path, "/fpl/fpl_player_data_latest.json"))
  elo <- fromJSON(paste0(data_path, "/clubelo.com/elo_data_latest.json"))
  grand_unified <- fromJSON(paste0(data_path, "/modelling/grand_unified_latest.json"))
  
  # Extract data on the next three upcoming fixtures
  upcoming <- fpl %>% 
    mutate(
      upcoming = map(fixtures, function(x) {
        x %>% 
          filter(!finished) %>%
          mutate(opponent_team = ifelse(is_home, team_a, team_h)) %>% 
          select(
            kickoff_time, opponent_team, gameweek, was_home = is_home
          ) %>% 
          head(3)
      })
    ) %>% 
    select(-fixtures, -history) %>% 
    unnest() %>% 
    # Add elo values
    mutate(
      date = as.Date(kickoff_time),
      team_elo_strength = 
        map2_dbl(team_name, date, ~elo_value_lookup(elo, .x, .y)),
      opponent_elo_strength =
        map2_dbl(opponent_team, date, ~elo_value_lookup(elo, .x, .y)),
      season = map_chr(kickoff_time, ~get_season(.x)),
      was_home = as.integer(was_home)
    ) %>% 
    select(-date)
    
  # Add `xseason_id` to the upcoming fixtures dataset 
  xseason_table <- grand_unified %>% 
    select(xseason_id, name) %>% 
    distinct()
  
  new_players <- upcoming %>% 
    left_join(., xseason_table, by = "name") %>% 
    select(xseason_id, name) %>% 
    distinct() %>% 
    filter(is.na(xseason_id)) %>% 
    arrange(name) %>% 
    mutate(xseason_id = max(xseason_table$xseason_id) + row_number())

  xseason_table <- rbind(xseason_table, new_players) %>% 
    arrange(xseason_id)

  # Write to disk
  left_join(upcoming, xseason_table, by = "name") %>% 
    toJSON(., "rows") %>% 
    write_file(., paste0(data_path, "/modelling/upcoming_player_fixtures_latest.json"))
}


# Function for creating up to date versions of the modelling data
# ------------------------------------------------------------------------------
update_modelling_data <- function(data_path = "..") {
  
  update_source_data(data_path = data_path)
  gen_grand_unified_dataset_latest(data_path = data_path) 
  gen_upcoming_fixtures_latest(data_path = data_path)
}


# Function for generating a table over predictions and expected value for
# three upcoming fixtures
# ------------------------------------------------------------------------------
gen_eval_latest <- function(data_path = "..", refresh = F) {
  require(dplyr)
  require(jsonlite)
  require(readr)
  
  # Download latest data if instructed to do so (takes a few minutes)
  if (refresh) {
    source(paste0(data_path, "/modelling/func.R"))
    update_modelling_data(data_path = data_path)
  }
  
  # Get data on the upcoming player fixtures
  upcoming <- fromJSON(
    paste0(data_path, "/modelling/upcoming_player_fixtures_latest.json")
  )
  
  # Generate predictions based on locally stored data files
  source(paste0(data_path, "/../models/predict-goal-scorer/func.R"))
  source(paste0(data_path, "/../models/predict-assist/func.R")) 
  source(paste0(data_path, "/../models/predict-clean-sheet/func.R")) 
  
  goal_pred <- gen_goal_model_data(data_path = data_path) %>% 
    gen_goal_model_predictions(.)
  
  assist_pred <- gen_assist_model_data(data_path = data_path) %>% 
    gen_assist_model_predictions(.)
  
  cs_pred <- gen_cs_model_data(data_path = data_path) %>% 
    gen_cs_model_predictions(.)
  
  # Combine predictions into a single table, and return the result
  eval_table <- goal_pred %>%
    # Add assist probability
    left_join(.,
      assist_pred %>%
        select(xseason_id, gameweek, assist_prob),
      by = c("xseason_id", "gameweek")
    ) %>% 
    # Add goal keepers (not included in the goal / assist predictions)
    bind_rows(.,
      upcoming %>% 
        filter(position == "GK") %>% 
        mutate(goal_prob = 0, assist_prob = 0) %>% 
        select(
          xseason_id, name, team_name, opponent_team, 
          gameweek, goal_prob, assist_prob
        )
    ) %>% 
    # Add clean sheet probability
    left_join(.,
      cs_pred %>% 
        select(cs_prob, team_name, gameweek),
      by = c("team_name", "gameweek")
    ) %>% 
    # Add extra player-fixture information
    left_join(.,
      upcoming %>% 
        select(xseason_id, gameweek, position, status, price, home = was_home),
      by = c("xseason_id", "gameweek")
    ) %>% 
    # Add expected values
    mutate(
      goal_ev = case_when(
        position == "DEF" ~ goal_prob * 6,
        position == "MID" ~ goal_prob * 5,
        position == "FWD" ~ goal_prob * 4,
        TRUE ~ 0
      ),
      assist_ev = assist_prob * 3,
      cleansheet_ev = case_when(
        position %in% c("GK", "DEF") ~ cs_prob * 4,
        position == "MID" ~ cs_prob * 1,
        TRUE ~ 0
      ),
      total_ev = goal_ev + assist_ev + cleansheet_ev,
      evp = total_ev / price
    ) %>% 
    # Column order
    select(
      gameweek, name, team_name, opponent_team, home, total_ev,
      price, status, ends_with ("_ev"), ends_with("_prob"), 
      xseason_id, position
    )
  
  # Save file to disk
  toJSON(eval_table, "rows") %>% 
    write_file(., paste0(data_path, "/modelling/eval_latest.json"))
}