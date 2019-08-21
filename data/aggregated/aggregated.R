## Function for merging and cleaning the `vaastav` and FPL `players` datasets,
# leaving us with a complete overview over each player's history.
# Upcoming fixtures are included for convenience, note the `finished` column.
# ==============================================================================
gen_player_fixtures_complete <- function(data_path = "..", refresh = F) {
  require(dplyr)
  require(purrr)
  require(tidyr)
  require(stringr)
  require(lubridate)
  
  # Load the FPL players dataset
  # ----------------------------------------------------------------------------
  players_file_path <- paste0(data_path, "/fpl/players.RDS")
  if (refresh & file.exists(players_file_path)) 
    file.remove(paste0(data_path, "/fpl/players.RDS"))
  if (!file.exists(players_file_path)) {
    source(paste0(data_path, "/fpl/fpl.R"))
    gen_fpl_player_data(data_path)
  }
  
  players <- readRDS(players_file_path) %>% 
    # Pre-merge cleaning operations
    mutate(
      # Add `element_type` (position) to `history` column
      history = map2(history, element_type, function(x, y) {
        x$element_type <- y; return(x)
      })
    ) 
  
  # Create a dataset on upcoming fixtures in the same format as `history`
  # ---------------------------------------------------------------------------- 
  upcoming_fixtures <- players %>% 
    select(-history) %>% 
    unnest() %>% 
    mutate(
      opponent_team = ifelse(is_home, team_a, team_h),
      kickoff_time = as.POSIXct(kickoff_time, format = "%Y-%m-%dT%H:%M:%S")
    ) %>% 
    select(
      name, player_code, element_type, round = event, opponent_team, 
      kickoff_time, was_home = is_home, finished, fixture = code
    )
  
  # Load the vaastav datasets
  # ----------------------------------------------------------------------------
  vaastav_file_path <- paste0(data_path, "/vaastav/vaastav.RDS")
  if (!file.exists(vaastav_file_path)) {
    source(paste0(data_path, "/vaastav/vaastav.R"))
    gen_vaastav_data(data_path)
  }
  
  vaastav <- readRDS(vaastav_file_path) %>% 
    # Remove variables not present in the `players` dataset
    mutate(
      history = map(history, function(x) {
        x %>% 
          select(names(players$history[[1]]))
      })
    )
    
  team_ids <- readRDS(paste0(data_path, "/vaastav/team_ids.RDS"))
  
  # Load clubelo data
  # ----------------------------------------------------------------------------
  elo_file_path <- paste0(data_path, "/clubelo/team_elo_data.RDS")
  if (refresh & file.exists(elo_file_path))
    file.remove(elo_file_path)
  if (!file.exists(elo_file_path)) {
    source(paste0(data_path, "/clubelo/clubelo.R"))
    gen_team_elo_data(data_path)
  }
  
  team_elo_data <- readRDS(elo_file_path)
  
  # Function for retrieving elo scores for a given team on a given date
  elo_value_lookup <- function(team, date) {
    require(dplyr)
    require(purrr)
    require(lubridate)
    
    # Get the index of the desired value
    index <- which(
      team_elo_data$team == team & 
      date %within% interval(team_elo_data$from, team_elo_data$to)
    )
    
    # In case we don't have an elo score for a future fixture, use latest score
    if (length(index) == 0) {
      index <- which(team_elo_data$team == team) %>% max()
    }
    
    # Return the score
    team_elo_data$elo[index]
  }
 
  # Merge and clean
  # ----------------------------------------------------------------------------
  player_fixtures_complete <- vaastav %>%
    # Merge `vaastav` and `players`
    left_join(
      players %>% select(player_code, current_season_history = history),
      by = "player_code"
    ) %>% 
    # Merge the `history` columns
    mutate(
      history = map2(history, current_season_history, function(x, y) {
        if(is.null(y)) return(x)
        else return(bind_rows(x, y))
      })
    ) %>% 
    select(-current_season_history) %>% 
    # Convert from nested to long
    unnest() %>% 
    # Add a `finished` column before adding `upcoming_fixtures`
    mutate(finished = T) %>% 
    bind_rows(., upcoming_fixtures) %>% 
    # Cleaning
    mutate(
      # Add a `season` helper variable
      season = map_chr(kickoff_time, function(x) {
        year <- year(x); month <- month(x)
        case_when(
          month %in% c(1:7) ~ paste(year - 1, str_sub(year, 3, 4), sep = "-"),
          month %in% c(8:12) ~ paste(year, str_sub(year + 1, 3, 4), sep = "-")
        )
      }),
      # Add a more intuitive column indicating a player's position
      position = case_when(
        element_type == 1 ~ "GK",
        element_type == 2 ~ "DEF",
        element_type == 3 ~ "MID",
        element_type == 4 ~ "FWD"
      )
    ) %>% 
    # Replace `opponent_team` id values with team names
    rename(opponent_team_id = opponent_team) %>% 
    left_join(
      team_ids %>% 
        select(season, opponent_team_id = id, opponent_team = team),
      by = c("season", "opponent_team_id")
    ) %>% 
    select(-opponent_team_id) %>% 
    # Add a `team` variable for indicating what team a player belongs to
    left_join(
      {. -> x; x %>% 
        group_by(season, fixture, was_home) %>% 
        summarize(team = first(opponent_team)) %>% 
        mutate(was_home = !was_home)},
      by = c("season", "fixture", "was_home")
    ) %>%
    # Add clubelo strength variables
    group_by(team, opponent_team, kickoff_time) %>% 
    nest() %>% 
    mutate(
      team_elo_strength = 
        map2_dbl(team, kickoff_time, ~elo_value_lookup(.x, .y)),
      opponent_elo_strength = 
        map2_dbl(opponent_team, kickoff_time, ~elo_value_lookup(.x, .y))
    ) %>% 
    unnest() %>% 
    ungroup() %>% 
    # Order columns
    select(
      player_code, name, kickoff_time, season, team, opponent_team, position,
      everything(), -element_type, -element
    ) %>% 
    # Order rows
    arrange(season, kickoff_time, team)
    
  # Save the dataset to disk
  # ----------------------------------------------------------------------------
  file_path <- paste0(data_path, "/aggregated/player_fixtures_complete.RDS")
  saveRDS(player_fixtures_complete, file_path)
}