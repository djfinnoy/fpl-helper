# Function for compiling csv files in vaastav/Fantasy-Premier-League into a
# long dataset that's easily mergable with recent data from the FPL API
# ------------------------------------------------------------------------------
gen_vaastav_data <- function(
  data_path = "..", 
  seasons = c("2016-17", "2017-18", "2018-19")
) {
  require(dplyr)
  require(tibble)
  require(readr)
  require(purrr)
  require(tidyr)
  
  # Start with a tibble over all gameweeks in `seasons`
  vaastav <- tibble(
    season = map(seasons, ~rep(.x, 38)) %>% flatten_chr(),
    gameweek = rep(1:38, length(seasons)) %>% paste0("gw", .)
  ) %>% 
    # Download the gameweek .csv files from vaastav's repository
    mutate(
      gw_data = map2(season, gameweek, function(x, y) {
        paste0(
          "https://raw.githubusercontent.com/",
          "vaastav/Fantasy-Premier-League/master/data/",
          x, "/gws/", y, ".csv"
        ) %>% 
          read_csv(., col_types = cols())
      })
    ) %>% 
    # Merge gameweek tables into one long dataset for each season
    group_by(season) %>% 
    summarize(gw_data = list(bind_rows(gw_data))) %>% 
    ungroup() %>% 
    mutate(
      # Download additional data about players from the players_raw.csv files
      player_info = map(season, function(x) {
        paste0(
          "https://raw.githubusercontent.com/",
          "vaastav/Fantasy-Premier-League/master/data/",
          x, "/players_raw.csv"
        ) %>% 
          read_csv(., col_types = cols()) %>% 
          # Minor cleaning operations
          mutate(name = paste(first_name, second_name)) %>% 
          # Keep variables we want to add to the gameweek data
          select(element = id, player_code = code, name, element_type)      
      }),
      # Add useful columns from `player_info` to the gameweek data table
      gw_data = map2(gw_data, player_info, function(x, y) {
        x %>% 
          # The `name` variable from the gameweek files is corrupt..
          select(-name) %>% 
          # .. so we'll replace it with `name` from `player_info`
          left_join(., y, by = "element")
      })
    ) %>% 
    # Combine the modified gameweek tables into one long dataset
    pull(gw_data) %>% 
    map_dfr(., ~bind_rows(.x)) %>% 
    # Nest the data by players
    group_by(player_code, name) %>% 
    nest(.key = "history") %>% 
    # Deal with name changes
    group_by(player_code) %>% 
    summarize(
      name = last(name),
      history = list(bind_rows(history))
    ) %>% 
    ungroup()
  
  # Save the dataset to disk
  file_path <- paste0(data_path, "/vaastav/vaastav.RDS")
  saveRDS(vaastav, file_path)
}


# Function for creating a complete overview of team names and id numbers
# ------------------------------------------------------------------------------
gen_team_name_id_lookup <- function(data_path = "..") {
  require(dplyr)
  require(tibble)
  
  # Team name / code overview (update when new teams enter PL)
  team_names <- tribble(
    ~code, ~name,
    1, "Man Utd",
    3, "Arsenal",
    4, "Newcastle",
    6, "Spurs",
    7, "Aston Villa",
    8, "Chelsea",
    11, "Everton",
    13, "Leicester",
    14, "Liverpool",
    20, "Southampton",
    21, "West Ham",
    25, "Middlesbrough",
    31, "Crystal Palace",
    35, "West Bromwich Albion",
    36, "Brighton",
    38, "Huddersfield",
    39, "Wolves",
    43, "Man City",
    45, "Norwich",
    49, "Sheffield Utd",
    54, "Fulham",
    56, "Sunderland",
    57, "Watford",
    80, "Swansea City",
    88, "Hull",
    90, "Burnley",
    91, "Bournemouth",
    97, "Cardiff",
    110, "Stoke City"
  )
  
  # Download season specific team `id` / `code` bridging
  team_ids <- tibble(
    # Update this line to include all seasons since 2016
    season = c("2016-17","2017-18","2018-19", "2019-20")
  ) %>% 
    mutate(
      player_info = map(season, function(x) {
        paste0(
          "https://raw.githubusercontent.com/",
          "vaastav/Fantasy-Premier-League/master/data/",
          x, "/players_raw.csv"
        ) %>% 
          read_csv(col_types = cols()) %>% 
          select(team, team_code) %>% 
          distinct()
      })
    ) %>% 
    unnest() %>% 
    as_tibble() %>% 
    select(season, id = team, code = team_code) %>% 
    # Add team names
    left_join(team_names, by = "code") %>% 
    arrange(season, id)  
  
  # Save dataset to disk
  file_path <- paste0(data_path, "/vaastav/team_ids.RDS")
  saveRDS(team_ids, file_path)
}