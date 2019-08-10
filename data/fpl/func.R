# Function for retrieving data on individual players from the FPL api
# ------------------------------------------------------------------------------
fetch_fpl_player_data <- function(url = "https://fantasy.premierleague.com/api/") {
  require(dplyr)
  require(purrr)
  require(stringr)
  require(jsonlite)

  # Lookup table for team names
  teams <- fromJSON(paste0(url, "bootstrap-static"))$teams %>% 
    select(team_name = name, code, id)
  
  # Player summaries and fixtures, will return latest player summary
  players <- fromJSON(paste0(url, "bootstrap-static"))$elements %>% 
    # Fix names, position and current status
    mutate(
      name = paste(first_name, second_name),
      team_name = map_chr(team_code, function(x) {
        teams$team_name[which(teams$code == x)]
      }),
      position = case_when(
        element_type == 1 ~ "GK",
        element_type == 2 ~ "DEF",
        element_type == 3 ~ "MID",
        element_type == 4 ~ "FWD"
      ),
      status = case_when(
        status == "a" ~ "Available",
        status == "i" ~ "Injured",
        status == "u" ~ "Unavailable",
        status == "d" ~ "Doubtful",
        status == "n" ~ "On loan",
        status == "s" ~ "Suspended"
      )
    ) %>% 
    # Order columns, and drop unwanted variables
    select(
      player_id = id, name, position, team_code, team_name, status,
      price = now_cost
    ) %>% 
    # Add data from the `element-summary` api subdomain
    mutate(
      element_summary = map(player_id, function(x) {
        fromJSON(paste0(url, "element-summary/", x, "/"))   
      }),
      fixtures = map(element_summary, function(x) {
        x$fixtures %>% 
          mutate(
            kickoff_time = map_chr(kickoff_time, function(y) {
              str_replace(y, "T", " ") %>% 
                str_replace(., "Z", "")
            }) %>% as.POSIXct(),
            team_h = map_chr(team_h, function(x) {
              teams$team_name[which(teams$id == x)]
            }),
            team_a = map_chr(team_a, function(x) {
              teams$team_name[which(teams$id == x)]
            })         
          ) %>% 
          select(
            kickoff_time, team_h, team_a, gameweek = event, finished, is_home
          ) %>% 
          arrange(kickoff_time)
      }),
      history = map(element_summary, function(x) x$history)
    ) %>% 
    select(-element_summary)
  
  return(players)
}


# Function for writing the latest FPL player dataset to disk
# ------------------------------------------------------------------------------
gen_fpl_player_data_latest <- function(data_path = "..") {
  require(dplyr)
  require(jsonlite)
  require(readr)
  
  fetch_fpl_player_data() %>% 
    toJSON(., "rows") %>% 
    write_file(., paste0(data_path, "/fpl/fpl_player_data_latest.json"))
}
