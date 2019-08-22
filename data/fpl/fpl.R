# Function for downloading player data from the current season via the FPL API
# ------------------------------------------------------------------------------
gen_fpl_player_data <- function(data_path = "..") {
  require(dplyr)
  require(purrr)
  require(jsonlite)
  require(tibble)
 
  # Define the FPL API url
  api_url <- "https://fantasy.premierleague.com/api/"
  
  # Download the `elements` dataset (this changes every week)
  elements <- fromJSON(paste0(api_url, "bootstrap-static/"))$elements
  
  # Create a clean dataset on player data from current season
  players <- elements %>% 
    # Minor claning operations
    mutate(
      # Merge name columns
      name = paste(first_name, second_name),
      # Make `status` column more user friendly
      status = case_when(
        status == "a" ~ "Available",
        status == "i" ~ "Injured",
        status == "u" ~ "Unavailable",
        status == "d" ~ "Doubtful",
        status == "n" ~ "On loan",
        status == "s" ~ "Suspended"
      ),
      # Download the `element-summary` for each player
      element_summary = map(id, function(x) {
        paste0(api_url, "element-summary/", x, "/") %>% 
          fromJSON()
      }),
      # Extract the `fixtures` dataset from `element_summary`
      fixtures = map(element_summary, function(x) x$fixtures),
      # Extract the `history` dataset from `element_summary`
      history = map(element_summary, function(x) {
        x$history %>% 
          # Minor cleaning operations
          mutate(
            # Enforce correct datetime format
            kickoff_time = as.POSIXct(
              kickoff_time,
              format = "%Y-%m-%dT%H:%M:%S",
              tz = "UTC"
            ),
            # Fix numerical variables read as strings
            influence = as.numeric(influence),
            creativity = as.numeric(creativity),
            threat = as.numeric(threat),
            ict_index = as.numeric(ict_index)
          ) 
      })
    ) %>% 
    # Drop variables we no longer need
    select(
      name, player_id = id, player_code = code,
      element_type, status, team_code, fixtures, history
    )
  
  # Save the dataset to disk
  file_path <- paste0(data_path, "/fpl/players.RDS")
  saveRDS(players, file_path)
}
