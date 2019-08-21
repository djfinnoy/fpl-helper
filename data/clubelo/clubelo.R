# Function for downloading team strength data from clubelo.com
# ------------------------------------------------------------------------------
gen_team_elo_data <- function(
  data_path = "..",
  from = "2016-08-01",
  to = lubridate::today()
) {
  require(dplyr)
  require(purrr)
  require(readr)
  require(stringr)
  require(lubridate)
  
  # URL for accessing data at www.clubelo.com
  elo_url <- "http://api.clubelo.com/"
  
  # Load data on all teams that have participated in PL since the 2016 season
  team_ids <- readRDS(paste0(data_path, "/vaastav/team_ids.RDS"))
  
  # Create a team strength overview (over time)
  team_elo_data <- team_ids %>% 
    select(team) %>% 
    distinct() %>% 
    mutate(
      # Convert from FPL to clubelo.com naming convention
      elo_name = case_when(
        team == "West Bromwich Albion" ~ "WestBrom",
        team == "Man Utd" ~ "ManUnited",
        team == "Spurs" ~ "Tottenham",
        team == "Stoke City" ~ "Stoke",
        team == "Swansea City" ~ "Swansea",
        team == "Sheffield Utd" ~ "SheffieldUnited",
        team %>% str_detect(" ") ~ str_replace(team, " ", ""),
        TRUE ~ team            
      ),
      # Download data from clubelo.com
      data = map(elo_name, function(x) {
        paste0(elo_url, x) %>% 
          read_csv(., col_types = cols()) %>% 
          filter(From %within% interval(ymd(from), ymd(to))) %>% 
          select(elo = Elo, from = From, to = To)       
      })
    ) %>% 
    # Convert from nested to long format
    select(team, data) %>% 
    unnest() %>% 
    # Order rows
    arrange(team, from, to)
  
  # Save dataset to disk
  file_path <- paste0(data_path, "/clubelo/team_elo_data.RDS")
  saveRDS(team_elo_data, file_path)
}