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
    select(name) %>% 
    distinct() %>% 
    mutate(
      # Convert from FPL to clubelo.com naming convention
      elo_name = case_when(
        name == "West Bromwich Albion" ~ "WestBrom",
        name == "Man Utd" ~ "ManUnited",
        name == "Spurs" ~ "Tottenham",
        name == "Stoke City" ~ "Stoke",
        name == "Swansea City" ~ "Swansea",
        name == "Sheffield Utd" ~ "SheffieldUnited",
        name %>% str_detect(" ") ~ str_replace(name, " ", ""),
        TRUE ~ name            
      ),
      # Download data from clubelo.com
      data = map(elo_name, function(x) {
        paste0(elo_url, x) %>% 
          read_csv(., col_types = cols()) %>% 
          mutate(team_name = x) %>% 
          filter(From %within% interval(ymd(from), ymd(to))) %>% 
          select(team_name, elo = Elo, from = From, to = To)       
      })
    ) %>% 
  # Convert from nested to long format
  pull(data) %>% 
  map_dfr(., ~bind_rows(.x)) %>% 
  # Order rows
  arrange(team_name, from, to)
  
  # Save dataset to disk
  file_path <- paste0(data_path, "/clubelo/team_elo_data.RDS")
  saveRDS(team_elo_data, file_path)
}