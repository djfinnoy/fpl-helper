# This Shiny module contains a datatable for easy navigation of potential picks
# for upcoming gameweeks. It also visualises minutes played, and points
# obtained in previous fixtures.
# ==============================================================================

# UI function
# ==============================================================================
upcomingPlayerFixturesDatatableUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # Filters
    # --------------------------------------------------------------------------
    fluidRow(
      box(
        title = "Table options", 
        status = "primary", 
        width = 6,
        height = "17em",
        column(
          width = 6,
          selectInput(
            inputId = ns("team_selector"),
            label = "Team:",
            choices = list()  # Populated by the module's server function
          ),
          selectInput(
            inputId = ns("position_selector"),
            label = "Position:",
            choices = list("All", "GK", "DEF", "MID", "FWD")
          )  
        ),  # End column
        column(
          width = 6,
          selectInput(
            inputId = ns("nrounds_selector"),
            label = "Upcoming rounds considered:",
            choices = list(1, 2, 3),
            selected = 3
          ),
          sliderInput(
            inputId = ns("price_selector"),
            label = "Max price:",
            value = 12.5,  # Repopulated by the module's server function,
            min = 4,  # Repopulated by the module's server function,
            max = 12.5,  # Repopulated by the module's server function,
            step = 0.5
          )
        )  # End column
      ),  # End box
      box(
        title = "Table information",
        status = "primary",
        width = 6,
        height = "17em",
        withTags({
          div(
            class = "datatable-info",
            tags$b("Expected value:"),
            tags$br(),
            "Probability of goal/assist/clean sheet * Fantasy point value.",
            tags$br(), tags$br(),
            tags$b("EV / Price:"),
            tags$br(),
            "Expected value / Price.",
            tags$br(), tags$br(),
            tags$b("Rank:"),
            tags$br(),
            "A players rank, within his position, for Expected value and EV/P."
          )
        })
      )  # End box
    ),  # End fluidRow
    
    # Output
    # --------------------------------------------------------------------------   
    fluidRow(
      box(
        title = "Player overview for upcoming fixtures",
        status = "primary",
        width = 12,
        DT::DTOutput(
          outputId = ns("datatable")
        )
      )  # End box
    )  # End fluidRow
  )  # End tagList
}

# Server function
# ==============================================================================
upcomingPlayerFixturesDatatable <- function(input, output, session, data) {
  # Data used by function is eval_latest.json
  
  # Set initial filter values based on input dataset
  # ---------------------------------------------------------------------------- 
  updateSelectInput(
    session = session,
    inputId = "team_selector",
    choices = list("All", unique(data$team_name)) %>% purrr::flatten()
  )
  
  updateSliderInput(
    session = session,
    inputId = "price_selector",
    min = min(data$price) / 10,
    max = max(data$price) / 10,
    value = max(data$price) / 10
  )
  
  # Data table
  # ----------------------------------------------------------------------------
  
  # Prepare datatable contents based on input filters
  get_dt_data <- reactive({
    data %>% 
      mutate(price = price / 10) %>% 
      filter(price <= input$price_selector) %>% 
      { if (input$team_selector != "All")
          filter(., team_name == input$team_selector)
        else .
      } %>% 
      { if (input$position_selector != "All")
          filter(., position == input$position_selector) 
        else .
      } %>% 
      arrange(gameweek) %>% 
      mutate(gw_rank = data.table::rleid(gameweek)) %>% 
      filter(gw_rank <= input$nrounds_selector) %>% 
      select(-gw_rank) %>% 
      group_by(xseason_id) %>% 
      summarize(
        name = first(name),
        position = first(position),
        team = first(team_name),
        status = first(status),
        price = first(price),
        ev = sum(total_ev)
      ) %>% 
      ungroup() %>% 
      mutate(value = ev / (price / 10)) %>% 
      group_by(position) %>% 
      arrange(-ev) %>% 
      mutate(ev_rank = row_number()) %>% 
      arrange(-value) %>% 
      mutate(value_rank = row_number()) %>% 
      select(
        xseason_id, name, position, team, status, price, 
        ev, ev_rank, value, value_rank
      ) %>% 
      arrange(-ev)
  })
  
  # Create and render the datatable
  output$datatable <- DT::renderDT(
    DT::datatable(
      get_dt_data() %>% select(-xseason_id),
      options = list(
        columnDefs = list(list(className = "dt-center", targets = 1:6)),
        scrollX = "100%"
      ),
      rownames = F,
      colnames = c(
        "Name", "Position", "Team", "Status", "Price", 
        "Expected value", "Rank", "EV / Price", "Rank"
      ),
      caption = "Click players to populate the visualizations below."
    ) %>% DT::formatRound(c(5, 6, 8), c(1, 4, 4))
  )
  
  # Return rows relating to the players selected in the datatable from 
  # `eval_latest.json`
  get_selected_players_data <- function(x = input$datatable_rows_selected) {
    selected_players <- get_dt_data()$xseason_id[x]
    data %>% filter(xseason_id %in% selected_players)
  }
  
  # Output data on selected players for use in other modules
  return(
    reactive({ get_selected_players_data() })
  )
}