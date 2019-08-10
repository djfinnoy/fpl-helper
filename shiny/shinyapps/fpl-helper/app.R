# This shiny app presents the user with predictions, expected values, and
# visualizations. Predictions and expected values cover the three
# upcoming gameweeks. Historical data is provided by `vaastav`.
# ==============================================================================

# Load dependencies
# ==============================================================================
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(ggthemes)
library(RColorBrewer)


# Load data and shiny modules
# ==============================================================================
eval_latest <- readRDS("./data/eval_latest.RDS")
player_history_latest <- readRDS("./data/player_history_latest.RDS")
fixture_difficulty_latest <- readRDS("./data/fixture_difficulty_latest.RDS")
source("./modules/upcomingPlayerFixturesDatatableUI.R")
source("./modules/upcomingPlayerFixturesPlotsUI.R")


# Define the app's UI
# ==============================================================================
ui <- dashboardPage(
  title = "fpl-helper",
  
  
  dashboardHeader(
    title = tags$img(src = "./fpl-logo.png", height = "40px"),
    titleWidth = 75
  ),
  
  
  dashboardSidebar(
    collapsed = T,
    sidebarMenu(
      menuItem(
        text = "Upcoming player fixtures",
        tabName = "upcomingPlayerFixturesTab",
        icon = icon("dashboard")
      )  # End menuItem
    )  # End sidebarMenu
  ),  # End dashboardSidebar
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    shinyWidgets::setSliderColor("#340C39", 1),
    tabItems(
      tabItem(
        tabName = "upcomingPlayerFixturesTab",
        upcomingPlayerFixturesDatatableUI(id = "upcomingPlayerFixturesDatatable"),
        upcomingPlayerFixturesPlotsUI(id = "upcomingPlayerFixturesPlots")
      )  # End tabItem
    )  # End tabItems
  )  # End dashboardBody
)  # End dashboardPage


# Define the app's server function
# ==============================================================================
server <- function(input, output, session) {
  
  # Upcoming player fixtures
  dt_output <- callModule(
    module = upcomingPlayerFixturesDatatable,
    id = "upcomingPlayerFixturesDatatable",
    data = eval_latest
  )
  
  callModule(
    module = upcomingPlayerFixturesPlots,
    id = "upcomingPlayerFixturesPlots",
    data1 = player_history_latest,
    data2 = dt_output,
    data3 = fixture_difficulty_latest
  )
  
}  # End server


# Combine UI and server to form a functioning app
# ==============================================================================
shinyApp(ui, server)