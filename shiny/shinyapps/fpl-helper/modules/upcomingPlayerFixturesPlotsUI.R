# This Shiny module contains interactive plots for players selected through
# clicking rows in the `upcomingPlayerFixturesDatatable`
# ==============================================================================

# UI function
# ==============================================================================
upcomingPlayerFixturesPlotsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # Output
    # --------------------------------------------------------------------------   
    fluidRow(
      box(
        title = "Minutes played / Fantasy points obtained",
        status = "primary",
        width = 12,
        column(
          width = 12,
          align = "center",
          radioButtons(
            inputId = ns("minutes_points_selector"),
            label = "Select y axis variable:",
            choices = list("Fantasy points" = "total_points", "Minutes played" = "minutes"),
            inline = T
          ),
          plotlyOutput(outputId = ns("minutes_points_plot"))
        )  # End column
      )  # End box
    ),  # End fluidRow
    fluidRow(
      box(
        title = "Goal / Assist / Clean sheet probabilities",
        status = "primary",
        width = 12,
        column(width = 4, plotlyOutput(outputId = ns("goal_prob_barplot"))),
        column(width = 4, plotlyOutput(outputId = ns("assist_prob_barplot"))),      
        column(width = 4, plotlyOutput(outputId = ns("cs_prob_barplot")))               
      )  # End box
    ),  # End fluidRow
    fluidRow(
      box(
        title = "Fixture difficulty",
        status = "primary",
        width = 12,
        plotlyOutput(outputId = ns("fixture_difficulty_plot"))
      )  # End box
    )  # End fluidRow
  )  # End tagList
}

# Server function
# ==============================================================================
upcomingPlayerFixturesPlots <- function(input, output, session, data1, data2, data3) {
  # data1 = player_history_latest.RDS
  # data2 = Subset of eval_latest.RDS selected via the datatable module
  # data3 = fixture_difficulty_latest.RDS
  
  # Minutes / Fantasy points plot
  # ---------------------------------------------------------------------------- 
  gen_minutes_points_plot <- function(data1, data2, yvar) {
    require(dplyr)
    require(ggplot2)
    require(plotly)
    require(RColorBrewer)
    require(ggthemes)
    
    plot_data <- data1 %>%
      # Keep players selected in datatable
      filter(xseason_id %in% unique(data2$xseason_id)) %>% 
      # Shorten player names for nicer plots
      mutate(
        short_name = map_chr(name, function(x) {
          split <- str_split(x, " ") %>% unlist %>% .[-1]
          paste(split, collapse = " ")
        })
      )
    
    p <- plot_data %>% 
      ggplot(
        aes_string(
          y = yvar, 
          x = "kickoff_time", 
          color = "short_name", 
          group = "short_name"
        )
      ) +
      labs(x = "", y = "") +
      theme_tufte(base_family = "sans-serif") +
      theme(
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(),
        legend.title = element_blank()
      )
    
    if(nrow(plot_data) > 0) {
      p <- p +
        geom_line() +
        suppressWarnings(
          geom_point(
            aes(
              text = paste(
                name,
                "\n\nGameweek:", gameweek,
                "\nSeason:", season,
                "\nTeam:", team_name,
                "\n\nFantasy points:", total_points,
                "\nMinutes played:", minutes,
                "\nOpponent team:", opponent_team
              )
            )
          ) 
        ) +
        scale_color_brewer(palette = "Set2")
    }
      
    ggplotly(p, tooltip = "text") %>% 
      layout(
        xaxis = list(fixedrange = T),  
        yaxis = list(fixedrange = T)
      ) %>% 
      config(displayModeBar = F) 
  }
  
  # Render the plot
  output$minutes_points_plot <- renderPlotly(
    gen_minutes_points_plot(data1, data2(), input$minutes_points_selector)
  )
  
  
  # Probability barplots
  # ----------------------------------------------------------------------------
  gen_prob_barchart <- function(data, var = c("goal_prob", "assist_prob", "cs_prob")) {
    require(dplyr)
    require(ggplot2)
    require(plotly)
    require(RColorBrewer)
    require(ggthemes)
    
    main_var <- case_when(
      var == "goal_prob" ~ "Goal probability: ",
      var == "assist_prob" ~ "Assist probability: ",
      var == "cs_prob" ~ "Clean sheet probability: "
    )
    
    p <- data %>% 
      mutate(prob := !!sym(var)) %>% 
      ggplot(aes(y = prob, x = gameweek, fill = name)) +
      labs(x = "Gameweek", y = "") +
      scale_x_discrete() +
      scale_y_continuous(
        limits = c(0, 0.7), breaks = seq(0, 0.7, 0.1), labels = scales::percent_format(accuracy = 1)
      ) +
      theme_tufte(base_family = "sans-serif") +
      theme(
        legend.position = "none",
        strip.text.x = element_blank()
      ) +
      scale_fill_brewer(palette = "Set2")
    
    if(nrow(data) > 0) {
      p <- p +
      suppressWarnings(
        geom_bar(
          aes(
            text = paste0(
              name, 
              "\n\nGameweek: ", gameweek, "\n",
              main_var, round(prob * 100, 1) %>% paste0(., "%"),
              "\n\nOpponent: ", opponent_team,
              "\nLocation: ", ifelse(home, "Home", "Away")
            )
          ),
          stat = "identity",
          position = position_dodge()
        )
      ) +
      facet_wrap(~name)
    } else {
      p <- p +
        theme(axis.text = element_blank())
    }
    
    ggplotly(p, tooltip = "text") %>% 
      layout(xaxis = list(fixedrange = T),  yaxis = list(fixedrange = T)) %>% 
      config(displayModeBar = F) 
  } 
  
  # Render the plots
  output$goal_prob_barplot <- renderPlotly(gen_prob_barchart(data2(), "goal_prob"))
  output$assist_prob_barplot <- renderPlotly(gen_prob_barchart(data2(), "assist_prob"))
  output$cs_prob_barplot <- renderPlotly(gen_prob_barchart(data2(), "cs_prob")) 
  

  # Fixture difficulty plot
  # ----------------------------------------------------------------------------
  gen_fixture_heatmap <- function(data) {
    require(dplyr)
    require(ggplot2)
    require(plotly)
    
    plot_data <- data %>%
      mutate(team_name = factor(team_name) %>% factor(., levels = rev(levels(.)))) %>% 
      complete(gameweek, team_name) %>% 
      arrange(team_name, gameweek) %>% 
      group_by(team_name, gameweek) %>% 
      summarize(
        double = n() > 1,
        opponent_team = paste(opponent_team, collapse = " & "),
        strength_string = paste(as.character(round(opponent_elo_strength, 0)), collapse = " & "),
        opponent_elo_strength = mean(opponent_elo_strength)
      ) 
  
    p <- plot_data %>%  
      ggplot(aes(
        x = gameweek, 
        y = team_name, 
        fill = opponent_elo_strength,
        text = paste(
          "Gameweek:", gameweek,
          "\n\nOpponent:", opponent_team,
          "\nStrength:", strength_string
        ))
      ) +
      geom_raster() +
      scale_x_continuous(limits = c(0, 39), breaks = c(1:38), expand = c(0,0)) +
      scale_fill_distiller(palette = "YlOrRd", direction = 1) +
      theme_tufte(base_family = "sans-serif") +
      labs(x = "Gameweek", y = "", fill = "Opponent strength")
  
  ggplotly(p, tooltip = "text") %>% 
    layout(
      xaxis = list(fixedrange = T),  
      yaxis = list(fixedrange = T)
    ) %>% 
    config(displayModeBar = F)
  }
  
  output$fixture_difficulty_plot <- renderPlotly(gen_fixture_heatmap(data3))
}