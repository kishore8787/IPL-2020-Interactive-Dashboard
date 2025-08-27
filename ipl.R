You said:
  library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)

# Load Data
ipl_matches <- read.csv("C:/Users/Pooja/Desktop/matches.csv")
ipl_deliveries <- read.csv("C:/Users/Pooja/Desktop/deliveries.csv")

# Filter for IPL 2020 (season = "2020/21")
season_2020_ids <- ipl_matches %>%
  filter(season == "2020/21") %>%
  pull(id)

ipl_deliveries_2020 <- ipl_deliveries %>%
  filter(match_id %in% season_2020_ids)

matches_2020 <- ipl_matches %>%
  filter(id %in% season_2020_ids)

# Team Colors
team_colors <- c(
  "Mumbai Indians" = "#004BA0",
  "Chennai Super Kings" = "#FCCA0D",
  "Royal Challengers Bangalore" = "#D50000",
  "Sunrisers Hyderabad" = "#FF6F00",
  "Delhi Capitals" = "#004C93",
  "Kolkata Knight Riders" = "#3B0067",
  "Kings XI Punjab" = "#D91F27",
  "Rajasthan Royals" = "#004BA0"
)

# UI
ui <- dashboardPage(
  skin = "purple",  # Changed from default blue to purple
  dashboardHeader(title = "IPL 2020 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Top Scorers", tabName = "topscorers", icon = icon("chart-bar")),
      menuItem("Top Bowlers", tabName = "topbowlers", icon = icon("chart-pie")),
      menuItem("Runs Over Time", tabName = "runsover", icon = icon("chart-line")),
      menuItem("Run Distribution", tabName = "rundist", icon = icon("chart-area")),
      menuItem("Player Search", tabName = "playersearch", icon = icon("search")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("home",
              fluidRow(
                valueBoxOutput("totalRuns"),
                valueBoxOutput("totalWickets"),
                valueBoxOutput("totalMatches"),
                valueBoxOutput("totalTeams")
              ),
              fluidRow(
                box(title = "Team Performance (Total Runs)",
                    width = 12,
                    plotlyOutput("teamPerformancePlot"))
              )
      ),
      tabItem("topscorers",
              h3("Top 10 Run Scorers"),
              plotlyOutput("topScorersPlot")
      ),
      tabItem("topbowlers",
              h3("Top 10 Wicket Takers"),
              plotlyOutput("topBowlersPlot")
      ),
      tabItem("runsover",
              h3("Runs by Over"),
              plotlyOutput("linePlot")
      ),
      tabItem("rundist",
              h3("Distribution of Runs per Ball"),
              plotlyOutput("histPlot")
      ),
      tabItem("playersearch",
              h3("Search Player Performance"),
              selectInput("playerInput", "Choose a player:",
                          choices = sort(unique(ipl_deliveries_2020$batter))),
              verbatimTextOutput("playerStats")
      ),
      tabItem("about",
              h3("About This App"),
              p("This dashboard shows IPL 2020 stats using R Shiny."),
              p("Built by Pooja Vijay Anand.")
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  output$totalRuns <- renderValueBox({
    total_runs <- sum(ipl_deliveries_2020$total_runs, na.rm = TRUE)
    valueBox(total_runs, "Total Runs", icon = icon("chart-line"), color = "green")
  })
  
  output$totalWickets <- renderValueBox({
    total_wickets <- sum(ipl_deliveries_2020$player_dismissed != "", na.rm = TRUE)
    valueBox(total_wickets, "Total Wickets", icon = icon("skull-crossbones"), color = "red")
  })
  
  output$totalMatches <- renderValueBox({
    valueBox(n_distinct(matches_2020$id), "Total Matches", icon = icon("calendar"), color = "blue")
  })
  
  output$totalTeams <- renderValueBox({
    teams <- unique(c(matches_2020$team1, matches_2020$team2))
    valueBox(length(unique(teams)), "Total Teams", icon = icon("users"), color = "yellow")
  })
  
  output$teamPerformancePlot <- renderPlotly({
    team_runs <- ipl_deliveries_2020 %>%
      group_by(batting_team) %>%
      summarise(total_runs = sum(total_runs, na.rm = TRUE)) %>%
      arrange(desc(total_runs))
    
    plot_ly(team_runs, 
            x = ~reorder(batting_team, total_runs), 
            y = ~total_runs,
            type = "bar",
            marker = list(color = team_colors[team_runs$batting_team])) %>%
      layout(title = "Total Runs by Team",
             xaxis = list(title = "Team"),
             yaxis = list(title = "Runs"),
             margin = list(b = 100))
  })
  
  output$topScorersPlot <- renderPlotly({
    top_batters <- ipl_deliveries_2020 %>%
      group_by(batter) %>%
      summarise(runs = sum(batsman_runs, na.rm = TRUE)) %>%
      arrange(desc(runs)) %>%
      slice_head(n = 10)
    
    plot_ly(top_batters, 
            x = ~reorder(batter, runs), 
            y = ~runs,
            type = "bar",
            marker = list(color = "orange")) %>%
      layout(title = "Top 10 Run Scorers",
             xaxis = list(title = "Player"),
             yaxis = list(title = "Runs"),
             margin = list(b = 100))
  })
  
  output$topBowlersPlot <- renderPlotly({
    top_bowlers <- ipl_deliveries_2020 %>%
      filter(player_dismissed != "") %>%
      group_by(bowler) %>%
      summarise(wickets = n()) %>%
      arrange(desc(wickets)) %>%
      slice_head(n = 10)
    
    plot_ly(top_bowlers,
            labels = ~bowler,
            values = ~wickets,
            type = "pie",
            textinfo = "label+percent",
            marker = list(colors = RColorBrewer::brewer.pal(10, "Purples"))) %>%
      layout(title = "Top 10 Wicket Takers (Pie Chart)")
  })
  
  output$linePlot <- renderPlotly({
    overwise_runs <- ipl_deliveries_2020 %>%
      group_by(match_id, over) %>%
      summarise(runs = sum(total_runs)) %>%
      group_by(over) %>%
      summarise(avg_runs = mean(runs))
    
    plot_ly(overwise_runs, 
            x = ~over, y = ~avg_runs, 
            type = 'scatter', mode = 'lines+markers',
            line = list(color = 'blue')) %>%
      layout(title = "Average Runs per Over",
             xaxis = list(title = "Over"),
             yaxis = list(title = "Avg Runs"))
  })
  
  output$histPlot <- renderPlotly({
    plot_ly(ipl_deliveries_2020, 
            x = ~total_runs, 
            type = "histogram",
            marker = list(color = "teal")) %>%
      layout(title = "Run Distribution per Ball",
             xaxis = list(title = "Runs per Ball"),
             yaxis = list(title = "Frequency"))
  })
  
  output$playerStats <- renderPrint({
    req(input$playerInput)
    stats <- ipl_deliveries_2020 %>% filter(batter == input$playerInput)
    cat("Player:", input$playerInput, "\n")
    cat("Total Runs:", sum(stats$batsman_runs), "\n")
    cat("Balls Faced:", nrow(stats), "\n")
    cat("Strike Rate:", round(sum(stats$batsman_runs) / nrow(stats) * 100, 2), "\n")
  })
}

# Run the App
shinyApp(ui, server)