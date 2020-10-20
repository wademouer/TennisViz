### Dashboard Skeleton ###
library(shiny)
#install.packages('shinydashboard')
#install.packages('dashboardthemes')
#install.packages('plotly')
library(tidyverse)
library(shinydashboard)
library(dashboardthemes)
library(ggthemes)
library(plotly)


ui <- dashboardPage( 
  dashboardHeader(title = "Player Data "),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Player", tabName = "player", icon = icon("team"), 
               menuSubItem("Overview", tabName = "overview", icon = icon("team")),
               menuSubItem("Serving Data", tabName = "serving", icon = icon("team")),
               menuSubItem("Returning Data", tabName = "returning", icon = icon("team")),
               menuSubItem("Rally Data", tabName = "rally", icon = icon("team"))), 
      menuItem("Team", tabName = "overview", icon = icon("team"), 
               menuSubItem("Overview", tabName = "teamOverview", icon = icon("team")),
               menuSubItem("Leaderboard", tabName = "teamLeaderboard", icon = icon("team")))
      
    )
  ),
  dashboardBody(
    shinyDashboardThemes(theme = 'flat_red'),
    #customTheme,
    tabItems(
      tabItem("overview",
              fluidRow(
                box(h1('Stats Overview'),selectInput("player", "Player:", unique(AllData$match$player)), width = 4)
                ),
              
              fluidRow(
                infoBoxOutput('MatchesBox'),
                infoBoxOutput('GamesBox'),
                infoBoxOutput('PointsBox')
              ),
              
              # fluidRow(
              #   column(width = 6,
              #          box(width = NULL,
              #              #plotlyOutput('MatchWins')
              #              plotOutput('WinningOutcomes')
              #          ),
              #          box(width = NULL,
              #              plotOutput('LosingOutcomes'))),
              #   column(width = 6,
              #          infoBoxOutput('MatchesBox'),
              #          infoBoxOutput('GamesBox'),
              #          infoBoxOutput('PointsBox'),
              #          valueBox(12,subtitle = 'Box'))
              # ),
              
              fluidRow(
                box(h2('Win Rates over time'),width = 4),
                box(
                selectInput("wonbytotal", "Stat:", c('Points', 'Deuces' ,'Games', 'Sets')), width = 4
              )),
              
              fluidRow(
                box(
                  plotlyOutput('WonByTotal'), width = 6
                ), 
                box(
                  plotlyOutput('WinRate'), width = 6
                )
              ),
              fluidRow(
                box(h2('Outcomes'),
                    selectInput("time", "Time Period:", c('Career', as.character(unique(AllData$match$year))), selected = 'Career'), width = 4)
                # box(
                #   plotlyOutput('MatchWins'), width = 8
                # ),
                
              ),
              
              fluidRow(
                box(title = 'Frequency of Outcomes for Points Won',plotOutput('WinningOutcomes')),
                box(title = 'Frequency of Outcomes for Points Lost',plotOutput('LosingOutcomes')),
                box(title = 'Unforced Error by Shot %',plotOutput('ErrorShot')),
                box(title = 'Unforced Error by Type %',plotOutput('ErrorType'))
                )
              # fluidRow(
              #   valueBoxOutput('MatchWinsBox'),
              #   valueBoxOutput('TotalMatchesBox'),
              #   valueBoxOutput('MatchLossesBox')
              # ),
              # box(
              #   width = 12,
              #   column(width = 6, plotlyOutput('MatchWins')),
              #   column(width = 6,
              #          valueBoxOutput('MatchWinsBox'),
              #          valueBoxOutput('TotalMatchesBox'),
              #          valueBoxOutput('MatchLossesBox'))
              # )
              
              
              
      ), 
      tabItem('player', 
              h1('This is the player page'))
    )
  )
)


server <- function(input, output){
  output$MatchWins <- renderPlotly({
    ggplotly(
      AllData$match %>% 
        filter(player == input$player) %>% 
        yearFilter(input$time) %>% 
        mutate(MatchOutcome = case_when(SetsWon == 2~ 'Win', 
                                        SetsWon != 2 ~ 'Loss',
                                        TRUE ~'Other')) %>% 
        ggplot(aes(x = date, y = TotalGames, color = MatchOutcome, size = PointsWon)) +
        geom_point() + 
        scale_color_manual(values = pal)+
        theme_minimal()
    )
  })
  
  output$WonByTotal <- renderPlotly({
    AllData$match %>% filter(player == input$player) %>% WonByTotalPlotly(input$wonbytotal)
  })
  
  output$WinRate <- renderPlotly({
    AllData$match %>% filter(player == input$player) %>% WinRatePlotly(input$wonbytotal)
  })
  
  output$WinningOutcomes <- renderPlot({
    CIZR %>% OutcomesBar(name = input$player, time=input$time)
  })
  
  output$LosingOutcomes <- renderPlot({
    CIZR %>% OutcomesBar(name = input$player, time=input$time, won = F)
  })
  
    output$ErrorShot <- renderPlot({
   ErrorShot(AllData, input$player)
  })
  
  output$ErrorType <- renderPlot({
    ErrorType(AllData, input$player)
  })
  
  output$MatchesBox <- renderInfoBox({
    infoBox(
      'Matches Won',
      AllData$match %>% 
        filter(player == input$player) %>% 
        yearFilter('Career') %>% 
        wonPlayed('Matches'),
      #icon = icon('trophy', lib = 'glyphicon'),
      fill = TRUE,
      color = 'blue'
    )
  })
  
  output$GamesBox <- renderInfoBox({
    infoBox(
      'Games Won',
      AllData$match %>% 
        filter(player == input$player) %>% 
        yearFilter('Career') %>% 
        wonPlayed('Games'),
      #icon = icon('tennis ball', lib = 'glyphicon'),
      fill = TRUE,
      color = 'purple'
    )
  })
  
  output$PointsBox <- renderInfoBox({
    infoBox(
      'Points Won',
      AllData$match %>% 
        filter(player == input$player) %>% 
        yearFilter('Career') %>% 
        wonPlayed('Points'),
      #icon = icon('thumbs-down', lib = 'glyphicon'),
      fill = TRUE,
      color = 'red'
    )
  })
}

shinyApp(ui, server)
