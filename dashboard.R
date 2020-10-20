### Dashboard Skeleton ###
library(shiny)
#install.packages('shinydashboard')
#install.packages('dashboardthemes')
library(shinydashboard)
library(dashboardthemes)
library(ggthemes)
library(plotly)


ui <- dashboardPage( 
  dashboardHeader(title = "Orange6"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Player Overview", tabName = "poverview", icon = icon("team")),
      menuItem("Outcomes and Errors", tabName = "outcomes_errors"),
      menuItem("Serving and Returning", tabName = 'serve_return'), 
      menuItem('Team Leaderboard', tabName = 'team_leaderboard')
    )
  ),
  dashboardBody(
    
    #shinyDashboardThemes(theme = 'flat_red'),
    shinyDashboardThemes(theme = 'grey_light'),
    
    
    tabItems(
      tabItem("poverview",
              fluidRow(
                box(selectInput("player", "Player:", unique(AllData$match$player)),
                     width = 4)
              ), 
              
              fluidRow(column(width = 4, DT::dataTableOutput('WinPctTable')), 
                       column(width = 8, 
                              fluidRow(infoBoxOutput('top1'), infoBoxOutput('top2'), infoBoxOutput('top3')),
                              fluidRow(infoBoxOutput('bottom1'), infoBoxOutput('bottom2'), infoBoxOutput('bottom3')))
                       ),
              
              
              fluidRow(
                box(h2('Stat Development Over Time'),width = 6),
                box(
                  selectInput("measureOverTime", "Measure:", AllData$matchMeasures, selected = 'Games Won per Match'), width = 6
                )),
              
              fluidRow(
                box(
                  plotlyOutput('statByMatch'), width = 6
                ), 
                box(
                  plotlyOutput('statByMonth'), width = 6
                )
              ),
              fluidRow(box(DT::dataTableOutput('playerStatTable'), width = 12))
              
      ),
      tabItem('outcomes_errors',
              fluidRow(box(h2("Outcomes and Errors"), width = 6), 
                       box(selectInput("time", "Time Period:", 
                                       c('Career', as.character(unique(AllData$match$year))), 
                                       selected = 'Career'), width = 6)),
              
              fluidRow(
                box(title = 'Frequency of Outcomes for Points Won',plotOutput('WinningOutcomes')),
                box(title = 'Frequency of Outcomes for Points Lost',plotOutput('LosingOutcomes'))
              ),
              
              fluidRow(
                box(h3('Outcome Breakdown'), width = 6),
                box(selectInput("outcomeSelect", "Outcome Type:", c(as.character(unique(CIZR$outcome))), selected = 'UnforcedError'), width = 6)
              ),
              
              # Shot type breakdown bar charts
              fluidRow(
                box(title = 'Shot Type Breakdown (Wins)', plotOutput('WinningShotBreakdown'), width=3),
                box(title = 'Error Type Breakdown (Wins)', plotOutput('WinningErrorBreakdown'), width=3),
                box(title = 'Shot Type Breakdown (Losses)', plotOutput('LosingShotBreakdown'), width=3),
                box(title = 'Error Type Breakdown (Losses)', plotOutput('LosingErrorBreakdown'), width=3)
              ),
              
              fluidRow(
                box(DT::dataTableOutput('ErrorTable') ,width = 4),
                box(plotOutput('ErrorShot') ,width = 4),
                box(plotOutput('ErrorType') ,width = 4)
              )
              ),
      tabItem('serve_return',
              fluidRow(column(width = 4, h2('Serving info table')),
                       column(width = 8), h2('Serving info boxes'))
      ), 
      
      tabItem('team_leaderboard', 
              fluidRow(box(h1('Team Leaderboard'))),
              fluidRow(box(DT::dataTableOutput('TeamLeaderboard'), width = 12))
      )
      
    )
    
  )
)


server <- function(input, output){
  key <- reactive({
    KeyStats(AllData$player, input$player)
  })
  
  output$WinPctTable <- DT::renderDataTable({wonPlayedTable(input$player)})
  
  
  output$top1 <-   renderInfoBox({infoBox('Strongest Stat', key()$top[1], color = 'red'    )})
  output$top2 <- renderInfoBox({infoBox('Second Strongest', key()$top[2], color = 'red'     )})
  output$top3 <-  renderInfoBox({infoBox('Third Strongest', key()$top[3], color = 'red'     )})
  output$bottom1 <- renderInfoBox({infoBox('Weakest Stat', key()$bottom[1], color = 'blue'  )})
  output$bottom2 <- renderInfoBox({infoBox('Second Weakest', key()$bottom[2], color = 'blue')})
  output$bottom3 <- renderInfoBox({infoBox('Third Weakest', key()$bottom[3], color = 'blue' )})
  
  output$playerStatTable <- DT::renderDataTable({playerStatTable(input$player)})
  
  output$statByMatch <- renderPlotly({playerStatByMonth(input$player, input$measureOverTime, F)})
  
  output$statByMonth <- renderPlotly({playerStatByMonth(input$player, input$measureOverTime, T)})
  
  output$WinningOutcomes <- renderPlot({
    CIZR %>% OutcomesBar(name = input$player, time=input$time)
  })
  
  output$LosingOutcomes <- renderPlot({
    CIZR %>% OutcomesBar(name = input$player, time=input$time, won = F)
  })
  
  
  # Shot Type Breakdown bar charts
  output$WinningShotBreakdown <- renderPlot({
    CIZR %>% BreakdownShot(name = input$player, time=input$time, outcomeParam=input$outcomeSelect)
  })
  
  output$LosingShotBreakdown <- renderPlot({
    CIZR %>% BreakdownShot(name = input$player, time=input$time, outcomeParam=input$outcomeSelect, won = F)
  })
  
  
  # Error Type Breakdown bar charts
  output$WinningErrorBreakdown <- renderPlot({
    CIZR %>% BreakdownError(name = input$player, time=input$time, outcomeParam=input$outcomeSelect)
  })
  
  output$LosingErrorBreakdown <- renderPlot({
    CIZR %>% BreakdownError(name = input$player, time=input$time, outcomeParam=input$outcomeSelect, won = F)
  })
  
  output$ErrorTable <- DT::renderDataTable({ErrorTable(CIZR, name = input$player)})
  
  output$ErrorShot <- renderPlot({
    ErrorShot(CIZR, input$player)
  })
  
  output$ErrorType <- renderPlot({
    ErrorType(CIZR, input$player)
  })
  
  output$TeamLeaderboard <- DT::renderDataTable({teamLeaderboard()})
  
}

shinyApp(ui, server)

