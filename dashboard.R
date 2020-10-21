### Dashboard Skeleton ###
library(shiny)
#install.packages('shinydashboard')
#install.packages('dashboardthemes')
library(shinydashboard)
library(dashboardthemes)
library(ggthemes)
library(plotly)



#####---------------------- BEGIN DASHBOARD THEME ------------------------ #####

custom_red <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(42,102,98)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(255,255,254)"
  
  ### header
  ,logoBackColor = "rgb(45,59,66)"
  
  ,headerButtonBackColor = "rgb(45,59,66)"
  ,headerButtonIconColor = "rgb(255,255,255)"
  ,headerButtonBackColorHover = "rgb(45,59,66)"
  ,headerButtonIconColorHover = "rgb(207,57,92)"
  
  ,headerBackColor = "rgb(45,59,66)"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  ,sidebarBackColor = "rgb(225,85,85)"
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(255,255,255)"
  ,sidebarSearchIconColor = "rgb(207,57,92)"
  ,sidebarSearchBorderColor = "rgb(255,255,255)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 14
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = 0
  
  ,sidebarTabBackColorSelected = "rgb(45,59,66)"
  ,sidebarTabTextColorSelected = "rgb(255,255,255)"
  ,sidebarTabRadiusSelected = "0px"
  
  ,sidebarTabBackColorHover = "rgb(168,42,42)"
  ,sidebarTabTextColorHover = "rgb(255,255,255)"
  ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "none"
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "0px"
  
  ### boxes
  ,boxBackColor = "rgb(248,248,248)"
  ,boxBorderRadius = 0
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = ""
  ,boxTitleSize = 18
  ,boxDefaultColor = "rgb(248,248,248)"
  ,boxPrimaryColor = "rgb(15,124,191)"
  ,boxInfoColor = "rgb(225,225,225)"
  ,boxSuccessColor = "rgb(59,133,95)"
  ,boxWarningColor = "rgb(178,83,149)"
  ,boxDangerColor = "rgb(207,57,92)"
  
  ,tabBoxTabColor = "rgb(248,248,248)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(42,102,98)"
  ,tabBoxTabTextColorSelected = "rgb(207,57,92)"
  ,tabBoxBackColor = "rgb(248,248,248)"
  ,tabBoxHighlightColor = "rgb(207,57,92)"
  ,tabBoxBorderRadius = 0
  
  ### inputs
  ,buttonBackColor = "rgb(207,57,92)"
  ,buttonTextColor = "rgb(255,255,255)"
  ,buttonBorderColor = "rgb(207,57,92)"
  ,buttonBorderRadius = 0
  
  ,buttonBackColorHover = "rgb(186,51,83)"
  ,buttonTextColorHover = "rgb(255,255,255)"
  ,buttonBorderColorHover = "rgb(186,51,83)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(118,118,118)"
  ,textboxBorderRadius = 0
  ,textboxBackColorSelect = "rgb(255,255,255)"
  ,textboxBorderColorSelect = "rgb(118,118,118)"
  
  ### tables
  ,tableBackColor = "rgb(248,248,248)"
  ,tableBorderColor = "rgb(235,235,235)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)



##### ------------------------- END DASHBOARD THEME ------------------------- #####



ui <- dashboardPage( 
  dashboardHeader(title = "Orange6"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Player Overview", tabName = "poverview", icon = icon("team")),
      menuItem("Outcomes and Errors", tabName = "outcomes_errors"),
      menuItem("Serving", tabName = 'serve_return'), 
      menuItem('Team Leaderboard', tabName = 'team_leaderboard')
    ),
    
    # --- Leaderboard Checkbox Styling --- #
    tags$head(
      tags$style(
        HTML("
               .multicol { 
                 # height: 150px;
                 -webkit-column-count: 5; /* Chrome, Safari, Opera */ 
                 -moz-column-count: 5;    /* Firefox */ 
                 column-count: 5; 
                 -moz-column-fill: auto;
                 -column-fill: auto;
               } 
               div.checkbox > label {
                 padding-left: 55px;
               }
               ")) 
    )
    # - End Leaderboard Checkbox Styling-- #
  ),
  dashboardBody(
    custom_red,
    #shinyDashboardThemes(theme = 'flat_red'),
    #shinyDashboardThemes(theme = 'grey_light'),
    
    
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
              fluidRow(box(h3('Error Breakdown'), width = 12)),
              
              fluidRow(
                box(title = 'Career Error Types',DT::dataTableOutput('ErrorTable') ,width = 4),
                box(title = 'Shot Types for Career Unforced Errors' , plotOutput('ErrorShot') ,width = 4),
                box(title = 'Error Types for Career Unforced Errors', plotOutput('ErrorType') ,width = 4)
              )
              ),
      tabItem('serve_return',
              fluidRow(box(h2('Serving Data'))),
              fluidRow(column(width = 4, DT::dataTableOutput('ServeTable')),
                       column(width = 8, fluidRow(valueBoxOutput('AccuracyBox'), valueBoxOutput('AcesBox'), valueBoxOutput('DoubleFaultBox')))),
              fluidRow(box(title = 'Serving Outcomes', d3treeOutput('ServingTree', height =  '700px'), width = 12))
      ), 
      
      tabItem('team_leaderboard', 
              fluidRow(box(h1('Team Leaderboard'))),
              tags$div(align = 'left', class = 'multicol',
                       fluidRow(checkboxGroupInput("show_vars", "Columns to show:",
                                                   AllData$matchMeasures,selected=AllData$matchMeasures[1:8],
                                                   width='80%'), width = 9)),  # , inline=TRUE, width='100%'
              fluidRow(box(DT::dataTableOutput('TeamLeaderboard'), width = 12)),

              # fluidRow(
              #   column(width = 9,box(DT::dataTableOutput('TeamLeaderboard'), width = NULL) ),
              #   column(width = 3, checkboxGroupInput("show_vars", "Columns to show:",
              #                                        c('TotalMatches' ,AllData$matchMeasures),selected=c('TotalMatches',AllData$matchMeasures[1:8]),
              #                                        width='80%')))
                
              
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
  
  # This is the output of the team leaderboard that connects with the checkboxes
  teamLeaderboard <- teamLeaderboard()
  
  output$TeamLeaderboard <- DT::renderDataTable({
    DT::datatable(teamLeaderboard[, input$show_vars, drop = FALSE],
                  options = list(dom = 't',scrollX = TRUE, pageLength = 20)) %>% formatPercentage(intersect(AllData$percentageNameStrings, input$show_vars), digits = 0)
    
    
  })
  
  output$ServeTable <- DT::renderDataTable({ServeTable(CIZR, input$player)})
  
  output$AccuracyBox <-   renderValueBox({valueBox(subtitle = 'First Serve Accuracy', 
                                                 scales::percent(AllData$player %>% filter(player == input$player) %>% 
                                                                   summarise(val = FirstServeInCount / FirstServeCount) %>% pull(val)) %>% as.character() , 
                                                 color = 'green'    )})
  output$AcesBox <-   renderValueBox({valueBox(subtitle = 'Aces per Match', 
                                             round(digits = 2, 
                                                   x = AllData$player %>% filter(player == input$player) %>% 
                                                     summarise(val = Aces / TotalMatches) %>% pull(val)) %>% as.character(),
                                             color = 'aqua'    )})
  output$DoubleFaultBox <-   renderValueBox({valueBox(subtitle = 'Double Faults per Match', 
                                                    round(digits = 2, 
                                                          x = AllData$player %>% filter(player == input$player) %>% 
                                                          summarise(val = DoubleFault / TotalMatches) %>% pull(val)) %>% as.character(), 
                                                    color = 'maroon'    )})
  output$ServingTree <- renderD3tree({TreeMap(CIZR, input$player)})
  
}

shinyApp(ui, server)

