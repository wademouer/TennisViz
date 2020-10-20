### Filtering and building plots###
#install.packages('treemap')
#devtools::install_github("d3treeR/d3treeR")
library(tidyverse)
library(plotly)
library(treemap)
library(d3treeR)
library(DT)


CIZR <- data.table::fread('~/MSA 21/AA 502/Visualization/CIZR CSV 2018-2020.csv')
CIZR <- CIZR %>% 
  mutate(date = as.Date(date, '%m/%d/%y'),
         player = case_when(str_detect(player, 'Rebol') ~ 'Amanda Rebol',
                            str_detect(player, 'Moldovan') ~ 'Bianca Moldovan',
                            str_detect(player, 'Bridges') ~ 'Taylor Bridges',
                            str_detect(player, 'Reami') ~ 'Adriana Reami' ,
                            str_detect(player,'Wiktorin') ~ 'Claudia Wiktorin' ,
                            TRUE ~ player)
  ) 


PlayerFilter <- function(data, filtered = T){
  data %>% 
    MatchFilter(filtered) %>% 
    group_by(player) %>% summarise(TotalMatches = n(),
                                       MatchesWon = sum(matchWon),
                                       across(where(is.numeric), sum))
}

#Will change
MatchFilter <- function(data, filtered = T) {
  data %>% 
    SetFilter(filtered) %>% 
    group_by(matchId, matchName, player, date) %>% 
    summarise(matchWon = last(setWon),
              TotalSets = n(),
              SetsWon = sum(setWon),
              across(where(is.numeric), sum) ) %>% 
    arrange(date) %>% 
    mutate(year = as.factor(format(date, format = '%Y')), month = as.factor(format(date, format = "%Y-%b")))
}

#Will change
SetFilter <- function(data, filtered = T){
  if(!filtered)
    data <- data %>% GameFilter()
  
  data %>% 
    group_by(matchId, matchName, setId, player, date) %>% 
    summarise(setWon = last(gameWon), 
              TotalGames = n(),
              GamesWon = sum(gameWon),
              across(where(is.numeric), sum))%>% 
    arrange(date) %>% 
    mutate(year = as.factor(format(date, format = '%Y')), month = as.factor(format(date, format = "%Y-%B")))
}

#will change slightly
GameFilter <- function(data){
  data %>% 
    group_by(matchId, matchName, setId,gameId, player, date) %>% 
    summarise(gameWon = last(!pointWonBy),
              PointsWon = sum(!pointWonBy),
              oppPointsWon = sum(pointWonBy),
              TotalPoints = PointsWon + oppPointsWon,
              Winners = sum(!pointWonBy & outcome == 'Winner'),
              ErrorsForced = sum(!pointWonBy & outcome == 'ForcedError'), 
              UnforcedErrors = sum(pointWonBy & outcome == 'UnforcedError'),
              BreakPoints =  sum(server & returnerScore == 40),
              BreakPointsWon = sum(server & returnerScore == 40 & !pointWonBy),
              Aces = sum(!pointWonBy & outcome == 'Ace'),
              FirstServeCount = sum(!server),
              FirstServeInCount = sum(!server & firstServeIn),
              FirstServeWonCount = sum(!server & firstServeIn & !pointWonBy),
              #FirstServeInPct = FirstServeInCount/FirstServeCount,
              #FirstServeWonPct = FirstServeWonCount/FirstServeInCount, 
              SecondServeCount = sum(!server & !firstServeIn), 
              SecondServeInCount = sum(!server & !firstServeIn & !(outcome %in% c('Fault', 'Let'))),
              SecondServeWonCount = sum(!server & !firstServeIn & !pointWonBy),
              #SecondServeInPct = SecondServeInCount/SecondServeCount,
              #SecondServeWonPct = SecondServeWonCount/SecondServeCount, 
              DoubleFault = sum(!server & !firstServeIn & outcome %in% c('Fault', 'Let')),
              FirstReturnerCount = sum(server & firstServeIn),
              FirstReturnedInCount = sum(server & firstServeIn & returnInPlay),
              #FirstReturnPct = FirstReturnedInCount/FirstReturnerCount,
              SecondReturnerCount = sum(server & !firstServeIn),
              SecondReturnedInCount = sum(server & !firstServeIn & returnInPlay),
              #SecondReturnPct = SecondReturnedInCount/SecondReturnerCount,
              ShortRallies = sum(rallyLength < 5),
              ShortRalliesWon = sum(rallyLength < 5 & !pointWonBy),
              #ShortRallyWinPct = ShortRalliesWon/ShortRallies, 
              MedRallies = sum(rallyLength > 4 & rallyLength < 9),
              MedRalliesWon = sum(rallyLength > 4 & rallyLength < 9 & !pointWonBy),
              #MedRallyWinPct = MedRalliesWon/MedRallies,
              LongRallies = sum(rallyLength > 8 ),
              LongRalliesWon = sum(rallyLength > 8 & !pointWonBy),
              #LongRallyWinPct = LongRalliesWon/ LongRallies,
              ServiceGames = sum(!pointsWon & !oppPointsWon & !server),
              ServiceGamesWon = sum(!pointsWon & !oppPointsWon & !server & !gameWonBy)
    )  %>% 
    arrange(date) %>% 
    mutate(year = as.factor(format(date, format = '%Y')), month = as.factor(format(date, format = "%Y-%B")))
}


AllData = list(game = GameFilter(CIZR))
AllData$set = SetFilter(AllData$game)
AllData$match = MatchFilter(AllData$game)
AllData$player = PlayerFilter(AllData$game)
AllData$matchMeasures = c("Match Win Percent"   ,"Sets Won per Match" , "Games Won per Match" ,"Points Won per Match"  ,"Winners"  ,"ErrorsForced" , "UnforcedErrors" ,
                          "Aces" ,"DoubleFault"  ,"Break Point Win Percent" , "First Serve In Percent" ,
                          "First Serve Won Percent", "Second Serve In Percent" , "Second Serve Won Percent",  "First Serve Returned Percent" ,
                          "Second Serve Returned Percent", "Short Rally Win Percent" , "Med Rally Win Percent"   ,  "Long Rally Win Percent" )


#Creates a filter to use with the input values for time period - either filters a specific year or returns original data
yearFilter <- function(data, time){
  if(time == 'Career'){
    return(data)}
  else{ return(filter(data, year == time))}
}

#Won by Played
#Gives a string response of '#Won / #Played: percentage': i.e. '3/4: 75%' 
#allows you to pass variable name as a string
wonPlayed <- function(data, var) {
  if (var == 'Matches') {
    ret <- data %>%
      ungroup() %>%
      summarise(
        totalMatches = n(),
        matchWins = sum(SetsWon == 2),
        matchLosses = totalMatches - matchWins
      )
    return(str_c(
      ret$matchWins,
      '/',
      ret$totalMatches,
      ': ',
      round(100 * ret$matchWins / ret$totalMatches), '%'
    ))
  }
  
  sym_w <- as.symbol(str_c(var, 'Won'))
  sym_t <- as.symbol(str_c('Total', var))
  
  ret <- data %>%
    ungroup() %>%
    summarise(total = sum(!!sym_t),
              won = sum(!!sym_w))
  
  return(str_c(ret$won, '/', ret$total, ': ', round(100 * ret$won/ret$total), '%'))
}

wonPlayedTable <- function(name){
  data <- AllData$match %>% ungroup %>% filter(player == name) %>% 
    select(matchWon, TotalSets, SetsWon, TotalGames, GamesWon, TotalPoints, PointsWon) %>% 
    summarise(TotalMatches = n(), MatchesWon = sum(matchWon),
              across(where(is.numeric), sum))
  
  table = data.frame('Category' = c('Matches', 'Sets', 'Games', 'Points'),
                     'Total' = t(data[1,c(1,3,5,7)]),
                     'Won' = t(data[1,c(2,4,6,8)]))
  table$Percentage = table$Won / table$Total
  
  table <- table %>% 
    mutate(Percentage = scales::percent(Percentage))%>%
    remove_rownames() %>% column_to_rownames(var = 'Category') 
  datatable(table, options = list(dom = 't'))
}

ServeTable <- function(data, name){
  data <- data %>% filter(player == name & !server) %>% 
    select(firstServeIn, pointWonBy, outcome)
  
  fCount <- count(data)$n
  fIn <- sum(data$firstServeIn)
  fWon <- sum(!data[data$firstServeIn]$pointWonBy)
  sCount <- fCount - fIn
  sIn <- data %>% filter(!firstServeIn & !(outcome %in% c('Fault', 'Let'))) %>% count() %>% pull(n)
  sWon <- data %>% filter(!firstServeIn & !pointWonBy) %>% count() %>% pull(n)
  
  tb = data.frame('Serve' = c('First Serve', 'Second Serve'), 
                  'Count' = c(fCount, sCount), 'In' = c(fIn, sIn))
  tb$`In Percentage` = round(100 * tb$In / tb$Count)
  tb$Won = c(fWon, sWon)
  tb$`Won Percentage` = round(100*tb$Won / tb$In)
  
  tb <- tb %>% 
    mutate(`In Percentage` = str_c(`In Percentage`, '%'),
           `Won Percentage` = str_c(`Won Percentage`, '%'))%>%
    remove_rownames() %>% column_to_rownames(var = 'Serve') 
  datatable(tb, options = list(dom = 't'))
} 

#Creates a plotly bar chart of won by total over the months of a players career
WonByTotalPlotly <- function(data, var){
  sym_w <- as.symbol(str_c(var,'Won'))
  sym_t <- as.symbol(str_c('Total',var))
  
  ggplotly(
    data %>%
      group_by(month) %>%
      summarise(TotalWon = sum(!!sym_w), Total = sum(!!sym_t)) %>%
      ggplot(aes(x = month)) +
      geom_col(aes(y = Total),
               fill = '#A9A9A9',
               alpha = 1,
               position = 'dodge') +
      geom_col(aes(y = TotalWon), fill = '#D45555',  alpha = 1) +
      labs(title = str_c(var, ' Won Over Time'), 
           y = var )+
      theme_minimal()
  )
}

#creates a plotly line chart of win rate over the players career
WinRatePlotly <- function(data, var){
  sym_w <- as.symbol(str_c(var,'Won'))
  sym_t <- as.symbol(str_c('Total',var))
  
  ggplotly(
    data %>%
      group_by(month) %>%
      summarise(TotalWon = sum(!!sym_w), Total = sum(!!sym_t), Rate = TotalWon/Total) %>%
      ggplot(aes(x = month,y = Rate, group = 1)) +
      geom_point(color = '#D45555') + 
      stat_summary(fun=sum, geom="line", color = '#D45555') +
      scale_y_continuous(labels = scales::percent, limits = c(0,1))+
      labs(title = str_c(var, ' Win Rate Over Time'), 
           y = str_c(var, ' Win Rate') )+
      theme_minimal()
  )
  
}

#This creaes a bar chart of the outcome of points over a give time period, compared to the teams average
#filter by whether the point was won or lost by the nscu player
OutcomesBar <- function(data, time = 'Career', name, won = T){
  data <- data %>% filter(case_when(won ~ !pointWonBy,
                                    !won ~ pointWonBy == 1))
  
  if(time != 'Career'){
    data <- data %>% filter(format(date, format = '%Y') == time)
  }
  
  dataP <- data %>% filter(player == name)
  
  getFreq <- function(df, name){
    df %>% 
      #group_by(Error, type, shotType) %>% 
      group_by(outcome) %>% 
      summarize(n = n()) %>% 
      ungroup() %>% 
      mutate(outcome = factor(outcome, levels = outcome[order(n)]),
             freq = n / sum(n),
             filter = name)
  }
  

  data %>% getFreq('Team Average') %>% rbind(getFreq(dataP, name)) %>%
    ggplot(aes(x = outcome, y = freq, fill = filter)) +
    geom_col(position = 'dodge') +
    scale_y_continuous(labels = scales::percent)+
    labs(#title = 'Outcome Frequencies for Points Won',
         y = 'Frequency',
         x = 'Outcome',
         fill = '') +
    scale_fill_manual(values = pal)  + theme_minimal(base_size = 16)

}

# This creates a bar chart breaking down the Shot Types for the outcome from the OutcomesBar function above it
BreakdownShot <- function(data, time = 'Career', name, outcomeParam = 'UnforcedError', won = T){
  data <- data %>% filter(case_when(won ~ !pointWonBy,
                                    !won ~ pointWonBy == 1)) %>%
    filter(outcome == outcomeParam)
  
  if(time != 'Career'){
    data <- data %>% filter(format(date, format = '%Y') == time)
  }
  
  dataP <- data %>% filter(player == name)
  
  getFreq <- function(df, name){
    df %>% group_by(shotType) %>% 
      summarize(n = n()) %>% 
      #ungroup() %>% 
      mutate(shotType = factor(shotType, levels = shotType[order(n)]),
             freq = n / sum(n),
             filter = name)
  }
  
  data %>% getFreq('Team Average') %>% rbind(getFreq(dataP, name)) %>% 
    ggplot(aes(x = shotType, y = freq, fill = filter)) +
    geom_col(position = 'dodge') +
    scale_y_continuous(labels = scales::percent)+
    labs(#title = 'Shot Type Frequencies for Points Won',
      y = 'Frequency',
      x = 'Shot Type',
      fill = '') + 
    scale_fill_manual(values = pal)  + theme_minimal(base_size = 16) +
    theme(legend.position = "bottom")
  
}
# This creates a bar chart breaking down the Error Types for the outcome from the OutcomesBar function above it
BreakdownError <- function(data, time = 'Career', name, outcomeParam = 'UnforcedError', won = T){
  data <- data %>% filter(case_when(won ~ !pointWonBy,
                                    !won ~ pointWonBy == 1)) %>%
    filter(outcome == outcomeParam)
  
  if(time != 'Career'){
    data <- data %>% filter(format(date, format = '%Y') == time)
  }
  
  dataP <- data %>% filter(player == name)
  
  getFreq <- function(df, name){
    df %>% group_by(errorType) %>% 
      summarize(n = n()) %>% 
      #ungroup() %>% 
      mutate(errorType = factor(errorType, levels = errorType[order(n)]),
             freq = n / sum(n),
             filter = name)
  }
  
  data %>% getFreq('Team Average') %>% rbind(getFreq(dataP, name)) %>% 
    ggplot(aes(x = errorType, y = freq, fill = filter)) +
    geom_col(position = 'dodge') +
    scale_y_continuous(labels = scales::percent)+
    labs(#title = 'Shot Type Frequencies for Points Won',
      y = 'Frequency',
      x = 'Error Type',
      fill = '') + 
    scale_fill_manual(values = pal)  + theme_minimal(base_size = 16) +
    theme(legend.position = "bottom")
  
}

ErrorTable  <- function(data, name, won = T){
  data <- data %>% 
    filter(player == name, 
           pointWonBy == 1,
           outcome %in% c('UnforcedError', 'ForcedError')) %>% 
    mutate(outcome = case_when(outcome == 'UnforcedError' ~ 'Unforced',
                               T ~ 'Forced'))%>% 
    group_by(outcome, errorType, shotType) %>% 
    rename(Error = outcome,
           Type = errorType,
           Shot = shotType) %>% 
    summarise(Count = n()) %>% ungroup() %>% 
    mutate(Frequency = scales::percent(Count / sum(Count))) %>% 
    arrange(desc(Count))
  
  data %>% datatable(options = list(dom = 't', pageLength = 15))
  
}


TreeMap <- function(data,  name, is.server = T){
  
  data <- data %>% filter(player == name, 
                          case_when(is.server ~ !server,
                                    !is.server ~ server == 1))

  data <- data %>% mutate(FirstServe = case_when(firstServeIn ~ 'First Serve In', T ~ 'First Serve Out', ),
                  pointOutcome = case_when(!pointWonBy ~ 'Point Won', T ~ 'Point Lost')) %>% 
    select(player, FirstServe, pointOutcome, outcome, errorType, shotType) %>% 
    group_by(player, FirstServe, pointOutcome, outcome, shotType, errorType) %>% 
    summarise(Count = n())
  
  data %>%  treemap(index = c('FirstServe', 'pointOutcome', 'outcome'), vSize = 'Count', type = 'index',
                 title = 'Outcome Breakdown when Serving', 
                 palette = pal[c(1,3)]) %>% d3tree()
}

CIZR %>% TreeMap('Bianca Moldovan')


#Takes player data
KeyStats <- function(data, name, time = 'Career', top = T){
  
  data <- data %>% group_by(player) %>% mutate(across(everything(), ~ . / TotalMatches)) %>% 
    mutate(`Break Point Win Percent` = BreakPointsWon/BreakPoints,
           `First Serve In Percent` = FirstServeInCount/FirstServeCount,
           `First Serve Won Percent` = FirstServeWonCount/FirstServeInCount,
           `Second Serve In Percent` = SecondServeInCount/SecondServeCount,
           `Second Serve Won Percent` = SecondServeWonCount/SecondServeInCount,
           `First Serve Returned Percent` = FirstReturnedInCount/FirstReturnerCount,
           `Second Serve Returned Percent` = SecondReturnedInCount/SecondReturnerCount,
           `Short Rally Win Percent` = ShortRalliesWon/ShortRallies, 
           `Med Rally Win Percent` = MedRalliesWon/MedRallies,
           `Long Rally Win Percent` = LongRalliesWon/ LongRallies,
           #ServiceGameWinPct = ServiceGamesWon/ServiceGames
    ) %>% 
    select(-TotalMatches) 
  
  for(col in 2:length(data)){
    data[,col] = min_rank(pull(data,col))
    if( !(colnames(data[,col]) %in% c('UnforcedErrors', 'DoubleFault'))){
      data[,col] = max(data[,col]) - data[,col] + 1
    }
  }
  
  keeps<-c("player", AllData$matchMeasures  )
  
  sym = sym(name)
  
  data <- data %>% rename(`Match Win Percent` = MatchesWon,
                                           `Sets Won per Match` = SetsWon,
                                           `Games Won per Match` = GamesWon,
                                           `Points Won per Match` = PointsWon) %>%
    select(keeps) %>% 
    pivot_longer(-player, 'variable', 'value') %>% pivot_wider(variable, player)%>% 
    ungroup() %>% select(variable, !!sym)
  # 
  #   if (top) {
  #     vec <- data %>% slice_min(!!sym, n = 3)  %>% pull(variable)
  #   }
  #   else{
  #     vec <- data  %>% slice_max(!!sym, n = 3)  %>% pull(variable)
  #   }
  
  top <- data %>% slice_min(!!sym, n = 3)  %>% pull(variable)
  bottom <- data  %>% slice_max(!!sym, n = 3)  %>% pull(variable)
  
  list(top = top[1:3], bottom = bottom[1:3] )
  
  
}

KeyStats(AllData$player, name = 'Bianca Moldovan', top = T)

teamLeaderboard <- function(){
  
  
  tb <- AllData$match %>% mutate(matchWon = as.numeric(matchWon)) %>% 
    group_by(player) %>% summarise(across(where(is.numeric), mean)) %>% 
    ungroup() %>% mutate(`Break Point Win Percent` = BreakPointsWon/BreakPoints,
                         `First Serve In Percent` = FirstServeInCount/FirstServeCount,
                         `First Serve Won Percent` = FirstServeWonCount/FirstServeInCount,
                         `Second Serve In Percent` = SecondServeInCount/SecondServeCount,
                         `Second Serve Won Percent` = SecondServeWonCount/SecondServeInCount,
                         `First Serve Returned Percent` = FirstReturnedInCount/FirstReturnerCount,
                         `Second Serve Returned Percent` = SecondReturnedInCount/SecondReturnerCount,
                         `Short Rally Win Percent` = ShortRalliesWon/ShortRallies, 
                         `Med Rally Win Percent` = MedRalliesWon/MedRallies,
                         `Long Rally Win Percent` = LongRalliesWon/ LongRallies) %>% #,ServiceGameWinPct = ServiceGamesWon/ServiceGames
     rename(`Match Win Percent` = matchWon,
                 `Sets Won per Match` = SetsWon,
                 `Games Won per Match` = GamesWon,
                 `Points Won per Match` = PointsWon) %>% 
    select(c(player, AllData$matchMeasures)) %>% 
    mutate(across(where(is.numeric), ~ round(.,digits = 2)), 
           across(c(2,11:20),  scales::percent))
  
  AllData$player %>% select(player, TotalMatches) %>% left_join(tb, by = 'player')%>% 
    remove_rownames() %>% 
    column_to_rownames(var = 'player') %>% 
    datatable(options = list(dom = 't',scrollX = TRUE, pageLength = 20))
}


playerStatByMonth <- function(name, measure, line = T){
  data <- AllData$match %>% filter(player == name) %>% mutate(matchWon = as.numeric(matchWon))
  
  if(line){
    data <- data %>% group_by(player,month) %>% summarize(#matchWon = mean(),
                                                          across(where(is.numeric), mean))
  }
  
  data <- data %>%  mutate(`Break Point Win Percent` = BreakPointsWon/BreakPoints,
           `First Serve In Percent` = FirstServeInCount/FirstServeCount,
           `First Serve Won Percent` = FirstServeWonCount/FirstServeInCount,
           `Second Serve In Percent` = SecondServeInCount/SecondServeCount,
           `Second Serve Won Percent` = SecondServeWonCount/SecondServeInCount,
           `First Serve Returned Percent` = FirstReturnedInCount/FirstReturnerCount,
           `Second Serve Returned Percent` = SecondReturnedInCount/SecondReturnerCount,
           `Short Rally Win Percent` = ShortRalliesWon/ShortRallies, 
           `Med Rally Win Percent` = MedRalliesWon/MedRallies,
           `Long Rally Win Percent` = LongRalliesWon/ LongRallies,
           #ServiceGameWinPct = ServiceGamesWon/ServiceGames
    ) %>% rename(`Match Win Percent` = matchWon,
                 `Sets Won per Match` = SetsWon,
                 `Games Won per Match` = GamesWon,
                 `Points Won per Match` = PointsWon) 
  
  
  if(line) data <- data%>% select(c(player, month, AllData$matchMeasures))
  else data <- data  %>%  select(c(player,matchName, date, AllData$matchMeasures))
  
  #return(data)
  
  if(line){
    if(word(measure, -1) != 'Match'){ m_title = str_c(measure, ' per Match')}
    else{m_title = measure}
    
    g <- data %>% ggplot(aes(x = month, y = !!sym(measure))) +
      geom_point(color = '#D45555') +
      geom_line(aes(group = 1) ,color = '#D45555')+
      labs(title = str_c('Average ' , m_title, " by Month"), 
           x = "")+
      #stat_summary(fun=sum, geom="line", color = '#D45555') +
      theme_minimal()
    
  }else{
    g <-  data %>% ungroup() %>% 
      mutate(`Match Outcome` = case_when(`Match Win Percent` == 1 ~ 'Won', 
                                         TRUE ~ 'Lost'))%>% 
      ggplot(aes(x = date, y = !!sym(measure), color = `Match Outcome`, label = matchName)) +
      geom_point() +
      labs(title = str_c(measure, " by Date"), 
           x = "")+
      scale_color_manual(values = pal[c(2,1)]) + 
      theme_minimal() 
  }
  
  if(word(measure, -1) == 'Percent') {
    g <- g + scale_y_continuous(limits = c(0,1), labels = scales::percent)
  }else{g <- g + ylim(c(0,NA))}
  
  return(ggplotly(g) %>% layout(legend = list(orientation = 'h', x= .35, y = -.2)))
    
}

playerStatTable <- function(name){
  
  getMeasures <- function(data){
    data %>% summarise(across(where(is.numeric), sum)) %>% 
      ungroup() %>% mutate(`Break Point Win Percent` = BreakPointsWon/BreakPoints,
                           `First Serve In Percent` = FirstServeInCount/FirstServeCount,
                           `First Serve Won Percent` = FirstServeWonCount/FirstServeInCount,
                           `Second Serve In Percent` = SecondServeInCount/SecondServeCount,
                           `Second Serve Won Percent` = SecondServeWonCount/SecondServeInCount,
                           `First Serve Returned Percent` = FirstReturnedInCount/FirstReturnerCount,
                           `Second Serve Returned Percent` = SecondReturnedInCount/SecondReturnerCount,
                           `Short Rally Win Percent` = ShortRalliesWon/ShortRallies, 
                           `Med Rally Win Percent` = MedRalliesWon/MedRallies,
                           `Long Rally Win Percent` = LongRalliesWon/ LongRallies)%>% #,ServiceGameWinPct = ServiceGamesWon/ServiceGames
      rename(`Match Win Percent`    = matchWon,
             `Sets Won per Match`   = SetsWon,
             `Games Won per Match`  = GamesWon,
             `Points Won per Match` = PointsWon)
  }
  
  years <- AllData$match %>% filter(player == name) %>% 
    mutate(matchWon = as.numeric(matchWon)) %>% 
    group_by(year) %>% getMeasures()  %>% select(c(year, AllData$matchMeasures))
  
  career <- AllData$match %>% filter(player == name) %>% 
    mutate(matchWon = as.numeric(matchWon)) %>% 
    group_by(player) %>% getMeasures()  %>% select(AllData$matchMeasures)
  
  rbind(cbind(year = 'Career', career), years ) %>% 
    rename(`Total Matches Won`  = `Match Win Percent`   ,
          `Total Sets Won`   = `Sets Won per Match`  ,
          `Total Games Won`   = `Games Won per Match` ,
          `Total Points Won`   = `Points Won per Match`) %>% 
    mutate(across(11:20, ~ str_c(round(100 * . ), '%')))%>% 
    remove_rownames() %>% 
    column_to_rownames(var = 'year') %>% 
    datatable(options = list(dom = 't',scrollX = TRUE))
}
  
ErrorShot <- function(data, name){
  dataP <- data %>% filter(player == name) %>% PieChartMutate()
  dfError <- data.frame(
    UE <- dataP$UnforcedErrors,
    Error = c("Forehand", "Backhand", "Volley"),
    ErrorAmounts = c(dataP$ErrorByForehand/UE, dataP$ErrorByBackhand/UE, dataP$ErrorByVolley/UE) %>% round(3)
  )
  
  dfError <- dfError %>%
    arrange(desc(Error)) %>%
    mutate(lab.ypos = cumsum(ErrorAmounts) - 0.5*ErrorAmounts)
  ggplot(dfError, aes(x = "", y = ErrorAmounts, fill = Error)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0)+
    geom_text(aes(y = lab.ypos, label = scales::percent(ErrorAmounts)), color = "white")+
    scale_fill_manual(values = pal) +
    theme_void(base_size = 16)
}

PieChartMutate <- function(data){
  data %>% group_by(player) %>% 
    summarise(Winners = sum(!pointWonBy & outcome == 'Winner'),
              ErrorsForced = sum(!pointWonBy & outcome == 'ForcedError'), 
              UnforcedErrors = sum(pointWonBy & outcome == 'UnforcedError'),
              ErrorByNet = sum(pointWonBy & outcome == 'UnforcedError' & errorType == 'Net'),
              ErrorByOut = sum(pointWonBy & outcome == 'UnforcedError' & errorType == 'Long'),
              ErrorByForehand = sum(pointWonBy & outcome == 'UnforcedError' & shotType == 'Forehand'),
              ErrorByBackhand = sum(pointWonBy & outcome == 'UnforcedError' & shotType == 'Backhand'),
              ErrorByVolley = sum(pointWonBy & outcome == 'UnforcedError' & shotType == 'Volley'),
              WinnerByForehand = sum(!pointWonBy & outcome == 'Winner' & shotType == 'Forehand'),
              WinnerByBackhand = sum(!pointWonBy & outcome == 'Winner' & shotType == 'Backhand'),
              WinnerByVolley = sum(!pointWonBy & outcome == 'Winner' & shotType == 'Volley'),
              WinnersToUnforcedErrors = Winners/UnforcedErrors)
}


ErrorType <- function(data, name){
  dataP <- data %>% filter(player == name) %>% PieChartMutate()
  dfError <- data.frame(
    UE <- dataP$UnforcedErrors,
    Error = c("Out", "Net"),
    ErrorAmounts = c(dataP$ErrorByOut/UE, dataP$ErrorByNet/UE) %>% round(3)
  )
  
  dfError <- dfError %>%
    arrange(desc(Error)) %>%
    mutate(lab.ypos = cumsum(ErrorAmounts) - 0.5*ErrorAmounts)
  ggplot(dfError, aes(x = "", y = ErrorAmounts, fill = Error)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0)+
    geom_text(aes(y = lab.ypos, label = scales::percent(ErrorAmounts)), color = "white")+
    scale_fill_manual(values = pal) + theme_void(base_size = 16)
}

pal <- c('#D45555', '#A9A9A9', '#414770', '#566246', '#FAFF81','#4b5f6d')
### Palette ###
# Indian Red :  #D45555
# Silver Chalice: #A9A9A9
# Purple Navy: #414770
# Ebony : #566246
# Canary : #FAFF81
# Charcoal: '#4b5f6d'
