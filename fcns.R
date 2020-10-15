### Filtering and building plots###

library(tidyverse)
library(plotly)


# CIZR <- data.table::fread('~/MSA 21/AA 502/Visualization/CIZR CSV 2018-2020.csv')
CIZR <- data.table::fread('C:/Users/ericd/Downloads/CIZR CSV 2018-2020.csv')  # on Eric's machine

CIZR <- CIZR %>% 
  mutate(date = as.Date(date, '%m/%d/%y'),
         player = case_when(str_detect(player, 'Rebol') ~ 'Amanda Rebol',
                            str_detect(player, 'Moldovan') ~ 'Bianca Moldovan',
                            str_detect(player, 'Bridges') ~ 'Taylor Bridges',
                            str_detect(player, 'Reami') ~ 'Adriana Reami' ,
                            str_detect(player,'Wiktorin') ~ 'Claudia Wiktorin' ,
                            TRUE ~ player)
  ) 


PlayerFilter <- function(CIZR){
  CIZR %>% 
    MatchFilter() %>% 
    group_by(player) %>% summarise(TotalMatches = n(),
                                       MatchesWon = sum(SetsWon == 2),
                                       across(where(is.numeric), sum))
}

#Will change
MatchFilter <- function(CIZR) {
  CIZR %>% 
    group_by(matchId, player, date) %>% 
    summarise(TotalSets = max(set),
              SetsWon = sum(gamesWon == 5 & pointsWon == 3 & !pointWonBy) + min(setsWon),
              TotalGames = sum(!score & !oppScore),
              GamesWon = sum(!score & !oppScore & !gameWonBy),
              PointsWon = sum(!pointWonBy),
              oppPointsWon = sum(pointWonBy),
              TotalPoints = PointsWon + oppPointsWon,
              TotalDeuces = sum(noAd),
              DeucesWon = sum(noAd & !pointWonBy),
              Winners = sum(!pointWonBy & outcome == 'Winner'),
              ErrorsForced = sum(!pointWonBy & outcome == 'ForcedError'), 
              UnforcedErrors = sum(pointWonBy & outcome == 'UnforcedError'),
              BreakPoints =  sum(server & returnerScore == 40),
              BreakPointsWon = sum(server & returnerScore == 40 & !pointWonBy),
              Aces = sum(!pointWonBy & outcome == 'Ace'),
              FirstServeCount = sum(!server),
              FirstServeInCount = sum(!server & firstServeIn),
              FirstServeWonCount = sum(!server & firstServeIn & !pointWonBy),
              FirstServeInPct = FirstServeInCount/FirstServeCount,
              FirstServeWonPct = FirstServeWonCount/FirstServeInCount, 
              SecondServeCount = sum(!server & !firstServeIn), 
              SecondServeInCount = sum(!server & !firstServeIn & !(outcome %in% c('Fault', 'Let'))),
              SecondServeWonCount = sum(!server & !firstServeIn & !pointWonBy),
              SecondServeInPct = SecondServeInCount/SecondServeCount,
              SecondServeWonPct = SecondServeWonCount/SecondServeCount, 
              DoubleFault = sum(!server & !firstServeIn & outcome %in% c('Fault', 'Let')),
              FirstReturnerCount = sum(server & firstServeIn),
              FirstReturnedInCount = sum(server & firstServeIn & returnInPlay),
              FirstReturnPct = FirstReturnedInCount/FirstReturnerCount,
              SecondReturnerCount = sum(server & !firstServeIn),
              SecondReturnedInCount = sum(server & !firstServeIn & returnInPlay),
              SecondReturnPct = SecondReturnedInCount/SecondReturnerCount,
              ShortRallies = sum(rallyLength < 5),
              ShortRalliesWon = sum(rallyLength < 5 & !pointWonBy),
              ShortRallyWinPct = ShortRalliesWon/ShortRallies, 
              MedRallies = sum(rallyLength > 4 & rallyLength < 9),
              MedRalliesWon = sum(rallyLength > 4 & rallyLength < 9 & !pointWonBy),
              MedRallyWinPct = MedRalliesWon/MedRallies,
              LongRallies = sum(rallyLength > 8 ),
              LongRalliesWon = sum(rallyLength > 8 & !pointWonBy),
              LongRallyWinPct = LongRalliesWon/ LongRallies,
              ServiceGames = sum(!pointsWon & !oppPointsWon & !server),
              ServiceGamesWon = sum(!pointsWon & !oppPointsWon & !server & !gameWonBy) ) %>% 
    arrange(date) %>% 
    mutate(year = as.factor(format(date, format = '%Y')), month = as.factor(format(date, format = "%Y-%b")))
}

#Will change
SetFilter <- function(CIZR){
  CIZR %>% 
    group_by(setId, player, date) %>% 
    summarise(TotalGames = sum(!score & !oppScore),
              GamesWon = sum(!score & !oppScore & !gameWonBy),
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
              FirstServeInPct = FirstServeInCount/FirstServeCount,
              FirstServeWonPct = FirstServeWonCount/FirstServeInCount, 
              SecondServeCount = sum(!server & !firstServeIn), 
              SecondServeInCount = sum(!server & !firstServeIn & !(outcome %in% c('Fault', 'Let'))),
              SecondServeWonCount = sum(!server & !firstServeIn & !pointWonBy),
              SecondServeInPct = SecondServeInCount/SecondServeCount,
              SecondServeWonPct = SecondServeWonCount/SecondServeCount, 
              DoubleFault = sum(!server & !firstServeIn & outcome %in% c('Fault', 'Let')),
              FirstReturnerCount = sum(server & firstServeIn),
              FirstReturnedInCount = sum(server & firstServeIn & returnInPlay),
              FirstReturnPct = FirstReturnedInCount/FirstReturnerCount,
              SecondReturnerCount = sum(server & !firstServeIn),
              SecondReturnedInCount = sum(server & !firstServeIn & returnInPlay),
              SecondReturnPct = SecondReturnedInCount/SecondReturnerCount,
              ShortRallies = sum(rallyLength < 5),
              ShortRalliesWon = sum(rallyLength < 5 & !pointWonBy),
              ShortRallyWinPct = ShortRalliesWon/ShortRallies, 
              MedRallies = sum(rallyLength > 4 & rallyLength < 9),
              MedRalliesWon = sum(rallyLength > 4 & rallyLength < 9 & !pointWonBy),
              MedRallyWinPct = MedRalliesWon/MedRallies,
              LongRallies = sum(rallyLength > 8 ),
              LongRalliesWon = sum(rallyLength > 8 & !pointWonBy),
              LongRallyWinPct = LongRalliesWon/ LongRallies,
              ServiceGames = sum(!pointsWon & !oppPointsWon & !server),
              ServiceGamesWon = sum(!pointsWon & !oppPointsWon & !server & !gameWonBy)
    ) %>% 
    arrange(date) %>% 
    mutate(year = as.factor(format(date, format = '%Y')), month = as.factor(format(date, format = "%Y-%B")))
}

#will change slightly
GameFilter <- function(CIZR){
  CIZR %>% 
    group_by(gameId, player, date) %>% 
    summarise(PointsWon = sum(!pointWonBy),
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
              FirstServeInPct = FirstServeInCount/FirstServeCount,
              FirstServeWonPct = FirstServeWonCount/FirstServeInCount, 
              SecondServeCount = sum(!server & !firstServeIn), 
              SecondServeInCount = sum(!server & !firstServeIn & !(outcome %in% c('Fault', 'Let'))),
              SecondServeWonCount = sum(!server & !firstServeIn & !pointWonBy),
              SecondServeInPct = SecondServeInCount/SecondServeCount,
              SecondServeWonPct = SecondServeWonCount/SecondServeCount, 
              DoubleFault = sum(!server & !firstServeIn & outcome %in% c('Fault', 'Let')),
              FirstReturnerCount = sum(server & firstServeIn),
              FirstReturnedInCount = sum(server & firstServeIn & returnInPlay),
              FirstReturnPct = FirstReturnedInCount/FirstReturnerCount,
              SecondReturnerCount = sum(server & !firstServeIn),
              SecondReturnedInCount = sum(server & !firstServeIn & returnInPlay),
              SecondReturnPct = SecondReturnedInCount/SecondReturnerCount,
              ShortRallies = sum(rallyLength < 5),
              ShortRalliesWon = sum(rallyLength < 5 & !pointWonBy),
              ShortRallyWinPct = ShortRalliesWon/ShortRallies, 
              MedRallies = sum(rallyLength > 4 & rallyLength < 9),
              MedRalliesWon = sum(rallyLength > 4 & rallyLength < 9 & !pointWonBy),
              MedRallyWinPct = MedRalliesWon/MedRallies,
              LongRallies = sum(rallyLength > 8 ),
              LongRalliesWon = sum(rallyLength > 8 & !pointWonBy),
              LongRallyWinPct = LongRalliesWon/ LongRallies,
              ServiceGames = sum(!pointsWon & !oppPointsWon & !server),
              ServiceGamesWon = sum(!pointsWon & !oppPointsWon & !server & !gameWonBy)
    )  %>% 
    arrange(date) %>% 
    mutate(year = as.factor(format(date, format = '%Y')), month = as.factor(format(date, format = "%Y-%B")))
}


AllData = list(player = PlayerFilter(CIZR), match = MatchFilter(CIZR), set = SetFilter(CIZR), game = GameFilter(CIZR))

#Creates a filter to use with the input values for time period - either filters a specific year or returns original data
yearFilter <- function(data, year_selected){
  if(year_selected == 'Career'){
    return(data)}
  else{ return(filter(data, year == year_selected))}
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

#This creates a bar chart of the outcome of points over a give time period, compared to the teams average
#filter by whether the point was won or lost by the nscu player
OutcomesBar <- function(data, time = 'Career', name, won = T){
  data <- data %>% filter(case_when(won ~ !pointWonBy,
                                    !won ~ pointWonBy == 1))
  
  if(time != 'Career'){
    data <- data %>% filter(format(date, format = '%Y') == time)
  }
  
  dataP <- data %>% filter(player == name)
  
  getFreq <- function(df, name){
    df %>% group_by(outcome) %>% 
      summarize(n = n()) %>% 
      #ungroup() %>% 
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



pal <- c('#D45555', '#A9A9A9', '#414770', '#566246', '#FAFF81','#4b5f6d')
### Palette ###
# Indian Red :  #D45555
# Silver Chalice: #A9A9A9
# Purple Navy: #414770
# Ebony : #566246
# Canary : #FAFF81
# Charcoal: '#4b5f6d'
