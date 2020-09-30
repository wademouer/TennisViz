# THIS IS MY COMMENT

library(tidyverse)

CIZR <- data.table::fread('~/MSA 21/AA 502/Visualization/CIZR CSV 2018-2020.csv') #'~/MSA 21/AA 502/Visualization/CIZR CSV 2018-2020.csv'

distinct(CIZR, player)

#fix names! Some were improperly formatted
CIZR <- CIZR %>% 
  mutate(date = as.Date(date, '%m/%d/%y'),
         player = case_when(str_detect(player, 'Rebol') ~ 'Amanda Rebol',
                            str_detect(player, 'Moldovan') ~ 'Bianca Moldovan',
                            str_detect(player, 'Bridges') ~ 'Taylor Bridges',
                            str_detect(player, 'Reami') ~ 'Adriana Reami' ,
                            str_detect(player,'Wiktorin') ~ 'Claudia Wiktorin' ,
                            TRUE ~ player)
         ) 

#Summarizing Match Data
#There is still a bug for sets won due to missing data
MatchData <- CIZR %>% 
  group_by(matchId, player, date) %>% 
  summarise(TotalSets = max(set),
            SetsWon = sum(gamesWon == 5 & pointsWon == 3 & !pointWonBy),
            TotalGames = sum(!score & !oppScore),
            TotalGamesWon = sum(!score & !oppScore & !gameWonBy),
            pointsWon = sum(!pointWonBy),
            oppPointsWon = sum(pointWonBy),
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
  arrange(date)


#summarising player data
#There is still a bug for sets won due to missing data 
PlayerData <- CIZR %>% 
  group_by(player) %>% 
  summarise(TotalMatches = sum(set == 1 & !gamesWon & !oppGamesWon & !pointsWon & !oppPointsWon),
            MatchesWon = sum(setsWon == 1 & gamesWon == 5 & pointsWon == 3 & !pointWonBy),
            TotalSets = sum(!gamesWon & !oppGamesWon & !pointsWon & !oppPointsWon),
            SetsWon = sum(gamesWon == 5 & pointsWon == 3 & !pointWonBy),
            TotalGames = sum(!score & !oppScore),
            TotalGamesWon = sum(!score & !oppScore & !gameWonBy),
            pointsWon = sum(!pointWonBy),
            oppPointsWon = sum(pointWonBy),
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
  ) 

