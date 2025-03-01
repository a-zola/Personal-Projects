#Libraries
library(htmltools)
library(devtools)
library(tidyverse)
library(nbastatR)
library(proto)
library(RSQLite)
library(gsubfn)
library(sqldf)
library(zoo)

#LoadGameLogs ###Run This Section First Before Running the Below Scripts###
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
gl <- game_logs(seasons = 2025, season_types = c("Regular Season"))


#CalculateMaxGamesByTeam
GamesPlayedTeam <- gl %>% group_by(nameTeam) %>% summarise("MaxGames" = max(numberGameTeamSeason))

GL_Filter <- left_join(gl, GamesPlayedTeam, by = "nameTeam")

View(gl)

#####################################################
#Player/Game Inputs
FocusTeam <- "MIA"
FocusOpponent <- "OKC"
OverUnder <- 222.5
LineSpread <- -14.5

#Grab Team Stats By Game - Includes Offense and Defense
GameTeamStats <- GL_Filter %>% group_by(idGame, numberGameTeamSeason, MaxGames, slugTeam, slugOpponent, yearSeason,dateGame)%>%
  summarise("Pts" = sum(pts),
            "Twos" = sum(fg2m),
            "Threes" = sum(fg3m),
            "FTM" = sum(ftm))

GameFocusTeamStats <- GameTeamStats %>% filter(slugTeam==FocusTeam || slugOpponent==FocusTeam)
GameFocusOpponentStats <- GameTeamStats %>% filter(slugTeam==FocusOpponent || slugOpponent==FocusOpponent)

#Game Totals Table for Each Team
FocusTeamGameTotals <- GameFocusTeamStats %>% group_by(idGame,dateGame) %>%
  summarise("PointTotals" = sum(Pts)) %>% arrange(desc(dateGame))

FocusTeamGameTotals <- FocusTeamGameTotals %>% mutate(OU = if_else(OverUnder < PointTotals, "Over", "Under"))

FocusTeamGameCounts <- FocusTeamGameTotals %>% group_by(OU) %>% summarise("OU Count" = n())

view(FocusTeamGameCounts)

FocusOpponentGameTotals <- GameFocusOpponentStats %>% group_by(idGame,dateGame) %>%
  summarise("PointTotals" = sum(Pts)) %>% arrange(desc(dateGame))

FocusOpponentGameTotals <- FocusOpponentGameTotals %>% mutate(OU = if_else(OverUnder < PointTotals, "Over", "Under"))

FocusOpponentGameCounts <- FocusOpponentGameTotals %>% group_by(OU) %>% summarise("OU Count" = n())

view(FocusOpponentGameCounts)

#Averages for Focus Team
FocusTeamGameAverage <- as.data.frame(mean(FocusTeamGameTotals$PointTotals))
FocusTeamGameAverage <- FocusTeamGameAverage %>% mutate("TimeFrame" = "Season")
FocusTeamGameAverage4 <- as.data.frame(mean(FocusTeamGameTotals$PointTotals[1:4]))
FocusTeamGameAverage4 <- FocusTeamGameAverage4 %>% mutate("TimeFrame" = "Last4")
FocusTeamGameAverage8 <- as.data.frame(mean(FocusTeamGameTotals$PointTotals[1:8]))
FocusTeamGameAverage8 <- FocusTeamGameAverage8 %>% mutate("TimeFrame" = "Last8")
FocusTeamGameAverage16 <- as.data.frame(mean(FocusTeamGameTotals$PointTotals[1:16]))
FocusTeamGameAverage16 <- FocusTeamGameAverage16 %>% mutate("TimeFrame" = "Last16")

colnames(FocusTeamGameAverage)[1] <- "Average"
colnames(FocusTeamGameAverage4)[1] <- "Average"
colnames(FocusTeamGameAverage8)[1] <- "Average"
colnames(FocusTeamGameAverage16)[1] <- "Average"

#Averages for Focus Opponent
FocusOpponentGameAverage <- as.data.frame(mean(FocusOpponentGameTotals$PointTotals))
FocusOpponentGameAverage <- FocusOpponentGameAverage %>% mutate("TimeFrame" = "Season")
FocusOpponentGameAverage4 <- as.data.frame(mean(FocusOpponentGameTotals$PointTotals[1:4]))
FocusOpponentGameAverage4 <- FocusOpponentGameAverage4 %>% mutate("TimeFrame" = "Last4")
FocusOpponentGameAverage8 <- as.data.frame(mean(FocusOpponentGameTotals$PointTotals[1:8]))
FocusOpponentGameAverage8 <- FocusOpponentGameAverage8 %>% mutate("TimeFrame" = "Last8")
FocusOpponentGameAverage16 <- as.data.frame(mean(FocusOpponentGameTotals$PointTotals[1:16]))
FocusOpponentGameAverage16 <- FocusOpponentGameAverage16 %>% mutate("TimeFrame" = "Last16")

colnames(FocusOpponentGameAverage)[1] <- "Average"
colnames(FocusOpponentGameAverage4)[1] <- "Average"
colnames(FocusOpponentGameAverage8)[1] <- "Average"
colnames(FocusOpponentGameAverage16)[1] <- "Average"

#Union Together
FocusTeamTotalsAggregate1 <- union_all(FocusTeamGameAverage,FocusTeamGameAverage4)
FocusTeamTotalsAggregate2 <- union_all(FocusTeamTotalsAggregate1,FocusTeamGameAverage8)
FocusTeamTotalsAggregate3 <- union_all(FocusTeamTotalsAggregate2,FocusTeamGameAverage16)

FocusOpponentTotalsAggregate1 <- union_all(FocusOpponentGameAverage,FocusOpponentGameAverage4)
FocusOpponentTotalsAggregate2 <- union_all(FocusOpponentTotalsAggregate1,FocusOpponentGameAverage8)
FocusOpponentTotalsAggregate3 <- union_all(FocusOpponentTotalsAggregate2,FocusOpponentGameAverage16)

FocusTeamTotalsAggregate3 <- FocusTeamTotalsAggregate3 %>% mutate(OU = if_else(OverUnder < Average, "Over", "Under"))
FocusOpponentTotalsAggregate3 <- FocusOpponentTotalsAggregate3 %>% mutate(OU = if_else(OverUnder < Average, "Over", "Under"))

view(FocusTeamTotalsAggregate3)
view(FocusOpponentTotalsAggregate3)


view(GameFocusTeamStats)
view(GameFocusOpponentStats)
view(FocusTeamGameTotals)
view(FocusOpponentGameTotals)
view(FocusTeamGameAverage)
view(FocusOpponentGameAverage)


#DefensiveStatsSeason
GameSeasonDefStats <- GameTeamStats %>% group_by(slugOpponent, yearSeason)%>%
summarise("avgPts" = mean(Pts),
            "avgtwopts" = mean(Twos),
            "avgthreepts" = mean(Threes),
            "avgFTM" = mean(FTM)) %>% mutate("TimeFrame" = "Season")

#DefensiveStatsLast4
GameSeasonDefStats4 <- GameTeamStats %>% filter(MaxGames - numberGameTeamSeason <= 4) %>% group_by(slugOpponent, yearSeason)%>%
  summarise("avgPts" = mean(Pts),
            "avgtwopts" = mean(Twos),
            "avgthreepts" = mean(Threes),
            "avgFTM" = mean(FTM)) %>% mutate("TimeFrame" = "Last4")

#DefensiveStatsLast8
GameSeasonDefStats8 <- GameTeamStats %>% filter(MaxGames - numberGameTeamSeason <= 8) %>% group_by(slugOpponent, yearSeason)%>%
  summarise("avgPts" = mean(Pts),
            "avgtwopts" = mean(Twos),
            "avgthreepts" = mean(Threes),
            "avgFTM" = mean(FTM)) %>% mutate("TimeFrame" = "Last8")

#DefensiveStatsLast16
GameSeasonDefStats16 <- GameTeamStats %>% filter(MaxGames - numberGameTeamSeason <= 16) %>% group_by(slugOpponent, yearSeason)%>%
  summarise("avgPts" = mean(Pts),
            "avgtwopts" = mean(Twos),
            "avgthreepts" = mean(Threes),
            "avgFTM" = mean(FTM)) %>% mutate("TimeFrame" = "Last16")



#Offensive Stats Season
GameSeasonOffStats <- GameTeamStats %>% group_by(slugTeam, yearSeason)%>%
  summarise("avgPts" = mean(Pts),
            "avgtwopts" = mean(Twos),
            "avgthreepts" = mean(Threes),
            "avgFTM" = mean(FTM)) %>% mutate("TimeFrame" = "Season")

GameSeasonOffStats4 <- GameTeamStats %>%filter(MaxGames - numberGameTeamSeason <= 4) %>% group_by(slugTeam, yearSeason) %>%
  summarise("avgPts" = mean(Pts),
            "avgtwopts" = mean(Twos),
            "avgthreepts" = mean(Threes),
            "avgFTM" = mean(FTM)) %>% mutate("TimeFrame" = "Last4")

GameSeasonOffStats8 <- GameTeamStats %>%filter(MaxGames - numberGameTeamSeason <= 8) %>% group_by(slugTeam, yearSeason) %>%
  summarise("avgPts" = mean(Pts),
            "avgtwopts" = mean(Twos),
            "avgthreepts" = mean(Threes),
            "avgFTM" = mean(FTM)) %>% mutate("TimeFrame" = "Last8")

GameSeasonOffStats16 <- GameTeamStats %>%filter(MaxGames - numberGameTeamSeason <= 16) %>% group_by(slugTeam, yearSeason) %>%
  summarise("avgPts" = mean(Pts),
            "avgtwopts" = mean(Twos),
            "avgthreepts" = mean(Threes),
            "avgFTM" = mean(FTM)) %>% mutate("TimeFrame" = "Last16")




#LeagueAverageOff&Def
NBADefStats <- GameTeamStats %>% group_by(yearSeason) %>%
  summarise("avgPts" = mean(Pts),
            "avgtwopts" = mean(Twos),
            "avgthreepts" = mean(Threes),
            "avgFTM" = mean(FTM)) %>% mutate("TimeFrame" = "Season")



#LeagueDefensiveStatsLast4
NBADefStats4 <- GameTeamStats %>% filter(MaxGames - numberGameTeamSeason <= 4) %>% group_by(yearSeason) %>%
  summarise("avgPts" = mean(Pts),
            "avgtwopts" = mean(Twos),
            "avgthreepts" = mean(Threes),
            "avgFTM" = mean(FTM)) %>% mutate("TimeFrame" = "Last4")



#LeagueDefensiveStatsLast8
NBADefStats8 <- GameTeamStats %>% filter(MaxGames - numberGameTeamSeason <= 8) %>% group_by(yearSeason) %>%
  summarise("avgPts" = mean(Pts),
            "avgtwopts" = mean(Twos),
            "avgthreepts" = mean(Threes),
            "avgFTM" = mean(FTM)) %>% mutate("TimeFrame" = "Last8")



#LeagueDefensiveStatsLast16
NBADefStats16 <- GameTeamStats %>% filter(MaxGames - numberGameTeamSeason <= 16) %>% group_by(yearSeason) %>%
  summarise("avgPts" = mean(Pts),
            "avgtwopts" = mean(Twos),
            "avgthreepts" = mean(Threes),
            "avgFTM" = mean(FTM)) %>% mutate("TimeFrame" = "Last16")



#Defensive Multipliers
#CreateMultiplierDataSets
DefMultipliersSeason <- left_join(GameSeasonDefStats, NBADefStats, by = "yearSeason")
DefMultipliersSeason4 <- left_join(GameSeasonDefStats4, NBADefStats4, by = "yearSeason")
DefMultipliersSeason8 <- left_join(GameSeasonDefStats8, NBADefStats8, by = "yearSeason")
DefMultipliersSeason16 <- left_join(GameSeasonDefStats16, NBADefStats16, by = "yearSeason")






DefMultipliers <- DefMultipliersSeason %>% group_by(slugOpponent) %>%
  summarise("PtsQuotient" = avgPts.x/avgPts.y,
            "TwosQuotient" = avgtwopts.x/avgtwopts.y,
            "ThreesQuotient" = avgthreepts.x/avgthreepts.y,
            "FTMQuotient" = avgFTM.x/avgFTM.y) %>% mutate("TimeFrame" = "Season")

DefMultipliers4 <- DefMultipliersSeason4 %>% group_by(slugOpponent) %>%
  summarise("PtsQuotient" = avgPts.x/avgPts.y,
            "TwosQuotient" = avgtwopts.x/avgtwopts.y,
            "ThreesQuotient" = avgthreepts.x/avgthreepts.y,
            "FTMQuotient" = avgFTM.x/avgFTM.y) %>% mutate("TimeFrame" = "Last4")

DefMultipliers8 <- DefMultipliersSeason8 %>% group_by(slugOpponent) %>%
  summarise("PtsQuotient" = avgPts.x/avgPts.y,
            "TwosQuotient" = avgtwopts.x/avgtwopts.y,
            "ThreesQuotient" = avgthreepts.x/avgthreepts.y,
            "FTMQuotient" = avgFTM.x/avgFTM.y) %>% mutate("TimeFrame" = "Last8")

DefMultipliers16 <- DefMultipliersSeason16 %>% group_by(slugOpponent) %>%
  summarise("PtsQuotient" = avgPts.x/avgPts.y,
            "TwosQuotient" = avgtwopts.x/avgtwopts.y,
            "ThreesQuotient" = avgthreepts.x/avgthreepts.y,
            "FTMQuotient" = avgFTM.x/avgFTM.y) %>% mutate("TimeFrame" = "Last16")

Multipliers1 <- union_all(DefMultipliers,DefMultipliers4)
Multipliers2 <- union_all(Multipliers1,DefMultipliers8)
AggregateMultipliers <- union_all(Multipliers2, DefMultipliers16)
AggregateMultipliersFocusTeam <- AggregateMultipliers %>% filter(slugOpponent == FocusTeam)
AggregateMultipliersFocusOpponent <- AggregateMultipliers %>% filter(slugOpponent == FocusOpponent)

View(AggregateMultipliers)






#FocusTeamOff
FocusTeamOffSeason <- GameSeasonOffStats %>% filter(slugTeam == FocusTeam)
FocusTeamOffSeason4 <- GameSeasonOffStats4 %>% filter(slugTeam == FocusTeam)
FocusTeamOffSeason8 <- GameSeasonOffStats8 %>% filter(slugTeam == FocusTeam)
FocusTeamOffSeason16 <- GameSeasonOffStats16 %>% filter(slugTeam == FocusTeam)

FocusTeamAggregateOff1 <- union_all(FocusTeamOffSeason,FocusTeamOffSeason4)
FocusTeamAggregateOff2 <- union_all(FocusTeamAggregateOff1,FocusTeamOffSeason8)
FocusTeamOff <- union_all(FocusTeamAggregateOff2,FocusTeamOffSeason16)






#FocusOpponentOff

FocusOpponentOffSeason <- GameSeasonOffStats %>% filter(slugTeam == FocusOpponent)
FocusOpponentOffSeason4 <- GameSeasonOffStats4 %>% filter(slugTeam == FocusOpponent)
FocusOpponentOffSeason8 <- GameSeasonOffStats8 %>% filter(slugTeam == FocusOpponent)
FocusOpponentOffSeason16 <- GameSeasonOffStats16 %>% filter(slugTeam == FocusOpponent)

FocusOpponentAggregateOff3 <- union_all(FocusOpponentOffSeason,FocusOpponentOffSeason4)
FocusOpponentAggregateOff4 <- union_all(FocusOpponentAggregateOff3,FocusOpponentOffSeason8)
FocusOpponentOff <- union_all(FocusOpponentAggregateOff4,FocusOpponentOffSeason16)






#FocusTeamDef
FocusTeamDefSeason <- GameSeasonDefStats %>% filter(slugOpponent == FocusTeam)
FocusTeamDefSeason4 <- GameSeasonDefStats4 %>% filter(slugOpponent == FocusTeam)
FocusTeamDefSeason8 <- GameSeasonDefStats8 %>% filter(slugOpponent == FocusTeam)
FocusTeamDefSeason16 <- GameSeasonDefStats16 %>% filter(slugOpponent == FocusTeam)

FocusTeamAggregateDef1 <- union_all(FocusTeamDefSeason,FocusTeamDefSeason4)
FocusTeamAggregateDef2 <- union_all(FocusTeamAggregateDef1,FocusTeamDefSeason8)
FocusTeamDef <- union_all(FocusTeamAggregateDef2,FocusTeamDefSeason16)



#FocusOpponentDef
FocusOpponentDefSeason <- GameSeasonDefStats %>% filter(slugOpponent == FocusOpponent)
FocusOpponentDefSeason4 <- GameSeasonDefStats4 %>% filter(slugOpponent == FocusOpponent)
FocusOpponentDefSeason8 <- GameSeasonDefStats8 %>% filter(slugOpponent == FocusOpponent)
FocusOpponentDefSeason16 <- GameSeasonDefStats16 %>% filter(slugOpponent == FocusOpponent)

FocusOpponentAggregateDef3 <- union_all(FocusOpponentDefSeason,FocusOpponentDefSeason4)
FocusOpponentAggregateDef4 <- union_all(FocusOpponentAggregateDef3,FocusOpponentDefSeason8)
FocusOpponentDef <- union_all(FocusOpponentAggregateDef4,FocusOpponentDefSeason16)

#join OpponentDefensiveMultipliers onto offensive stats

FocusTeamOffProj1 <- left_join(FocusTeamOff, AggregateMultipliersFocusOpponent, by = "TimeFrame")


FocusOpponentOffProj1 <- left_join(FocusOpponentOff, AggregateMultipliersFocusTeam, by = "TimeFrame")


#Grab Stats by timeframe

FocusTeamProj <- FocusTeamOffProj1 %>% group_by(TimeFrame,slugTeam) %>%
  mutate("ProjPts" = (avgPts*PtsQuotient),
         "Proj2Pts" = (avgtwopts*TwosQuotient*2),
         "Proj3Pts" = (avgthreepts*ThreesQuotient*3),
         "ProjFTM" = (avgFTM*FTMQuotient))

FocusOpponentProj <- FocusOpponentOffProj1 %>% group_by(TimeFrame,slugTeam) %>%
  mutate("ProjPts" = (-avgPts*PtsQuotient),
         "Proj2Pts" = (-avgtwopts*TwosQuotient*2),
         "Proj3Pts" = (-avgthreepts*ThreesQuotient*3),
         "ProjFTM" = (-avgFTM*FTMQuotient))

#Aggregate spread data

SpreadData <- union_all(FocusTeamProj,FocusOpponentProj)

SpreadData <- SpreadData %>% mutate(Points = Proj2Pts+Proj3Pts+ProjFTM)

Score <- select(SpreadData, slugTeam, TimeFrame, Proj2Pts, Proj3Pts, ProjFTM, Points)

view(Score)

View(SpreadData)

Projections <- SpreadData %>% group_by(TimeFrame) %>% summarise("Spread" = (Proj2Pts+Proj3Pts+ProjFTM))

Projections <- Projections %>% group_by(TimeFrame) %>% summarise("Spread" = sum(Spread))

Projections <- Projections %>% mutate("CoverMargin" = Spread - LineSpread)

Projections <- Projections %>% mutate(Winner = if_else(Spread > 0, FocusTeam, FocusOpponent))
Projections <- Projections %>% mutate(Spread_Winner = if_else(LineSpread < Spread, FocusTeam, FocusOpponent))

View(Projections)

#Making OU line

FocusOpponentProj1 <- FocusOpponentOffProj1 %>% group_by(TimeFrame,slugTeam) %>%
  mutate("ProjPts" = avgPts*PtsQuotient)

view(FocusTeamProj)
view(FocusOpponentProj)

OUData <- union_all(FocusTeamProj,FocusOpponentProj)

OUProjections <- SpreadData %>% group_by(TimeFrame) %>% summarise("PointTotal" = sum(abs(Proj2Pts)+abs(Proj3Pts)+abs(ProjFTM)))

OUProjections <- OUProjections %>% mutate(Side = if_else(PointTotal >= OverUnder, "Over", "Under"))

view(OUProjections)
