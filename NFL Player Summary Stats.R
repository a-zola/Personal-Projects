#install libraries
library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(dplyr)
library(tidyr)
library(proto)
library(RSQLite)
library(gsubfn)
library(sqldf)
library(zoo)
library(readr)

#Grab Data For Analysis
PlayerStats <- load_player_stats(seasons = TRUE)
schedules <- load_schedules(seasons = TRUE)
pbp <- load_pbp(2024)

view(head(pbp))
view(PlayerStats)

#Participation commented out
#load_participation(seasons = TRUE)
#participation <- load_participation(2022)
#view(participation)

#participation <- try({ # prevents cran errors
#  load_participation(seasons = 2022)
#})

#participation <- as.data.frame(participation)

#view(participation)

view(schedules)

EndSeason <- 2024

#Aggregate Weekly Defensive Stats By Team
DefWeeklyStats <- sqldf(" 

Select
P.game_id,
P.defteam,
P.week,
P.season,
P.spread_line,
avg(epa) as 'AverageEPA',
sum(case when play_type like 'pass' then yards_gained else 0 end) as 'PassYards',
sum(case when play_type like 'pass' then touchdown else 0 end) as 'PassingTDs',
avg(case when play_type like 'pass' then epa else null end) as 'AvgPassEPA',
sum(case when play_type like 'pass' then touchdown else 0 end) as 'PassingTouchdowns',
sum(case when play_type like 'pass' then interception else 0 end) as 'Interceptions',
sum(case when play_type like 'run' then yards_gained else 0 end) as 'RushYards',
sum(case when play_type like 'run' then 1 else 0 end) as 'RushAttempts',
sum(case when play_type like 'run' then touchdown else 0 end) as 'RushingTouchdowns',
sum(case when field_goal_result like 'made' then 1 else 0 end) as 'FGMade'


from pbp P

where 1=1

and play_type in ('pass','run','field_goal')

group by

P.game_id,
defteam,
P.week,
P.season,
P.spread_line

order by

P.game_id,
defteam,
P.week,
P.season,
P.spread_line

")

view(DefWeeklyStats)

#Aggregate Weekly offensive Stats By Team
OffWeeklyStats <- sqldf(" 

Select
P.game_id,
P.posteam,
P.week,
P.season,
P.spread_line,
avg(epa) as 'AverageEPA',
sum(case when play_type like 'pass' then yards_gained else 0 end) as 'PassYards',
sum(case when play_type like 'pass' then touchdown else 0 end) as 'PassingTDs',
avg(case when play_type like 'pass' then epa else null end) as 'AvgPassEPA',
sum(case when play_type like 'pass' then touchdown else 0 end) as 'PassingTouchdowns',
sum(case when play_type like 'pass' then interception else 0 end) as 'Interceptions',
sum(case when play_type like 'run' then yards_gained else 0 end) as 'RushYards',
sum(case when play_type like 'run' then 1 else 0 end) as 'RushAttempts',
sum(case when play_type like 'run' then touchdown else 0 end) as 'RushingTouchdowns',
sum(case when field_goal_result like 'made' then 1 else 0 end) as 'FGMade'


from pbp P

where 1=1

and play_type in ('pass','run','field_goal')

group by

P.game_id,
P.posteam,
P.week,
P.season,
P.spread_line

order by

P.game_id,
P.posteam,
P.week,
P.season,
P.spread_line

")

view(OffWeeklyStats)
#Grab Games
DistinctGames <- sqldf("
                       
                       
                       Select distinct
                       
                       home_team,
                       away_team,
                       week,
                       season
                       
                       from schedules
                       
                       
                       ")

#Include Opponent With PlayerStats Since its Not there inherently
PlayerStatsWOpponents <- sqldf("
                               
                               Select

                                case when DG.home_team = PS.recent_team then DG.away_team else DG2.home_team end as 'Opponent',
                                PS.*
                                
                                from PlayerStats PS
                                
                                left join DistinctGames DG
                                on DG.week = PS.week and DG.season = PS.season and DG.home_team = PS.recent_team
                                left join DistinctGames DG2
                                on DG2.week = PS.week and DG2.season = PS.season and DG2.away_team = PS.recent_team
                               
                               
                               
                               ")

#mean Stats For the Player Against Opposing Team By Season
meanStatsOpponent <- PlayerStatsWOpponents %>% filter(season >= EndSeason) %>%
  
  group_by(player_display_name,player_id, season, position_group, recent_team, Opponent) %>%summarise("MedPassYards" = mean(passing_yards),
                                                                                            "MedCompletions" = mean(completions),
                                                                                            "AvgpassTDs" = mean(passing_tds),
                                                                                            "MeanPassEPA" = mean(passing_epa),
                                                                                            "MedRushYards" = mean(rushing_yards),
                                                                                            "AvgRushingTds" = mean(rushing_tds),
                                                                                            "AvgRushingEPA" = mean(rushing_epa),
                                                                                            "MedRecYds" = mean(receiving_yards),
                                                                                            "AvgRecTds" = mean(receiving_tds),
                                                                                            "AvgRecEPA" = mean(receiving_epa),
                                                                                            "MedReceptions" = mean(receptions)
                                                                                            
  ) 

                                                
#mean Stats For the Player By Season
meanStatsSeason <- PlayerStatsWOpponents %>% filter(season >= EndSeason) %>%
  
  group_by(player_display_name,player_id, season,recent_team, position_group) %>% summarise("MedPassYards" = mean(passing_yards),
                                                                                  "MedCompletions" = mean(completions),
                                                                                  "AvgpassTDs" = mean(passing_tds),
                                                                                  "MeanPassEPA" = mean(passing_epa),
                                                                                  "MedRushYards" = mean(rushing_yards),
                                                                                  "AvgRushingTds" = mean(rushing_tds),
                                                                                  "AvgRushingEPA" = mean(rushing_epa),
                                                                                  "AvgCarries" = mean(carries),
                                                                                  "MedRecYds" = mean(receiving_yards),
                                                                                  "AvgRecTds" = mean(receiving_tds),
                                                                                  "AvgRecEPA" = mean(receiving_epa),
                                                                                  "MedReceptions" = mean(receptions),
                                                                                  
  )

meanStatsSeason <- meanStatsSeason %>% replace(is.na(.),0)



SummaryStatsSeason<- PlayerStatsWOpponents %>% filter(season >= EndSeason, week<=17) %>%
  
  group_by(player_display_name,player_id, season,recent_team, position_group) %>% summarise("PassYards" = sum(passing_yards),
                                                                                            "Completions" = sum(completions),
                                                                                            "PassTDs" = sum(passing_tds),
                                                                                            "MeanPassEPA" = mean(passing_epa),
                                                                                            "RushYards" = sum(rushing_yards),
                                                                                            "RushingTDs" = sum(rushing_tds),
                                                                                            "RushEPA" = mean(rushing_epa),
                                                                                            "RushAttempts" = sum(carries),
                                                                                            "RecYards" = sum(receiving_yards),
                                                                                            "RecTds" = sum(receiving_tds),
                                                                                            "AvgRecEPA" = mean(receiving_epa),
                                                                                            "Receptions" = sum(receptions),
                                                                                        
  )

view(SummaryStatsSeason)

#gamecounts by player

player_game_counts <- sqldf("select

                             player_display_name,
                             player_id,
                             season,
                             count(*) as 'GameCount'
                             
                             from PlayerStatsWOpponents
                             
                             group by
                             player_display_name,
                             player_id,
                             season
                            
                            ")

meanStatsSeason <- inner_join(meanStatsSeason,player_game_counts,by = c("season" = "season","player_id" = "player_id"))
#Edit After Here To Get all Players At Once

meanStatsSeason <- meanStatsSeason %>% mutate("FantastyPoints" = MedReceptions+0.1*MedRushYards+0.1*MedRecYds+6.0*AvgRecTds+6.0*AvgRushingTds+MedPassYards/25+AvgpassTDs*4)

meanStatsSeason <- meanStatsSeason %>% mutate("TotalFantastyPoints" = FantastyPoints*GameCount)

View(meanStatsOpponent)
View(meanStatsSeason)



#Aggregate Def Season Stats By Team
DefSeasonStats <- DefWeeklyStats %>% group_by(season, defteam) %>%
  summarise("MedPassYards" = mean(PassYards),
            "RiskPassYards" = sd(PassYards),
            "AvgPassTDs" = mean(PassingTouchdowns),
            "RiskPassTDs" = sd(PassingTouchdowns),
            "MeanInts" = mean(Interceptions),
            "meanRushYards" = mean(RushYards),
            "RiskRushYards" = sd(RushYards),
            "AvgRushTDs" = mean(RushingTouchdowns),
            "RiskRushTDs" = sd(RushingTouchdowns),
            "meanFG" = mean(FGMade)
  )


#Aggregate Def Season Stats For Entire NFL
DefNFLStats <- DefWeeklyStats %>% group_by(season) %>%
  summarise("MedPassYards" = mean(PassYards),
            "RiskPassYards" = sd(PassYards),
            "AvgPassTDs" = mean(PassingTouchdowns),
            "RiskPassTDs" = sd(PassingTouchdowns),
            "MeanInts" = mean(Interceptions),
            "meanRushYards" = mean(RushYards),
            "RiskRushYards" = sd(RushYards),
            "AvgRushTDs" = mean(RushingTouchdowns),
            "RiskRushTDs" = sd(RushingTouchdowns),
            "meanFG" = mean(FGMade)
  )

#join nfl season stats to team season stats
Calcs <- inner_join(DefSeasonStats, DefNFLStats, by = "season")

#Calculate Quotients/Multipliers
Calcs <- Calcs %>% mutate(
  "PYQuotient" = MedPassYards.x/MedPassYards.y,
  "PYRiskQuotient" = RiskPassYards.x/RiskPassYards.y,
  "PTDQuotient" = AvgPassTDs.x/AvgPassTDs.y,
  "PTDRiskQuotient" = RiskPassTDs.x/RiskPassTDs.y,
  "RYQuotient" = meanRushYards.x/meanRushYards.y,
  "RYRiskQuotient" = RiskRushYards.x/RiskRushYards.y,
  "RTDQuotient" = AvgRushTDs.x/AvgRushTDs.y,
  "RTDRiskQuotient" = RiskRushTDs.x/RiskRushTDs.y,
  "FGQuotient" = meanFG.x/meanFG.y,
  "IntQuotient" = MeanInts.x/MeanInts.y)

#Select Multipliers To Their Own df
Multipliers <- Calcs %>% 
  select(defteam, season, PYQuotient,PYRiskQuotient,PTDQuotient, PTDRiskQuotient,IntQuotient,RYQuotient,RYRiskQuotient,RTDQuotient,RTDRiskQuotient,FGQuotient) %>%
  filter(season >= EndSeason)



#Aggregate Def Season Stats By Team
OffSeasonStats <- OffWeeklyStats %>% group_by(season, posteam) %>%
  summarise("MedPassYards" = mean(PassYards),
            "RiskPassYards" = sd(PassYards),
            "AvgPassTDs" = mean(PassingTouchdowns),
            "RiskPassTDs" = sd(PassingTouchdowns),
            "MeanInts" = mean(Interceptions),
            "meanRushYards" = mean(RushYards),
            "RiskRushYards" = sd(RushYards),
            "AvgRushTDs" = mean(RushingTouchdowns),
            "RiskRushTDs" = sd(RushingTouchdowns),
            "meanFG" = mean(FGMade)
  )


#Aggregate Def Season Stats For Entire NFL
OffNFLStats <- OffWeeklyStats %>% group_by(season) %>%
  summarise("MedPassYards" = mean(PassYards),
            "RiskPassYards" = sd(PassYards),
            "AvgPassTDs" = mean(PassingTouchdowns),
            "RiskPassTDs" = sd(PassingTouchdowns),
            "MeanInts" = mean(Interceptions),
            "meanRushYards" = mean(RushYards),
            "RiskRushYards" = sd(RushYards),
            "AvgRushTDs" = mean(RushingTouchdowns),
            "RiskRushTDs" = sd(RushingTouchdowns),
            "meanFG" = mean(FGMade)
  )

view(OffNFLStats)

#join nfl season stats to team season stats
OffCalcs <- inner_join(OffSeasonStats, OffNFLStats, by = "season")

#Calculate Quotients/Multipliers
OffCalcs <- OffCalcs %>% mutate(
  "PYQuotient" = MedPassYards.x/MedPassYards.y,
  "PYRiskQuotient" = RiskPassYards.x/RiskPassYards.y,
  "PTDQuotient" = AvgPassTDs.x/AvgPassTDs.y,
  "PTDRiskQuotient" = RiskPassTDs.x/RiskPassTDs.y,
  "RYQuotient" = meanRushYards.x/meanRushYards.y,
  "RYRiskQuotient" = RiskRushYards.x/RiskRushYards.y,
  "RTDQuotient" = AvgRushTDs.x/AvgRushTDs.y,
  "RTDRiskQuotient" = RiskRushTDs.x/RiskRushTDs.y,
  "FGQuotient" = meanFG.x/meanFG.y,
  "IntQuotient" = MeanInts.x/MeanInts.y)

#Select Multipliers To Their Own df
OffMultipliers <- OffCalcs %>% 
  select(posteam, season, PYQuotient,PYRiskQuotient,PTDQuotient, PTDRiskQuotient,IntQuotient,RYQuotient,RYRiskQuotient,RTDQuotient,RTDRiskQuotient,FGQuotient) %>%
  filter(season >= EndSeason)

view(OffMultipliers)
view(Multipliers)
view(meanStatsSeason)
view(meanStatsOpponent)
view(OffWeeklyStats)
view(DefWeeklyStats)


write.csv(OffMultipliers, "XYZ.csv")
write.csv(OffSeasonStats,"XYZ.csv")
write.csv(Multipliers, "XYZ.csv")
write.csv(meanStatsSeason, "XYZ.csv")
write.csv(meanStatsOpponent, "XYZ.csv")
write.csv(OffWeeklyStats, "XYZ.csv")
write.csv(DefWeeklyStats, "XYZ.csv")
write.csv(DefNFLStats, "XYZ.csv")
write.csv(schedules, "XYZ.csv")

View(Calcs)
View(Multipliers)
View(OffCalcs)
View(OffMultipliers)






