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

#Grab Data For Analysis
PlayerStats <- load_player_stats(seasons = TRUE)
schedules <- load_schedules(seasons = TRUE)
pbp <- load_pbp(2019:2024)

#Inputs
Focus_Team <- 'HOU'
StartSeason <- 2000
EndSeason <- 2024
CurrentWeek <- 15

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
sum(case when play_type like 'run' then yards_gained else 0 end) as 'RushYards',
sum(case when play_type like 'run' then 1 else 0 end) as 'RushAttempts',
sum(case when play_type like 'run' then touchdown else 0 end) as 'RushingTouchdowns'


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


#Median Stats For the Player By Season
MedianStatsSeason <- PlayerStatsWOpponents %>% filter(recent_team == Focus_Team, season == EndSeason) %>%
  
  group_by(player_display_name, season,recent_team, position_group) %>% summarise("MedPassYards" = median(passing_yards),
                                                                                  "SDPassYards" = sd(passing_yards),
                                                                                  "MedCompletions" = median(completions),
                                                                                  "AvgpassTDs" = mean(passing_tds),
                                                                                  "MeanPassEPA" = mean(passing_epa),
                                                                                  "MedRushYards" = median(rushing_yards),
                                                                                  "SDRushYards" = sd(rushing_yards),
                                                                                  "AvgRushingTds" = mean(rushing_tds),
                                                                                  "AvgRushingEPA" = mean(rushing_epa),
                                                                                  "MedRecYds" = median(receiving_yards),
                                                                                  "SDRecYds" = sd(receiving_yards),
                                                                                  "AvgRecTds" = mean(receiving_tds),
                                                                                  "AvgRecEPA" = mean(receiving_epa),
                                                                                  "MedReceptions" = median(receptions))

View(MedianStatsSeason)
#Grab Team Of Focus Player
FocusTeam <- PlayerStats %>% select(recent_team, week, season) %>% 
                    filter(week == CurrentWeek-1, season == EndSeason, recent_team == Focus_Team)



#Grab Opponent
Opponent <- sqldf("

Select distinct

case when (DG.home_team = FT2.recent_team) then DG.away_team else DG.home_team end as 'Opponent'

from DistinctGames DG

join FocusTeam FT
on FT.week+1 = DG.week and DG.season = FT.season
left join FocusTeam FT2
on FT2.recent_team = DG.home_team
left join FocusTeam FT3
on FT3.recent_team = DG.away_team

where 1=1

and (FT2.recent_team is not null or FT3.recent_team is not null)


")

#Aggregate Def Season Stats By Team
DefSeasonStats <- DefWeeklyStats %>% group_by(season, defteam) %>%
                    summarise("MedPassYards" = median(PassYards),
                              "RiskPassYards" = sd(PassYards),
                              "AvgPassTDs" = mean(PassingTouchdowns),
                              "RiskPassTDs" = sd(PassingTouchdowns),
                              "MedianRushYards" = median(RushYards),
                              "RiskRushYards" = sd(RushYards),
                              "AvgRushTDs" = mean(RushingTouchdowns),
                              "RiskRushTDs" = sd(RushingTouchdowns)
                              )
View(DefSeasonStats)


#Aggregate Def Season Stats For Entire NFL
DefNFLStats <- DefWeeklyStats %>% group_by(season) %>%
  summarise("MedPassYards" = median(PassYards),
            "RiskPassYards" = sd(PassYards),
            "AvgPassTDs" = mean(PassingTouchdowns),
            "RiskPassTDs" = sd(PassingTouchdowns),
            "MedianRushYards" = median(RushYards),
            "RiskRushYards" = sd(RushYards),
            "AvgRushTDs" = mean(RushingTouchdowns),
            "RiskRushTDs" = sd(RushingTouchdowns)
  )

#join nfl season stats to team season stats
Calcs <- inner_join(DefSeasonStats, DefNFLStats, by = "season")
View(Calcs)

#Calculate Quotients/Multipliers
Calcs <- Calcs %>% mutate(
                            "PYQuotient" = MedPassYards.x/MedPassYards.y,
                            "PYRiskQuotient" = RiskPassYards.x/RiskPassYards.y,
                            "PTDQuotient" = AvgPassTDs.x/AvgPassTDs.y,
                            "PTDRiskQuotient" = RiskPassTDs.x/RiskPassTDs.y,
                            "RYQuotient" = MedianRushYards.x/MedianRushYards.y,
                            "RYRiskQuotient" = RiskRushYards.x/RiskRushYards.y,
                            "RTDQuotient" = AvgRushTDs.x/AvgRushTDs.y,
                            "RTDRiskQuotient" = RiskRushTDs.x/RiskRushTDs.y)

#Select Multipliers To Their Own df
Multipliers <- Calcs %>% 
                select(defteam, season, PYQuotient,PYRiskQuotient,PTDQuotient, PTDRiskQuotient,RYQuotient,RYRiskQuotient,RTDQuotient,RTDRiskQuotient) %>%
                filter(season == EndSeason)

View(Multipliers)

#Grab Opponent Multipliers
OpponentStats <- inner_join(Multipliers, Opponent, by = c("defteam" = "Opponent"))

#Projection For All Stats

Projection <- sqldf("
                    
                    Select
                    
                    MS.player_display_name,
                    MS.MedRecYds,
                    O.PYQuotient,
                    MS.SDRecYds,
                    MS.MedRecYds * O.PYQuotient as 'RecYdsProj',
                    MS.AvgRecTds,
                    MS.AvgRecTds*PTDQuotient as 'RecTDProj',
                    MS.MedRushYards,
                    O.RYQuotient,
                    MS.SDRushYards,
                    MS.MedRushYards * O.RYQuotient 'RushYdsProj',
                    MS.AvgRushingTDs,
                    O.RTDQuotient,
                    MS.AvgRushingTds * O.RTDQuotient as 'RushTDProj',
                    MS.MedPassYards,
                    O.PYQuotient,
                    MS.SDPassYards,
                    MS.MedPassYards * O.PYQuotient as 'PassYdsProj',
                    MS.AvgpassTDs,
                    O.PTDQuotient,
                    MS.AvgpassTDs * O.PTDQuotient as 'PassTDsProj'
                    
                    from MedianStatsSeason MS
                    
                    left join OpponentStats O
                      on 1=1
                      
                    where 1=1
                    
                    
                    
                    
")
View(Projection)
View(MedianStatsSeason)
View(OpponentStats)

