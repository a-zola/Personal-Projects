###Libraries
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

#InputCurrentWeeks
CurrentWeek <- 20
CurrentSeason <- 2024

###Load Data
schedules <- load_schedules(seasons = TRUE)
pbp <- load_pbp(2019:2024)

View(schedules)

#Convert NA to NULL
pbp <- as.data.frame(pbp)

#WeeklyData

AZ_pbp <- sqldf(" 

Select
P.game_id,
P.posteam as 'offteam',
P.defteam,
P.week,
P.season,
P.spread_line,
avg(epa) as 'AverageEPA',
sum(third_down_converted) as 'ThirdDown',
sum(tackled_for_loss) as 'TL',
sum(case when field_goal_result like 'made' then 1 else 0 end) as 'FGMade'


from pbp P

where 1=1

and play_type in ('pass','run','field_goal')
and P.week >= 7

group by

P.game_id,
P.posteam,
P.defteam,
P.week,
P.season,
P.spread_line

order by

P.game_id,
P.posteam,
P.defteam,
P.week,
P.season,
P.spread_line

")

#Prior 4 weeks Offenses

Off<- AZ_pbp %>% group_by(offteam,season) %>%
  
summarise(
"AverageEPA" = mean(AverageEPA),
"AverageTL" = mean(TL),
"Average3rddown" = mean(ThirdDown),
"AvgFGMade" = mean(FGMade)
  
)

view(Off)

#Prior 4 Week Defenses
Def<- AZ_pbp %>% group_by(defteam,season) %>%
  
  summarise(
    "AverageEPA" = mean(AverageEPA),
    "AverageTL" = mean(TL),
    "Average3rddown" = mean(ThirdDown),
    "AvgFGMade" = mean(FGMade)
    
  )

view(Def)

#grabbing matchups for model
MatchupsWeekly <- sqldf("

Select distinct

game_id,
week,
season,
away_team,
home_team,
away_spread_odds,
home_spread_odds,
spread_line

from schedules

where 1=1

and season >= 2019

                        
                        
")

#aggregate data
Model_Data <- sqldf("
                    
Select

Match.game_id,
Match.week,
Match.season,
Match.away_team,
Match.home_team,
Match.away_spread_odds,
Match.home_spread_odds,
Match.spread_line,
HomeOff.AverageEPA as 'HomeOffEPA',
HomeOff.AverageTL as 'HomeOffTL',
HomeOff.Average3rddown as 'HomeOff3rddown',
AwayOff.AverageEPA as 'AwayOffEPA',
AwayOff.AverageTL as 'AwayOffTL',
AwayOff.Average3rddown as 'AwayOff3rddown',
HomeDef.AverageEPA as 'HomeDefEPA',
HomeDef.AverageTL as 'HomeDefTL',
HomeDef.Average3rddown as 'HomeDef3rddown',
AwayDef.AverageEPA as 'AwayDefEPA',
AwayDef.AverageTL as 'AwayDefTL',
AwayDef.Average3rddown as 'AwayDef3rddown'

from MatchupsWeekly Match

left join Off HomeOff
  on HomeOff.offteam = match.home_team and HomeOff.season = Match.season
left join Off AwayOff
  on AwayOff.offteam = match.away_team and AwayOff.season = Match.season
left join Def HomeDef
  on HomeDef.Defteam = match.home_team and HomeDef.season = Match.season
left join Def AwayDef
  on AwayDef.Defteam = match.away_team and AwayDef.season = Match.season
 
where 1=1
                    
")

View(Model_Data)

#omit NAs
Model_Data <- na.omit(Model_Data)

colnames(Model_Data)

#linear model
Model <- lm(spread_line~
              
            HomeOffEPA+HomeDefEPA+AwayOffEPA+AwayDefEPA, 
              data = Model_Data)

#model Summary
summary(Model)

#model coefficients
Coeffs <- coefficients(Model)

Coeffs <- as.data.frame(t(Coeffs))

colnames(Coeffs)[1] = 'Intercept'

View(Coeffs)

#Grab Current Week
CurrentGames <- schedules %>% select(game_id, home_team, away_team, spread_line, week, season) %>% filter(season == CurrentSeason, week == CurrentWeek)

#HomeTeams
HomeTeams <- CurrentGames %>% select(game_id, home_team, spread_line, week, season)

#ChangeColumnNameForUnionOfTeams
colnames(HomeTeams)[2] = 'Team'

#AwayTeams
AwayTeams <- CurrentGames %>% select (game_id, away_team, spread_line, week,season)

#ChangeColumnNameForUnionOfTeams
colnames(AwayTeams)[2] = 'Team'

#Union of All Teams For the week
AllTeams <- union_all(HomeTeams,AwayTeams)

#Calculate Projected Spread Contributions By Home And Away
CurrentWeekData <- sqldf("
      
      
Select distinct

AT.game_id,
AT.Team,
case when HT.team is not null then C.Intercept else 0 end as 'HomeFlag',
Case when HT.team is not null then C.HomeOffEPA*O.AverageEPA else C.AwayOffEPA*O.AverageEPA end as 'AverageOFFEPAContribution',
Case when HT.team is not null then C.HomeDefEPA*D.AverageEPA else C.AwayDefEPA*D.AverageEPA end as 'AverageDEFEPAContribution'

from AllTeams AT

left join HomeTeams HT
  on HT.team = AT.team
left join Off O
  on O.offteam = AT.team and O.season = AT.season
left join Def D
  on D.defteam = AT.team and D.season = AT.season
left join Coeffs C
  on 1=1

      
      ")

View(CurrentWeekData)


#Sum Contributions To Get Projected Spread
GameSpreads <- sqldf("
                     

Select

CW.game_id,
(sum(CW.HomeFlag)+sum(AverageOFFEPAContribution)+sum(AverageDEFEPAContribution)) as 'Spread',
(sum(AverageOFFEPAContribution)+sum(AverageDEFEPAContribution)) as 'SpreadMinusHF'

from CurrentWeekData CW

group by

CW.game_id

order by

sum(CW.HomeFlag)+sum(AverageOFFEPAContribution)+sum(AverageDEFEPAContribution) asc

")

#View Table Of Spreads
View(GameSpreads)




