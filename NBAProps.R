###This uses the same packages as NBA Spread Model So Can Load Librarys That Way

#CalculateMaxGamesByTeam
GamesPlayedTeam <- gl %>% group_by(nameTeam) %>% summarise("MaxGames" = max(numberGameTeamSeason))

GL_Filter <- left_join(gl, GamesPlayedTeam, by = "nameTeam")

view(GL_Filter)

#Player/Game Inputs
FocusTeam <- "BOS"
FocusOpponent <- "DAL"

#SeasonStatsByPlayer
SeasonPlayerStats <- gl %>% group_by(namePlayer, slugTeam, nameTeam, yearSeason)%>%
  summarise("medMinutes" = mean(minutes),
            "medPts" = mean(pts),
            "sdPts" = sd(pts),
            "medRebounds" = mean(treb),
            "sdRebounds" = sd(treb),
            "medAssists" = mean(ast),
            "sdAssists" = sd(ast),
            "medSteals" = mean(stl),
            "sdSteals" = sd(stl),
            "medBlocks" = mean(blk),
            "sdBlocks" = sd(blk),
            "med3PointersMade" = mean(fg3m),
            "sd3PointersMade" = sd(fg3m),
            "med2PointersMade" = mean(fg2m),
            "sd2PointersMade" = sd(fg2m))

view(SeasonPlayerStats)

#Player Stats Last 4
SeasonPlayerStatsLast4 <- GL_Filter %>% filter(MaxGames - numberGameTeamSeason <= 4) %>% group_by(namePlayer, slugTeam, nameTeam, yearSeason)%>%
  summarise("medMinutes" = mean(minutes),
            "medPts" = mean(pts),
            "sdPts" = sd(pts),
            "medRebounds" = mean(treb),
            "sdRebounds" = sd(treb),
            "medAssists" = mean(ast),
            "sdAssists" = sd(ast),
            "medSteals" = mean(stl),
            "sdSteals" = sd(stl),
            "medBlocks" = mean(blk),
            "sdBlocks" = sd(blk),
            "med3PointersMade" = mean(fg3m),
            "sd3PointersMade" = sd(fg3m),
            "med2PointersMade" = mean(fg2m),
            "sd2PointersMade" = sd(fg2m))

#Player Stats Last 8
SeasonPlayerStatsLast8 <- GL_Filter %>% filter(MaxGames - numberGameTeamSeason <= 8) %>% group_by(namePlayer, slugTeam, nameTeam, yearSeason)%>%
  summarise("medMinutes" = mean(minutes),
            "medPts" = mean(pts),
            "sdPts" = sd(pts),
            "medRebounds" = mean(treb),
            "sdRebounds" = sd(treb),
            "medAssists" = mean(ast),
            "sdAssists" = sd(ast),
            "medSteals" = mean(stl),
            "sdSteals" = sd(stl),
            "medBlocks" = mean(blk),
            "sdBlocks" = sd(blk),
            "med3PointersMade" = mean(fg3m),
            "sd3PointersMade" = sd(fg3m),
            "med2PointersMade" = mean(fg2m),
            "sd2PointersMade" = sd(fg2m))

#Player Stats Last 16
SeasonPlayerStatsLast16 <- GL_Filter %>% filter(MaxGames - numberGameTeamSeason <= 16) %>% group_by(namePlayer, slugTeam, nameTeam, yearSeason)%>%
  summarise("medMinutes" = mean(minutes),
            "medPts" = mean(pts),
            "sdPts" = sd(pts),
            "medRebounds" = mean(treb),
            "sdRebounds" = sd(treb),
            "medAssists" = mean(ast),
            "sdAssists" = sd(ast),
            "medSteals" = mean(stl),
            "sdSteals" = sd(stl),
            "medBlocks" = mean(blk),
            "sdBlocks" = sd(blk),
            "med3PointersMade" = mean(fg3m),
            "sd3PointersMade" = sd(fg3m),
            "med2PointersMade" = mean(fg2m),
            "sd2PointersMade" = sd(fg2m))

GameTeamStats <- GL_Filter %>% group_by(idGame, numberGameTeamSeason, MaxGames, slugTeam, slugOpponent, yearSeason)%>%
  summarise("Minutes" = sum(minutes),
            "Pts" = sum(pts),
            "Rebounds" = sum(treb),
            "Assists" = sum(ast),
            "Steals" = sum(stl),
            "Blocks" = sum(blk),
            "ThreePointersMade" = sum(fg3m),
            "TwoPointersMade" = sum(fg2m))

GameSeasonOffStats4 <- GameTeamStats %>% filter(MaxGames - numberGameTeamSeason <= 4) %>% group_by(slugTeam, yearSeason)%>%
  summarise("medMinutes" = mean(Minutes),
            "medPts" = mean(Pts),
            "medRebounds" = mean(Rebounds),
            "medAssists" = mean(Assists),
           "medSteals" = mean(Steals),
            "medBlocks" = mean(Blocks),
          "med3PointersMade" = mean(ThreePointersMade),
            "med2PointersMade" = mean(TwoPointersMade))



GameSeasonOffStats8 <- GameTeamStats %>% filter(MaxGames - numberGameTeamSeason <= 8) %>% group_by(slugTeam, yearSeason)%>%
  summarise("medMinutes" = mean(Minutes),
            "medPts" = mean(Pts),
            "medRebounds" = mean(Rebounds),
            "medAssists" = mean(Assists),
            "medSteals" = mean(Steals),
            "medBlocks" = mean(Blocks),
            "med3PointersMade" = mean(ThreePointersMade),
            "med2PointersMade" = mean(TwoPointersMade))



GameSeasonOffStats16 <- GameTeamStats %>% filter(MaxGames - numberGameTeamSeason <= 16) %>% group_by(slugTeam, yearSeason)%>%
  summarise("medMinutes" = mean(Minutes),
            "medPts" = mean(Pts),
            "medRebounds" = mean(Rebounds),
            "medAssists" = mean(Assists),
            "medSteals" = mean(Steals),
            "medBlocks" = mean(Blocks),
            "med3PointersMade" = mean(ThreePointersMade),
            "med2PointersMade" = mean(TwoPointersMade))



GameSeasonOffStats <- GameTeamStats %>% group_by(slugTeam, yearSeason)%>%
  summarise("medMinutes" = mean(Minutes),
            "medPts" = mean(Pts),
            "medRebounds" = mean(Rebounds),
            "medAssists" = mean(Assists),
            "medSteals" = mean(Steals),
            "medBlocks" = mean(Blocks),
            "med3PointersMade" = mean(ThreePointersMade),
            "med2PointersMade" = mean(TwoPointersMade))



#DefensiveStatsSeason
GameSeasonDefStats <- GameTeamStats %>% group_by(slugOpponent, yearSeason)%>%
  summarise("medMinutes" = mean(Minutes),
            "medPts" = mean(Pts),
            "sdPts" = sd(Pts),
            "medRebounds" = mean(Rebounds),
            "sdRebounds" = sd(Rebounds),
            "medAssists" = mean(Assists),
            "sdAssists" = sd(Assists),
            "medSteals" = mean(Steals),
            "sdSteals" = sd(Steals),
            "medBlocks" = mean(Blocks),
            "sdBlocks" = sd(Blocks),
            "med3PointersMade" = mean(ThreePointersMade),
            "sd3PointersMade" = sd(ThreePointersMade),
            "med2PointersMade" = mean(TwoPointersMade),
            "sd2PointersMade" = sd(TwoPointersMade))

#DefensiveStatsLast4
GameSeasonDefStats4 <- GameTeamStats %>% filter(MaxGames - numberGameTeamSeason <= 4) %>% group_by(slugOpponent, yearSeason)%>%
  summarise("medMinutes" = mean(Minutes),
            "medPts" = mean(Pts),
            "sdPts" = sd(Pts),
            "medRebounds" = mean(Rebounds),
            "sdRebounds" = sd(Rebounds),
            "medAssists" = mean(Assists),
            "sdAssists" = sd(Assists),
            "medSteals" = mean(Steals),
            "sdSteals" = sd(Steals),
            "medBlocks" = mean(Blocks),
            "sdBlocks" = sd(Blocks),
            "med3PointersMade" = mean(ThreePointersMade),
            "sd3PointersMade" = sd(ThreePointersMade),
            "med2PointersMade" = mean(TwoPointersMade),
            "sd2PointersMade" = sd(TwoPointersMade))

#DefensiveStatsLast8
GameSeasonDefStats8 <- GameTeamStats %>% filter(MaxGames - numberGameTeamSeason <= 8) %>% group_by(slugOpponent, yearSeason)%>%
  summarise("medMinutes" = mean(Minutes),
            "medPts" = mean(Pts),
            "sdPts" = sd(Pts),
            "medRebounds" = mean(Rebounds),
            "sdRebounds" = sd(Rebounds),
            "medAssists" = mean(Assists),
            "sdAssists" = sd(Assists),
            "medSteals" = mean(Steals),
            "sdSteals" = sd(Steals),
            "medBlocks" = mean(Blocks),
            "sdBlocks" = sd(Blocks),
            "med3PointersMade" = mean(ThreePointersMade),
            "sd3PointersMade" = sd(ThreePointersMade),
            "med2PointersMade" = mean(TwoPointersMade),
            "sd2PointersMade" = sd(TwoPointersMade))

#DefensiveStatsLast16
GameSeasonDefStats16 <- GameTeamStats %>% filter(MaxGames - numberGameTeamSeason <= 16) %>% group_by(slugOpponent, yearSeason)%>%
  summarise("medMinutes" = mean(Minutes),
            "medPts" = mean(Pts),
            "sdPts" = sd(Pts),
            "medRebounds" = mean(Rebounds),
            "sdRebounds" = sd(Rebounds),
            "medAssists" = mean(Assists),
            "sdAssists" = sd(Assists),
            "medSteals" = mean(Steals),
            "sdSteals" = sd(Steals),
            "medBlocks" = mean(Blocks),
            "sdBlocks" = sd(Blocks),
            "med3PointersMade" = mean(ThreePointersMade),
            "sd3PointersMade" = sd(ThreePointersMade),
            "med2PointersMade" = mean(TwoPointersMade),
            "sd2PointersMade" = sd(TwoPointersMade))

#LeagueDefensiveStatsSeason
NBADefStats <- GameTeamStats %>% group_by(yearSeason) %>%
  summarise("medMinutes" = mean(Minutes),
            "medPts" = mean(Pts),
            "sdPts" = sd(Pts),
            "medRebounds" = mean(Rebounds),
            "sdRebounds" = sd(Rebounds),
            "medAssists" = mean(Assists),
            "sdAssists" = sd(Assists),
            "medSteals" = mean(Steals),
            "sdSteals" = sd(Steals),
            "medBlocks" = mean(Blocks),
            "sdBlocks" = sd(Blocks),
            "med3PointersMade" = mean(ThreePointersMade),
            "sd3PointersMade" = sd(ThreePointersMade),
            "med2PointersMade" = mean(TwoPointersMade),
            "sd2PointersMade" = sd(TwoPointersMade))

#LeagueDefensiveStatsLast4
NBADefStats4 <- GameTeamStats %>% filter(MaxGames - numberGameTeamSeason <= 4) %>% group_by(yearSeason) %>%
  summarise("medMinutes" = mean(Minutes),
            "medPts" = mean(Pts),
            "sdPts" = sd(Pts),
            "medRebounds" = mean(Rebounds),
            "sdRebounds" = sd(Rebounds),
            "medAssists" = mean(Assists),
            "sdAssists" = sd(Assists),
            "medSteals" = mean(Steals),
            "sdSteals" = sd(Steals),
            "medBlocks" = mean(Blocks),
            "sdBlocks" = sd(Blocks),
            "med3PointersMade" = mean(ThreePointersMade),
            "sd3PointersMade" = sd(ThreePointersMade),
            "med2PointersMade" = mean(TwoPointersMade),
            "sd2PointersMade" = sd(TwoPointersMade))

#LeagueDefensiveStatsLast8
NBADefStats8 <- GameTeamStats %>% filter(MaxGames - numberGameTeamSeason <= 8) %>% group_by(yearSeason) %>%
  summarise("medMinutes" = mean(Minutes),
            "medPts" = mean(Pts),
            "sdPts" = sd(Pts),
            "medRebounds" = mean(Rebounds),
            "sdRebounds" = sd(Rebounds),
            "medAssists" = mean(Assists),
            "sdAssists" = sd(Assists),
            "medSteals" = mean(Steals),
            "sdSteals" = sd(Steals),
            "medBlocks" = mean(Blocks),
            "sdBlocks" = sd(Blocks),
            "med3PointersMade" = mean(ThreePointersMade),
            "sd3PointersMade" = sd(ThreePointersMade),
            "med2PointersMade" = mean(TwoPointersMade),
            "sd2PointersMade" = sd(TwoPointersMade))

#LeagueDefensiveStatsLast16
NBADefStats16 <- GameTeamStats %>% filter(MaxGames - numberGameTeamSeason <= 16) %>% group_by(yearSeason) %>%
  summarise("medMinutes" = mean(Minutes),
            "medPts" = mean(Pts),
            "sdPts" = sd(Pts),
            "medRebounds" = mean(Rebounds),
            "sdRebounds" = sd(Rebounds),
            "medAssists" = mean(Assists),
            "sdAssists" = sd(Assists),
            "medSteals" = mean(Steals),
            "sdSteals" = sd(Steals),
            "medBlocks" = mean(Blocks),
            "sdBlocks" = sd(Blocks),
            "med3PointersMade" = mean(ThreePointersMade),
            "sd3PointersMade" = sd(ThreePointersMade),
            "med2PointersMade" = mean(TwoPointersMade),
            "sd2PointersMade" = sd(TwoPointersMade))

view(NBADefStats)
view(NBADefStats8)
view(NBADefStats16)
view(NBADefStats4)

#CreateMultiplierDataSets
DefMultipliersSeason <- left_join(GameSeasonDefStats, NBADefStats, by = "yearSeason")
DefMultipliersSeason4 <- left_join(GameSeasonDefStats4, NBADefStats4, by = "yearSeason")
DefMultipliersSeason8 <- left_join(GameSeasonDefStats8, NBADefStats8, by = "yearSeason")
DefMultipliersSeason16 <- left_join(GameSeasonDefStats16, NBADefStats16, by = "yearSeason")


DefMultipliers <- DefMultipliersSeason %>% group_by(slugOpponent) %>%
  summarise("PtsQuotient" = medPts.x/medPts.y,
            "ReboundsQuotient" = medRebounds.x/medRebounds.y,
            "AssistsQuotient" = medAssists.x/medAssists.y,
            "StealsQuotient" = medSteals.x/medSteals.y,
            "BlocksQuotient" = medBlocks.x/medBlocks.y,
            "ThreeQuotient" = med3PointersMade.x/med3PointersMade.y,
            "TwoQuotient" = med2PointersMade.x/med2PointersMade.y) %>% mutate("TimeFrame" = "Season")

DefMultipliers4 <- DefMultipliersSeason4 %>% group_by(slugOpponent) %>%
  summarise("PtsQuotient" = medPts.x/medPts.y,
            "ReboundsQuotient" = medRebounds.x/medRebounds.y,
            "AssistsQuotient" = medAssists.x/medAssists.y,
            "StealsQuotient" = medSteals.x/medSteals.y,
            "BlocksQuotient" = medBlocks.x/medBlocks.y,
            "ThreeQuotient" = med3PointersMade.x/med3PointersMade.y,
            "TwoQuotient" = med2PointersMade.x/med2PointersMade.y) %>% mutate("TimeFrame" = "Last4")

DefMultipliers8 <- DefMultipliersSeason8 %>% group_by(slugOpponent) %>%
  summarise("PtsQuotient" = medPts.x/medPts.y,
            "ReboundsQuotient" = medRebounds.x/medRebounds.y,
            "AssistsQuotient" = medAssists.x/medAssists.y,
            "StealsQuotient" = medSteals.x/medSteals.y,
            "BlocksQuotient" = medBlocks.x/medBlocks.y,
            "ThreeQuotient" = med3PointersMade.x/med3PointersMade.y,
            "TwoQuotient" = med2PointersMade.x/med2PointersMade.y) %>% mutate("TimeFrame" = "Last8")

DefMultipliers16 <- DefMultipliersSeason16 %>% group_by(slugOpponent) %>%
  summarise("PtsQuotient" = medPts.x/medPts.y,
            "ReboundsQuotient" = medRebounds.x/medRebounds.y,
            "AssistsQuotient" = medAssists.x/medAssists.y,
            "StealsQuotient" = medSteals.x/medSteals.y,
            "BlocksQuotient" = medBlocks.x/medBlocks.y,
            "ThreeQuotient" = med3PointersMade.x/med3PointersMade.y,
            "TwoQuotient" = med2PointersMade.x/med2PointersMade.y) %>% mutate("TimeFrame" = "Last16")

Multipliers1 <- union_all(DefMultipliers,DefMultipliers4)
Multipliers2 <- union_all(Multipliers1,DefMultipliers8)
AggregateMultipliers <- union_all(Multipliers2, DefMultipliers16)

View(AggregateMultipliers)

#CreateMultiplierDataSets
OffMultipliersSeason <- left_join(GameSeasonOffStats, NBADefStats, by = "yearSeason")
OffMultipliersSeason4 <- left_join(GameSeasonOffStats4, NBADefStats4, by = "yearSeason")
OffMultipliersSeason8 <- left_join(GameSeasonOffStats8, NBADefStats8, by = "yearSeason")
OffMultipliersSeason16 <- left_join(GameSeasonOffStats16, NBADefStats16, by = "yearSeason")


OffMultipliers <- OffMultipliersSeason %>% group_by(slugTeam) %>%
  summarise("PtsQuotient" = medPts.x/medPts.y,
            "ReboundsQuotient" = medRebounds.x/medRebounds.y,
            "AssistsQuotient" = medAssists.x/medAssists.y,
            "StealsQuotient" = medSteals.x/medSteals.y,
            "BlocksQuotient" = medBlocks.x/medBlocks.y,
            "ThreeQuotient" = med3PointersMade.x/med3PointersMade.y,
            "TwoQuotient" = med2PointersMade.x/med2PointersMade.y) %>% mutate("TimeFrame" = "Season")

OffMultipliers4 <- OffMultipliersSeason4 %>% group_by(slugTeam) %>%
  summarise("PtsQuotient" = medPts.x/medPts.y,
            "ReboundsQuotient" = medRebounds.x/medRebounds.y,
            "AssistsQuotient" = medAssists.x/medAssists.y,
            "StealsQuotient" = medSteals.x/medSteals.y,
            "BlocksQuotient" = medBlocks.x/medBlocks.y,
            "ThreeQuotient" = med3PointersMade.x/med3PointersMade.y,
            "TwoQuotient" = med2PointersMade.x/med2PointersMade.y) %>% mutate("TimeFrame" = "Last4")

OffMultipliers8 <- OffMultipliersSeason8 %>% group_by(slugTeam) %>%
  summarise("PtsQuotient" = medPts.x/medPts.y,
            "ReboundsQuotient" = medRebounds.x/medRebounds.y,
            "AssistsQuotient" = medAssists.x/medAssists.y,
            "StealsQuotient" = medSteals.x/medSteals.y,
            "BlocksQuotient" = medBlocks.x/medBlocks.y,
            "ThreeQuotient" = med3PointersMade.x/med3PointersMade.y,
            "TwoQuotient" = med2PointersMade.x/med2PointersMade.y) %>% mutate("TimeFrame" = "Last8")

OffMultipliers16 <- OffMultipliersSeason16 %>% group_by(slugTeam) %>%
  summarise("PtsQuotient" = medPts.x/medPts.y,
            "ReboundsQuotient" = medRebounds.x/medRebounds.y,
            "AssistsQuotient" = medAssists.x/medAssists.y,
            "StealsQuotient" = medSteals.x/medSteals.y,
            "BlocksQuotient" = medBlocks.x/medBlocks.y,
            "ThreeQuotient" = med3PointersMade.x/med3PointersMade.y,
            "TwoQuotient" = med2PointersMade.x/med2PointersMade.y) %>% mutate("TimeFrame" = "Last16")

OffMultipliers1 <- union_all(OffMultipliers,OffMultipliers4)
OffMultipliers2 <- union_all(OffMultipliers1,OffMultipliers8)
OffAggregateMultipliers <- union_all(OffMultipliers2, OffMultipliers16)

View(OffAggregateMultipliers)

#FocusTeamSeason
FocusPlayerStats <- SeasonPlayerStats %>% filter(slugTeam == FocusTeam) %>%
  mutate("TimeFrame" = "Season")

#FocusTeamLast4
FocusPlayerStats4 <- SeasonPlayerStatsLast4 %>% filter(slugTeam == FocusTeam) %>%
  mutate("TimeFrame" = "Last4")

#FocusTeamLast8
FocusPlayerStats8 <- SeasonPlayerStatsLast8 %>% filter(slugTeam == FocusTeam) %>%
  mutate("TimeFrame" = "Last8")

#FocusTeamLast16
FocusPlayerStats16 <- SeasonPlayerStatsLast16 %>% filter(slugTeam == FocusTeam) %>%
  mutate("TimeFrame" = "Last16")

#FocusOpponentSeason
OpponentStats <- DefMultipliers %>% filter(slugOpponent == FocusOpponent) %>%
  mutate("TimeFrame" = "Season")

#FocusOpponentLast4
OpponentStats4 <- DefMultipliers4 %>% filter(slugOpponent == FocusOpponent) %>%
  mutate("TimeFrame" = "Last4")

#FocusOpponentLast8
OpponentStats8 <- DefMultipliers8 %>% filter(slugOpponent == FocusOpponent) %>%
  mutate("TimeFrame" = "Last8")

#FocusOpponentLast16
OpponentStats16 <- DefMultipliers16 %>% filter(slugOpponent == FocusOpponent) %>%
  mutate("TimeFrame" = "Last16")

#AggregatePlayerData
PlayerStatsAggregate1 <- union_all(FocusPlayerStats,FocusPlayerStats4)
PlayerStatsAggregate2 <- union_all(PlayerStatsAggregate1,FocusPlayerStats8)
AggregatePlayerStats <- union_all(PlayerStatsAggregate2,FocusPlayerStats16)

view(AggregatePlayerStats)

#AggregateOpponentData
OpponentStatsAggregate1 <- union_all(OpponentStats,OpponentStats4)
OpponentStatsAggregate2 <- union_all(OpponentStatsAggregate1,OpponentStats8)
AggregateOpponentStats <- union_all(OpponentStatsAggregate2,OpponentStats16)

View(AggregateOpponentStats)


#CalculateProjections
Projections <- sqldf("
                     
                     Select
                     
                     
                     FP.namePlayer,
                     FP.TimeFrame,
                     FP.slugTeam,
                     FP.medMinutes,
                     FP.medPts,
                     FP.sdPts,
                     FP.medPts*O.PtsQuotient as 'ProjPts',
                     FP.medRebounds,
                     FP.sdRebounds,
                     FP.medRebounds*O.ReboundsQuotient as 'ProjRebounds',
                     FP.medAssists,
                     FP.sdAssists,
                     FP.medAssists*O.AssistsQuotient as 'ProjAssists',
                     FP.medSteals,
                     FP.sdSteals,
                     FP.medSteals*O.StealsQuotient as 'ProjSteals',
                     FP.medBlocks,
                     FP.sdBlocks,
                     FP.medBlocks*O.BlocksQuotient as 'ProjBlocks',
                     FP.med3PointersMade,
                     FP.sd3PointersMade,
                     FP.med3PointersMade*O.ThreeQuotient as 'Proj3pts',
                     FP.med2PointersMade,
                     FP.sd2PointersMade,
                     FP.med2PointersMade*O.TwoQuotient as 'Proj2pts'
                     
                     
                     from AggregatePlayerStats FP
                     
                     left join AggregateOpponentStats O
                      on FP.TimeFrame = O.TimeFrame
                     
                     
                     ")

View(Projections)


#FocusTeamSeason
OpponentPlayerStats <- SeasonPlayerStats %>% filter(slugTeam == FocusOpponent) %>%
  mutate("TimeFrame" = "Season")

#FocusTeamLast4
OpponentPlayerStats4 <- SeasonPlayerStatsLast4 %>% filter(slugTeam == FocusOpponent) %>%
  mutate("TimeFrame" = "Last4")

#FocusTeamLast8
OpponentPlayerStats8 <- SeasonPlayerStatsLast8 %>% filter(slugTeam == FocusOpponent) %>%
  mutate("TimeFrame" = "Last8")

#FocusTeamLast16
OpponentPlayerStats16 <- SeasonPlayerStatsLast16 %>% filter(slugTeam == FocusOpponent) %>%
  mutate("TimeFrame" = "Last16")

#FocusOpponentSeason
FocusDef <- DefMultipliers %>% filter(slugOpponent == FocusTeam) %>%
  mutate("TimeFrame" = "Season")

#FocusOpponentLast4
FocusDef4 <- DefMultipliers4 %>% filter(slugOpponent == FocusTeam) %>%
  mutate("TimeFrame" = "Last4")

#FocusOpponentLast8
FocusDef8<- DefMultipliers8 %>% filter(slugOpponent == FocusTeam) %>%
  mutate("TimeFrame" = "Last8")

#FocusOpponentLast16
FocusDef16 <- DefMultipliers16 %>% filter(slugOpponent == FocusTeam) %>%
  mutate("TimeFrame" = "Last16")

#AggregatePlayerData
PlayerStatsAggregate3 <- union_all(OpponentPlayerStats,OpponentPlayerStats4)
PlayerStatsAggregate4 <- union_all(PlayerStatsAggregate3,OpponentPlayerStats8)
AggregatePlayerStats2 <- union_all(PlayerStatsAggregate4,OpponentPlayerStats16)

View(AggregatePlayerStats2)

#AggregateOpponentData
OpponentStatsAggregate3 <- union_all(FocusDef,FocusDef4)
OpponentStatsAggregate4 <- union_all(OpponentStatsAggregate3,FocusDef8)
AggregateOpponentStats2 <- union_all(OpponentStatsAggregate4,FocusDef16)

View(AggregateOpponentStats2)


#CalculateProjections
Projections2 <- sqldf("
                     
                     Select
                     
                     
                     FP.namePlayer,
                     FP.TimeFrame,
                     FP.slugTeam,
                     FP.medMinutes,
                     FP.medPts,
                     FP.sdPts,
                     FP.medPts*O.PtsQuotient as 'ProjPts',
                     FP.medRebounds,
                     FP.sdRebounds,
                     FP.medRebounds*O.ReboundsQuotient as 'ProjRebounds',
                     FP.medAssists,
                     FP.sdAssists,
                     FP.medAssists*O.AssistsQuotient as 'ProjAssists',
                     FP.medSteals,
                     FP.sdSteals,
                     FP.medSteals*O.StealsQuotient as 'ProjSteals',
                     FP.medBlocks,
                     FP.sdBlocks,
                     FP.medBlocks*O.BlocksQuotient as 'ProjBlocks',
                     FP.med3PointersMade,
                     FP.sd3PointersMade,
                     FP.med3PointersMade*O.ThreeQuotient as 'Proj3pts',
                     FP.med2PointersMade,
                     FP.sd2PointersMade,
                     FP.med2PointersMade*O.TwoQuotient as 'Proj2pts'
                     
                     
                     from AggregatePlayerStats2 FP
                     
                     left join AggregateOpponentStats2 O
                      on FP.TimeFrame = O.TimeFrame
                     
                     
                     ")

View(Projections2)
View(gl)




