library(lubridate)
library(tidyr)
library(dplyr)
library(fitzRoy)
library(stringr)

createSIDataFrame <- function(year) {
  # Grab results from database - pings API
  playerStats <- fetch_player_stats(season = year)
  gameResults <- fetch_results(season = year)
  
  # Clean game results to get relevant columns
  gameResults <- gameResults %>% 
    select(season = match.date,
           homeTeam = match.homeTeam.name, 
           awayTeam = match.awayTeam.name,
           roundNumber = round.roundNumber,
           .homeGoals = homeTeamScore.matchScore.goals,
           .homeBehinds = homeTeamScore.matchScore.behinds,
           .awayGoals = awayTeamScore.matchScore.goals,
           .awayBehinds = awayTeamScore.matchScore.behinds) %>%
    mutate(homeScoringShots = .homeGoals + .homeBehinds,
           awayScoringShots = .awayGoals + .awayBehinds,
           season = as.integer(year(season)))
  
  # Merge the data frame to get each teams' performance each round on separate row
  homeGameResults <- gameResults %>% 
    select(season,
           teamName = homeTeam,
           roundNumber,
           scoringShots = homeScoringShots)
  
  awayGameResults <- gameResults %>% 
    select(season,
           teamName = awayTeam,
           roundNumber,
           scoringShots = awayScoringShots)
  
  gameResults <- rbind(homeGameResults, awayGameResults) %>%
    mutate(teamName = ifelse(teamName == "GWS Giants", "GWS GIANTS", teamName),
           teamName = ifelse(teamName == "Gold Coast Suns", "Gold Coast SUNS", teamName))
  
  # Clean player result data to get relevant columns, get rid of games that
  # haven't happened yet and fix both Josh Kennedys' names
  playerStats <- playerStats %>% 
    select(season = utcStartTime,
           jumperNumber = player.jumperNumber, 
           teamName = team.name, 
           roundNumber = round.roundNumber,
           firstName = player.player.player.givenName, 
           lastName = player.player.player.surname, 
           scoreInvolvements) %>% 
    filter(!is.na(teamName)) %>%
    mutate(season = as.integer(year(season)),
           firstName = str_replace(firstName, "\\s[A-Z]\\.$", "")) %>%
    
    # Find percentage of scores each player is involved in per game, shave off
    # irrelevant rows
    inner_join(gameResults, by = c("season", "roundNumber", "teamName")) %>%
    mutate(scoreInvolvements = 100 * scoreInvolvements / scoringShots) %>%
    select(!c(roundNumber, scoringShots)) %>%
    
    # Summarise data frame to record average of percentage of scores over a whole
    # year plus number of games
    group_by(season, jumperNumber, teamName, firstName, lastName) %>% 
    summarise(scoreInvolvements = mean(scoreInvolvements),
              gamesPlayed = n(),
              .groups = "drop")
  
  return(playerStats)
  
}

standardise <- function(x) {
  (x - mean(x) / sd(x))
}

expand_seasons <- function(season_string) {
  
  parts <- str_split(season_string, ",\\s*")[[1]]
  years <- c()
  
  for (part in parts) {
    if (str_detect(part, "-")) {
      range <- as.numeric(str_split(part, "-")[[1]])
      years <- c(years, seq(range[1], range[2]))
    } else {
      years <- c(years, as.numeric(part))
    }
  }
  
  return(years)
}

createPositionDataFrame<- function(year) {
  
  playerYearStats <- fetch_player_stats_afltables(year)
  playerHeights <- read.csv("playerheights.csv") %>%
    mutate(seasons = lapply(seasons, expand_seasons)) %>%
    filter(sapply(seasons, function(x) any(x == year)))
  
  playerYearStats <- playerYearStats %>% 
    
    # Use only relevant stats
    select(.season = Season,
           .firstName = First.name,
           .lastName = Surname,
           .teamName = Playing.for,
           Marks,
           marksInside50 = Marks.Inside.50,
           Goals,
           Behinds,
           hitOuts = Hit.Outs,
           tackles = Tackles,
           rebound50s = Rebounds,
           inside50s = Inside.50s,
           clearances = Clearances,
           Contested.Possessions,
           Uncontested.Possessions,
           onePercenters = One.Percenters,
           goalAssists = Goal.Assists,
           timeOnGround = Time.on.Ground) %>%
    
    # Engineer all relevant columns
    mutate(.season = as.integer(.season),
           marksOutside50 = Marks - marksInside50,
           scoringShots = Goals + Behinds,
           nonClearanceContPos = Contested.Possessions - clearances,
           otherPossessions = (Uncontested.Possessions 
                               - clearances 
                               - inside50s 
                               - scoringShots 
                               - goalAssists),
           timeOnGround = as.integer(timeOnGround)) %>%
    
    # Delete unneeded rows
    select(!c(Marks, 
              Goals, 
              Behinds, 
              Contested.Possessions, 
              Uncontested.Possessions)) %>%
    
    # Average stats for each player per season
    group_by(across(starts_with("."))) %>%
    summarise(across(everything(), mean), .groups = "drop") %>%
    
    # Fix temporary names for consistency
    rename(season = .season,
           firstName = .firstName,
           lastName = .lastName,
           teamName = .teamName) %>%
    
    mutate(across(c(marksInside50,
                    hitOuts,
                    tackles,
                    rebound50s,
                    inside50s,
                    clearances,
                    onePercenters,
                    goalAssists,
                    marksOutside50,
                    scoringShots,
                    nonClearanceContPos,
                    otherPossessions),
                  ~ . * 100 / timeOnGround)) %>%
    
    select(!c(timeOnGround)) %>%
    
    # Fixing errors in the scraped data to align the playeryearStats data frame with
    # the playerDetails data frame
    mutate(teamName = if_else(teamName == "Greater Western Sydney", "GWS", teamName),
           firstName = if_else(firstName == "Heber", "Hewago", firstName),
           lastName = if_else(lastName == "Quinton", "Oea", lastName),
           firstName = if_else(lastName == "Capuano", "Mathew", firstName),
           firstName = if_else(lastName == "Ryder", "Paddy", firstName),
           firstName = if_else(lastName == "de Boer", "Matt", firstName),
           firstName = if_else(lastName == "Sutcliffe", "Cam", firstName),
           lastName = if_else(lastName == "Litherland", "Dewar", lastName),
           lastName = if_else(lastName == "Kennedy-Harris", "Kennedy Harris", lastName),
           lastName = if_else(lastName == "MacPherson", "Macpherson", lastName),
           firstName = if_else(firstName == "Harrison" & lastName == "Himmelberg",
                               "Harry", firstName)) %>%
    
    left_join(unnest(playerHeights, seasons), by = c("firstName", 
                                                     "lastName", 
                                                     "teamName", 
                                                     "season" = "seasons"))
  
  statCols <- setdiff(names(playerYearStats), c("season", "firstName", "lastName", "teamName"))
  
  playerYearStats <- playerYearStats %>%
    
    group_by(season) %>%
    mutate(across(all_of(statCols), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
    mutate(across(all_of(statCols), ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE))) %>%
    ungroup() %>%
    # Fix apostrophes in names (in AFLTables, e.g. OConnor to O'Connor) 
    mutate(lastName = str_replace(lastName, "^([A-Z])([A-Z])", "\\1'\\2"))
  
  posFinderStats <- playerYearStats %>%
    select(all_of(statCols))
  
  medoids <- read.csv("positionmedoids.csv")
  
  positions <- apply(posFinderStats, 1, function(playerYear) {
    posDistance <- colSums((t(medoids) - playerYear)^2)
    return(which.min(posDistance))
  })
  
  playerYearStats$position <- positions
  playerYearStats <- playerYearStats %>%
    select(-all_of(statCols))
  
  return(playerYearStats)
  
}

updateDataFile <- function(dataFile, createDataFrameFunction) {
  
  oldDataFrame <- read.csv(dataFile)
  updateDataFrame <- data.frame()
  
  updateStart <- max(oldDataFrame$season)
  updateEnd <- as.integer(year(Sys.time()))
  oldDataFrame <- oldDataFrame %>%
    filter(season < updateStart)
  
  for (year in updateStart:updateEnd) {
    seasonData <- createDataFrameFunction(year)
    updateDataFrame <- rbind(updateDataFrame, seasonData)
  }
  
  updateDataFrame <- rbind(oldDataFrame, updateDataFrame)
  write.csv(updateDataFrame, dataFile, row.names = FALSE)
  
}

createHeightsDataFrame <- function() {
  
  heightData <- fetch_player_details_afltables() %>%
    select(.player = Player,
           teamName = Team,
           height = HT,
           seasons = Seasons) %>%
    separate(.player, 
             into = c("firstName", "lastName1", "lastName2", "lastName3"), 
             sep = " ", 
             fill = "right") %>%
    unite("lastName", lastName1:lastName3, sep = " ", na.rm = TRUE) %>%
    select(!starts_with(".")) %>%
    mutate(height = as.integer(str_extract(height, "\\d+")))
  
  return(heightData)
  
}

### UPDATE DATA

datasetUpdate <- function() {
  
  playerHeights <- createHeightsDataFrame()
  
  updateDataFile("allscoreinvolvements.csv", createSIDataFrame)
  updateDataFile("playerpositions.csv", createPositionDataFrame)
  
}