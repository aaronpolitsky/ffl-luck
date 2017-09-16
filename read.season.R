require('data.table')
library("XML")
library("stringr")

get.season.standings <- function(league.id, year) {
  standings <- 
    data.table(
      readHTMLTable(doc = paste0("http://games.espn.com/ffl/tools/finalstandings?leagueId=", league.id,
                                 "&seasonId=", year), header=TRUE, as.data.frame = T, 
                    stringsAsFactors=FALSE)$finalRankingsTable)
  standings <- standings[-1]
  standings <- standings[, c(3,5)]
  names(standings) <- c("owner", "record")
  standings$wins <- str_split(standings$record, '-', simplify = T)[,1]
  standings$year <- year
  standings[, wins := as.numeric(wins)]
  return(standings)  
}
  
get.season.scores <- function(league.id, year) {
  raw.season.dt <- data.table(readHTMLTable(doc=paste0("http://games.espn.com/ffl/schedule?leagueId=", league.id,
                                                       "&seasonId=", year, "&teamId=0"), 
                                            as.data.frame = T, stringsAsFactors=FALSE, which = 2))
  
  
  names(raw.season.dt) <- c("away.team", "away.owner", "at", "home.team", "home.owner", "result")
  raw.season.dt <- raw.season.dt[-1,] # first row was header
  
  # remove tie asterisks from result
  raw.season.dt[, result := gsub(x = result, pattern = "*", replacement = "", fixed = T)]
  
  # split result into two scores
  raw.season.dt$away.score <- str_split(raw.season.dt[, result], '-', simplify = T)[,1]
  raw.season.dt$home.score <- str_split(raw.season.dt[, result], '-', simplify = T)[,2]
  
  # throw away columns we don't need
  raw.season.dt[, away.team := NULL]
  raw.season.dt[, home.team := NULL]
  raw.season.dt[, result := NULL]
  raw.season.dt[, at := NULL]
  
  # add week number
  raw.season.dt[, week := floor(.I/9)+1]
  
  # purge extra and empty rows
  raw.season.dt <- raw.season.dt[!is.na(away.owner) & away.owner!="OWNER(S)",]
  
  # reshape into a tall data.table where we have owner, score, week
  away.season.dt <- raw.season.dt[, .(week, away.owner, away.score)]
  home.season.dt <- raw.season.dt[, .(week, home.owner, home.score)]
  
  names(away.season.dt) <- c("week", "owner", "score")
  names(home.season.dt) <- c("week", "owner", "score")
  home.season.dt
  away.season.dt
  season.dt <- rbind(home.season.dt, away.season.dt)
  season.dt$year <- year
  season.dt[, score := as.numeric(score)]
  return(season.dt)
}

attach.actual.wins <- function(season.scoring.dt, season.standings.dt) {
  setkeyv(season.scoring.dt, c("owner"))
  setkeyv(season.standings.dt, c("owner"))
  season.scoring.dt[season.standings.dt, actual.wins := wins]
  return(season.scoring.dt)
}
