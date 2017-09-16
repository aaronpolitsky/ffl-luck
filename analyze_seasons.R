source('read.season.R')
source('rank.simulation.R')

league.id <- 117
years <- 2004:2016

# get league scoring and standings history
league.scoring.history <- lapply(years, function(y) {
  return(get.season.scores(league.id = league.id, y))
})

league.standings.history <- lapply(years, function(y) {
  return(get.season.standings(league.id = league.id, y))
})

names(league.standings.history) <- names(league.scoring.history) <- as.character(years)
names(league.scoring.history) 

# attach actual wins to scoring history table
league.history <- lapply(as.character(years), function(y) {
  return(
    attach.actual.wins(season.scoring.dt = league.scoring.history[[y]], 
                     season.standings.dt = league.standings.history[[y]])
    )
})
names(league.history) <- as.character(years)

# finally, analyze
league.history.record.distributions <- 
  lapply(league.history, season.rank.simulation)


