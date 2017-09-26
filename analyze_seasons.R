source('read.season.R')
debugSource('rank.simulation.R')

num.teams <- 6
div.size <- 2

league.id <- 117
years <- 2016

weeks <- 2*(div.size-1) + num.teams - div.size

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
lapply(league.history, function(lh) {
  setkeyv(lh, c("week", "owner"))
})

league.history.record.distributions <- 
  lapply(league.history, function(lh) {
    season.rank.simulation(seas = lh, 
                           num.teams = num.teams, 
                           reg.season.weeks=weeks)
  })

league.history.record.distributions.dt <- rbindlist(league.history.record.distributions)
write.csv(x = league.history.record.distributions.dt, file = "league.history.record.distributions.csv")
