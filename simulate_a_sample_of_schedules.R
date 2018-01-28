#setwd("/home/rstudio-user/project")
#install.packages(c('data.table', 'XML', 'stringr', 'foreach', 'doSNOW'))
source('read.season.R')
source('create_all_schedules.R')
source("simulation_helper.R")
source("get.next.permutation.R")

weeks <- 2*(div.size-1) + num.teams - div.size

# read in all seasons' scores rather than just one year's
seasons <- as.data.table(read.csv(file = "league117history.csv"))
seasons <- seasons[, X:=NULL] # remove that index column

years <- seasons[, unique(year)]
# import standings
standings.dt <- as.data.table(read.csv(file = "standingsdt.csv")[,-1])

results.list <- lapply(years, function(yr) {
  standings <- standings.dt[year==yr]
  setkeyv(standings, "owner")
  
  season.dt <- seasons[year==yr] 
  
  setkeyv(season.dt, c("week", "owner"))
  season.dt[, owner.id := .GRP, by=.(owner)]
  
  season.dt <- season.dt[owner.id <= num.teams]
  
  sched <- create.reg.season.schedule.all.teams()
  
  season.dt <- season.dt[week <= weeks,]
  number.of.seasons.to.simulate <- 1024*1024
  
  # uses record accumulator to keep track of each team's 
  record.distribution <-      
    as.data.table(expand.grid(c(1:num.teams), c(0:weeks))) 
  names(record.distribution) <- c("home.owner.id", "w")
  setkeyv(record.distribution, c("home.owner.id", "w"))
  record.distribution$count = 0
  record.distribution$year = yr
  
  #construct playoff rates
  playoff.rates <-
    as.data.table(expand.grid(c(1:num.teams)))
  names(playoff.rates) <- "home.owner.id"
  playoff.rates$div.winner <- 0
  playoff.rates$wild.card <- 0
  playoff.rates$made.playoffs <- 0
  setkeyv(playoff.rates, c("home.owner.id"))

    # construct all matchups and generate perspective wins, to serve as a lookup 
  all.possible.matchups <- data.table(expand.grid(1:weeks, 1:num.teams, 1:num.teams))
  names(all.possible.matchups) <- c("week", "home.owner.id", "opp.owner.id")
  setkeyv(all.possible.matchups, c("week", "home.owner.id"))
  setkeyv(season.dt, c("week", "owner.id"))
  all.possible.matchups[season.dt, home.score := score]
  setkeyv(all.possible.matchups, c("week", "opp.owner.id"))
  all.possible.matchups[season.dt, opp.score := score]
  all.possible.matchups[, w := home.score > opp.score]
  setkeyv(all.possible.matchups, c("week", "home.owner.id", "opp.owner.id"))
  
  # get points for
  points.for <- all.possible.matchups[, unique(home.score), by=.(week, home.owner.id)][
    , sum(V1), by=.(home.owner.id)]

  all.possible.matchups[, unique(home.score), by=.(home.owner.id, week)][]
  
  set.seed(99)
  
  system.time(
    for(ii in 1:number.of.seasons.to.simulate) {
      curr <- sample(1:12, replace = F)
      
      simulate.season(schedule = sched, 
                      ordering = curr,
                      all.possible.matchups = all.possible.matchups, 
                      rec.dist = record.distribution,
                      points.for = points.for,
                      playoff.rates = playoff.rates)
      if(ii %% 1000==0) {
        print(paste(yr, ": ", ii/number.of.seasons.to.simulate))
      }
    }
  )
  
  # add back owner names
  record.distribution <- 
    merge(x=record.distribution, y=unique(season.dt[, .(owner.id, owner)]),by.x="home.owner.id", by.y = "owner.id")
  
  record.distribution[, expected.wins := sum(w*count)/sum(count), by=.(home.owner.id)]
  record.distribution[, prob := count/number.of.seasons.to.simulate, by=.(owner)]
  record.distribution[, cumprob := cumsum(prob), by=.(owner)]
  record.distribution[, stdev := sqrt(sum(count*(w-expected.wins)^2)/number.of.seasons.to.simulate), by=.(owner)]
  
  # attach actual wins
  setkeyv(record.distribution, "owner")
  record.distribution[standings, actual.wins := wins]
  setkeyv(record.distribution, c("home.owner.id", "w"))
  write.csv(x = record.distribution, file = paste0("rec.dist",yr,".csv"))

  record.distribution <- record.distribution[playoff.rates]
  return(record.distribution)
})
record.distribution.history <- rbindlist(results.list)
write.csv(x = record.distribution.history, file="record.distribution.history.csv")

