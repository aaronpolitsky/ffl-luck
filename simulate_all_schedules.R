source('read.season.R')
source('create_all_schedules.R')
source("simulation_helper.R")
source("get.next.permutation.R")

weeks <- 2*(div.size-1) + num.teams - div.size

# read in all seasons' scores rather than just one year's
seasons <- as.data.table(read.csv(file = "league117history.csv"))
seasons <- seasons[, X:=NULL] # remove that index column
#season.dt <- get.season.scores(league.id = 117, year = 2016)[week <= weeks]

years <- seasons[, unique(year)]

result.list <- lapply(years, function(yr) {
  season.dt <- seasons[year==yr] 

  setkeyv(season.dt, c("week", "owner"))
  season.dt[, owner.id := .GRP, by=.(owner)]

  season.dt <- season.dt[owner.id <= num.teams]

  sched <- create.reg.season.schedule.all.teams()

  season.dt <- season.dt[week <= weeks,]

  # uses record counter to keep track of each team's 
  record.distribution <-      
    as.data.table(expand.grid(c(1:num.teams), c(0:weeks))) 
  names(record.distribution) <- c("home.owner.id", "w")
  setkeyv(record.distribution, c("home.owner.id", "w"))
  record.distribution$count = 0

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


  curr <- 1:num.teams # first permutation
  j <- 0
  keep.going <- TRUE
  #ten.thousandth.curr <- c(1,2,3,4,6,12,11,7,9,10,5,8) # because i simulated it.
  system.time(
    # go through every ordering of teams, and hence, every schedule 
    # while(length(curr) > 0) { # this is what get.next.permutation returns when it rolls over.
    while(keep.going) {
      #  while(!all(curr == ten.thousandth.curr)) { 
      # curr is the current ordering of teams to schedule slots.  

      simulate.season(schedule = sched, 
                      ordering = curr,
                      all.possible.matchups = all.possible.matchups, 
                      rec.dist = record.distribution)
      curr <- get.next.permutation()

      j <- j + 1
      if(j==10000) {keep.going <- FALSE} # break condition
    } 
  )

  # add back owner names
  #record.distribution <- 
  #  merge(x=record.distribution, y=unique(season.dt[, .(owner.id, owner)]), by = "owner.id")

  #setkeyv(record.distribution, c("w", "owner.id"))
  return(record.distribution)
})
