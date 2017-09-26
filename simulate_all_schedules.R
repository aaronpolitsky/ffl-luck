source('read.season.R')
source('create_all_schedules.R')
debugSource("simulation_helper.R")
source("get.next.permutation.R")

weeks <- 2*(div.size-1) + num.teams - div.size

season.dt <- get.season.scores(league.id = 117, year = 2016)[week <= weeks]
season.dt[, owner.id := .GRP, by=.(owner)]

season.dt <- season.dt[owner.id <= num.teams]
owner.ids <- season.dt[, unique(owner.id)]

sched <- create.reg.season.schedule.all.teams()
sched[, team.id := as.integer(team.id)]

curr <- 1:num.teams # first permutation
j <- 0

season.dt <- season.dt[week <= weeks,]
# uses global record counter to keep track of each team's 
record.distribution <-      # teams, wins     losses   ties
  as.data.table(expand.grid(c(1:num.teams), c(0:weeks))) #

names(record.distribution) <- c("owner.id", "w")#, "l", "t")
setkeyv(record.distribution, c("owner.id", "w"))#, "l", "t"))
record.distribution$count = 0

system.time(
  while(length(curr)>1) { # curr is the current ordering of teams to schedule slots.  
    # simulate
    simulate.season(curr)
  
    curr <- get.next.permutation()
    j <- j + 1
    if(j==10000) {curr<-0} # break condition
  } 
)

# add back owner names
record.distribution <- 
  merge(x=record.distribution, y=unique(season.dt[, .(owner.id, owner)]), by = "owner.id")

setkeyv(record.distribution, c("w", "owner.id"))

record.distribution[owner.id==5 & count>0]

