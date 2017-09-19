source('read.season.R')
source('create_all_schedules.R')
source("simulation_helper.R")
source("get.next.permutation.R")

season.dt <- get.season.scores(league.id = 117, year = 2016)
season.dt[, owner.id := .GRP, by=.(owner)]
owner.ids <- season.dt[, unique(owner.id)]

library(combinat)

sched <- create.reg.season.schedule.all.teams()

curr <- 1:12 # first permutation

j <- 0

# uses global record counter to keep track of each team's 
record.distribution <-      # teams, wins     losses   ties
  #as.data.table(expand.grid(c(1:12), c(0:14), c(14:0), c(0:14)))[Var2 + Var3 + Var4 == 14]
  as.data.table(expand.grid(c(1:12), c(0:14))) #

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
#setkeyv(record.distribution, c("w", "t", "l", "owner.id"))
setkeyv(record.distribution, c("w", "owner.id"))

record.distribution[owner.id==5 & count>0]

