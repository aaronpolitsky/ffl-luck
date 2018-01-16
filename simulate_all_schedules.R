setwd("/home/rstudio-user/project")
install.packages(c('data.table', 'XML', 'stringr', 'foreach', 'doSNOW'))
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
# just do this one for now.
yr <- 2015
#result.list <- lapply(years, function(yr) {
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
denom <- factorial(12)
#save.image("./workspace.Rdata")    

load("breaks.Rda")
# restart from here
#load("./workspace.Rdata")
#keep.going <- TRUE

# go through every ordering of teams, and hence, every schedule 
library(foreach)
library(doSNOW)
#stopCluster(cl = cl)
cl <- makeCluster(8, outfile="")
registerDoSNOW(cl)

curr.index <- 1:16

clusterExport(cl, "breaks")
clusterExport(cl, "get.next.permutation")
clusterExport(cl, "simulate.season")
clusterEvalQ(cl, library('data.table'))
clusterExport(cl, c("sched", "all.possible.matchups", "record.distribution"))

#initialize if need be
#foreach(ci=1:8) %dopar% {
#  curr <- breaks[[ci]]
#  stop.curr <- breaks[[ci+1]]
#  j <- 0
#  save.image(file=paste0("image_", ci,".Rdata"))
#}

asdf1to8 <- foreach(ci=1:8) %dopar% {
  curr <- breaks[[ci]]
  stop.curr <- breaks[[ci+1]]
  j <- 0
  # for restarting
  load(file=paste0("image_", ci,".Rdata"))
  #while(j < 10000) {
  while(!all(curr==stop.curr)){
    simulate.season(schedule = sched, 
                    ordering = curr,
                    all.possible.matchups = all.possible.matchups, 
                    rec.dist = record.distribution)
    curr <- get.next.permutation(curr, stop.curr = stop.curr)
    j <- j + 1
    # every so often, save
    if(j%%10000==0) {
      print(c(paste0(ci,": ", round(j/denom*16*100, digits=3),"%")))
      save.image(file=paste0("image_", ci,".Rdata"))
    } 
  }
  save.image(file=paste0("image_",ci,".Rdata"))
  return(record.distribution)
}

stopCluster(cl)

stop()
while(length(curr) > 1) { # get.next.permutation returns 0 when it rolls over.
  #while(keep.going) {
  #  while(!all(curr == ten.thousandth.curr)) { 
  # curr is the current ordering of teams to schedule slots.  
  
  simulate.season(schedule = sched, 
                  ordering = curr,
                  all.possible.matchups = all.possible.matchups, 
                  rec.dist = record.distribution)
  curr <- get.next.permutation(curr)
  
  j <- j + 1
  # every so often, save
  if(j%%10000==0) {
    print(c(paste0(round(j/denom*100, digits=3),"%"),curr))
    save.image("./workspace.Rdata")
  } 
} 


# add back owner names
#record.distribution <- 
#  merge(x=record.distribution, y=unique(season.dt[, .(owner.id, owner)]), by = "owner.id")

#setkeyv(record.distribution, c("w", "owner.id"))
#return(record.distribution)
save(list=c("yr", "record.distribution"), file=paste0(year,"results.Rda"))
#})
