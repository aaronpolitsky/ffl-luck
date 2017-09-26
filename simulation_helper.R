require(data.table)


simulate.season <- function(ordering) {
  # assign team orders. 
  owner.slot.index <- as.data.table(list(owner.id = owner.ids, slot.id = ordering))
  
  setkey(owner.slot.index, owner.id)
  setkey(season.dt, owner.id)
  season.dt <- owner.slot.index[season.dt]
  
  setkeyv(season.dt, cols = c("week", "slot.id"))
  
  # get opponent scores
  setkeyv(sched, cols = c("week", "opponent.id"))
  sched[season.dt, opp.score := score]
  
  
  # get team scores and copy owner.id
  setkeyv(sched, cols = c("week", "team.id"))
  sched[season.dt, team.score := score]
  sched[season.dt, owner.id := owner.id]
  
  
  records <- sched[, sum(team.score > opp.score) , by=.(owner.id)]

  setkeyv(records, c("owner.id", "V1"))
  
  # merge and increment simulated records
  record.distribution[records, count := count + 1]
}

