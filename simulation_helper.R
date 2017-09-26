require(data.table)


simulate.season <- function(season, schedule, ordering, rec.dist) {
  # data.tables are passed by reference, so copy to discard any side effects
  sched <- copy(schedule)
  seas <- copy(season) 
  
  # assign team orders. 
  owner.slot.index <- as.data.table(list(owner.id = owner.ids, slot.id = ordering))
  
  setkey(owner.slot.index, owner.id)
  setkey(seas, owner.id)
  seas <- owner.slot.index[seas]
  
  setkeyv(seas, cols = c("week", "slot.id"))
  
  # get opponent scores
  setkeyv(sched, cols = c("week", "opponent.id"))
  sched[seas, opp.score := score]
  
  
  # get team scores and copy owner.id
  setkeyv(sched, cols = c("week", "team.id"))
  sched[seas, team.score := score]
  sched[seas, team.owner.id := owner.id]
  
  records <- sched[, sum(team.score > opp.score) , by=.(team.owner.id)]

  setkeyv(records, c("team.owner.id", "V1"))
  
  # merge and increment simulated records
  rec.dist[records, count := count + 1]
  
  return(rec.dist)
}

