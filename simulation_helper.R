require(data.table)

simulate.season <- function(schedule, ordering, all.possible.matchups, points.for, rec.dist, playoff.rates) {
  # generate owner.slot lookup from the ordering
  owner.slot.index <- as.data.table(list(owner.id = 1:12, slot.id = ordering))
  setkey(owner.slot.index, "slot.id")
  owner.slot.index[, division := ifelse(slot.id <= 4, 1, ifelse(slot.id <=8, 2, 3))]
  
  # copy schedule so as to avoid nasty side effects from iteration to iteration.
  sched <- copy(schedule)
  
  # identify owner.ids of home slot team
  setkeyv(sched, "home.slot.id")
  sched[owner.slot.index, home.owner.id := owner.id]
  # get owner.id of opponent slot.id
  setkeyv(sched, "opp.slot.id")
  sched[owner.slot.index, opp.owner.id := owner.id]

  # set keys to join sched to all.possible.matchups
  setkeyv(sched, c("week", "home.owner.id", "opp.owner.id"))
  
  # for each actual matchup, lookup whether it was a win, then find the total wins by owner
  records <- all.possible.matchups[sched, sum(w), by=.(home.owner.id)]

  # add back division
  setkeyv(owner.slot.index, "owner.id")
  setkeyv(records, "home.owner.id")
  records <- records[owner.slot.index, division := division]
  
  # get points for
#  records[all.possible.matchups[, unique(home.score), by=.(week, home.owner.id)][, sum(V1), by=.(home.owner.id)], points.for := i.V1]
  # calculate division place finish
  records[points.for, points.for := i.V1]
  records[, place := .(frank(.SD, -V1, -points.for)), by=.(division)]
  records[, div.winner := place==1]
  
  # calculate wild card
  records[, wild.card := F]
  records[div.winner==F, wild.card := frank(.SD, -V1, -points.for)==1]
  records[,  !div.winner & frank(.SD, -V1, -points.for)]
  
  # playoffs
  records[, made.playoffs := div.winner==T | wild.card==T]
  
  playoff.rates[records, div.winner    := div.winner + i.div.winner]
  playoff.rates[records, wild.card     := wild.card + i.wild.card]
  playoff.rates[records, made.playoffs := made.playoffs + i.made.playoffs]
  
  # then accumulate back to the record distribution
  setkeyv(records, c("home.owner.id", "V1"))
  rec.dist[records, count := count + 1]
  
}
