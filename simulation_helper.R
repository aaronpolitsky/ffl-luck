require(data.table)

simulate.season <- function(schedule, ordering, all.possible.matchups, rec.dist) {
  # generate owner.slot lookup from the ordering
  owner.slot.index <- as.data.table(list(owner.id = 1:12, slot.id = ordering))
  setkey(owner.slot.index, "slot.id")
  
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

  # then accumulate back to the record distribution
  setkeyv(records, c("home.owner.id", "V1"))
  rec.dist[records, count := count + 1]
  
}
