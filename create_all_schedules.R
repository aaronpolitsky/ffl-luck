require('data.table')

create.reg.season.schedule.all.teams <- function() {

 # import generic schedule matrix for teams assigned 1:12
  schedule.matrix <- as.data.table(read.csv("./ffl schedule - Sheet1.csv", 
                                            row.names=1))
  
  # form a long data.table with week, home, away column
  schedule.dt <- data.table("week" = numeric(),
                          "team.id" = integer(),
                          "opponent.id" = integer())
  last.3.dt <- copy(schedule.dt)
  
  for (tid in as.numeric(rownames(schedule.matrix))) {
    for(w in 1:11) {
      opp.id <- which(schedule.matrix[tid,]==w)
      schedule.dt <- rbind(schedule.dt, list(w, tid, opp.id))
    }
  }
  
  # weeks 12-14 are weeks 1-3 in reverse
  last.3.dt <- copy(schedule.dt)[, week := 11+(4-week)][week %in% 12:14]
  schedule.dt <- rbind(schedule.dt, last.3.dt)
  
  return(schedule.dt)
}
