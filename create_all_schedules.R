require('data.table')

num.teams <- 12
div.size <- 4

create.reg.season.schedule.all.teams <- function() {

 # import generic schedule matrix for teams assigned 1:num.teams
  if(num.teams==12) {
    schedule.matrix <- as.data.table(read.csv("./ffl schedule - Sheet1.csv", 
                                              row.names=1, colClasses = c("integer")))
  } else if (num.teams==6) {
    schedule.matrix <- as.data.table(read.csv("./six-team schedule - Sheet1.csv", 
                                              row.names=1, colClasses = c("integer")))
  } 
    
  # form a long data.table with week, home, away column
  schedule.dt <- data.table("week" = integer(),
                            "home.slot.id" = integer(),
                            "opp.slot.id" = integer())
  last.div.play.weeks.dt <- copy(schedule.dt)
  
  for (tid in as.integer(rownames(schedule.matrix))) {
    for(w in 1:(num.teams-1)) {
      opp.id <- which(schedule.matrix[tid,]==w)
      schedule.dt <- rbind(schedule.dt, list(w, tid, opp.id))
    }
  }
  
  if(num.teams == 12) {
    # weeks 12-14 are weeks 1-3 in reverse
    last.div.play.weeks.dt <- copy(schedule.dt)[, week := (num.teams-1)+(4-week)][week %in% 12:14]
  }
  else if (num.teams==6) {
    last.div.play.weeks.dt <- copy(schedule.dt)[week==1, ][, week:=6]
  }
  schedule.dt <- rbind(schedule.dt, last.div.play.weeks.dt)
  return(schedule.dt)
}
