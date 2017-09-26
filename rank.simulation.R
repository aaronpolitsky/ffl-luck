
# performs a simulation of all schedules based on rank of score in a week.
season.rank.simulation <- function(seas, num.teams, reg.season.weeks) {
  # seas has four columns:  week (number), owner (string), score (number), and year. 
  
  # assign an index to the owners
  seas[, owner.id := .GRP, by=.(owner)]
  
  # ignore playoffs and extra teams
  reg.season <- seas[week <= reg.season.weeks & owner.id <= num.teams]
  
  # generate count of worse scores, and their probabilities, per week
  # note:  rank needs score to be a numeric in order to properly rank scores. 
  reg.season[, worse.scores := base::rank(score)-1, by=.(week)]
  reg.season[, win.prob := worse.scores/(num.teams-1)]
  reg.season[, lose.prob := 1-win.prob]
  
  
  # order by owner, then week. we need this ordering for when we attach the simulated win/loss path
  setkeyv(reg.season, c("owner.id", "week"))
  
  # generate all potential win/loss paths a given 14-week season can take. 
  # 1 represents a win, 0 a loss.  
  all.paths <- data.table(
    expand.grid(c(1,0), c(1,0), c(1,0), c(1,0), 
                c(1,0), c(1,0), c(1,0), c(1,0), 
                c(1,0), c(1,0), c(1,0), c(1,0), 
                c(1,0), c(1,0)))
  all.paths <- all.paths[1:(2^reg.season.weeks-1), 1:num.teams, with=F]
  
  # construct a record distribution table
  record.distribution <- 
    as.data.table(expand.grid("owner.id"=c(1:num.teams), 
                              "wins"=c(0:reg.season.weeks), 
                              "losses"=c(reg.season.weeks:0)))[wins + losses == reg.season.weeks]
  setkeyv(record.distribution, c("owner.id", "wins"))
  # initialize its probability column
  record.distribution[, prob := 0]
  
  # add owner name column and actual wins. 
  reg.season[, owner.id := .GRP, by=.(owner)]
  
  owner.index <- unique(reg.season[owner.id <= num.teams, .(owner, actual.wins, year), by=.(owner.id)])
  setkeyv(owner.index, c("owner.id"))

  record.distribution <- record.distribution[owner.index ]
  
  # for each win/loss path 
  system.time(
    for(path.id in 1:dim(all.paths)[1]) {
      # each row of all.paths is a win/loss sequence.  
      #   for each row, transpose into a column, replicate for each owner, and attach to reg.season
      reg.season$result <- rep(t(all.paths[path.id, 1:reg.season.weeks, with=F]), num.teams)
      
      # use that result column as a switch to choose associated probability 
      reg.season[, result.probability := ifelse(result==1, win.prob, lose.prob)] 
      reg.season[, sim.wins := sum(result), by=.(owner)]
      
      # aggregate down to owner win-loss record and multiply the probability column
      #  the product represents the probability of taking that exact path through win-loss tree.
      to.merge <- 
        reg.season[, prod(result.probability), by=.(owner.id, sim.wins)]
      # prep to merge back to record.disribution
      setkeyv(to.merge, c("owner.id", "sim.wins"))
      
      # merge it back, accumulating probability per record-owner
      record.distribution[to.merge, prob := prob + V1]
    }
  )
  
  # calculate expected wins, variance, and such, based on record.distribution. 
  record.distribution[, expected.wins := sum(prob*wins), by=.(owner)]
  record.distribution[, variance := sum(prob*(wins-expected.wins)^2), by=.(owner)]
  record.distribution[, stddev := sqrt(variance), by=.(owner)]
  record.distribution[, wins.over.expeced := actual.wins-expected.wins, by=.(owner)]
  
  return(record.distribution)
}

