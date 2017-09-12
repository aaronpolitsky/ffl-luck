source('read.season.R')

# season.dt has four columns:  week (number), owner (string), score (number), and year. 
# assign an index to the owners
season.dt[, owner.id := .GRP, by=.(owner)]

# generate count of worse scores, and their probabilities, per week
season.dt[, worse.scores := rank(score)-1, by=.(week)]
season.dt[, win.prob := worse.scores/11]
season.dt[, lose.prob := 1-win.prob]

# ignore playoffs which start in week 15
reg.season <- season.dt[week<15]

# order by owner, then week. we need this ordering for when we attach the simulated win/loss path
setkeyv(reg.season, c("owner.id", "week"))

# generate all potential win/loss paths a given 14-week season can take. 
# 1 represents a win, 0 a loss.  
all.paths <- data.table(
  expand.grid(c(1,0), c(1,0), c(1,0), c(1,0), 
              c(1,0), c(1,0), c(1,0), c(1,0), 
              c(1,0), c(1,0), c(1,0), c(1,0), 
              c(1,0), c(1,0)))

# construct a record distribution table
record.distribution <- 
  as.data.table(expand.grid("owner.id"=c(1:12), 
                            "wins"=c(0:14), 
                            "losses"=c(14:0)))[wins + losses == 14]
setkeyv(record.distribution, c("owner.id", "wins"))
# initialize its probability column
record.distribution[, prob := 0]

# for each win/loss path 
system.time(
for(path.id in 1:dim(all.paths)[1]) {
  # each row of all.paths is a win/loss sequence.  
  #   for each row, transpose into a column, replicate for each owner, and attach to reg.season
  reg.season$result <- rep(t(all.paths[path.id,1:14, with=F]), 12)
  
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

# add owner name column. 
owner.index <- reg.season[, unique(owner), by=.(owner.id)]
record.distribution[owner.index, owner := V1]

write.csv(record.distribution, "record.distribution.2016.csv")

# calculate expected wins, variance, and such, using all.paths and record.distribution. 
record.distribution[, expected.wins := sum(prob*wins), by=.(owner)]
record.distribution[, variance := sum(prob*(wins-expected.wins)^2), by=.(owner)]
record.distribution[, stddev := sqrt(variance), by=.(owner)]
