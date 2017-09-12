require('data.table')


season.dt <- as.data.table(read.csv('ffl 2016 results - Sheet3.csv', header = T))
season.dt[is.na(week)]
season.dt <- season.dt[!is.na(week)]
season.dt[, owner := as.factor(as.character(owner))]
season.dt$year = 2016

