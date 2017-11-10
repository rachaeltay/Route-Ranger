library(mongolite)
library(jsonlite)
library(mc2d)
library(lubridate)

hourlyData <- mongo(collection = "hourlyData", db = "trr", url = "mongodb://soraares:bt3103@therouteranger-shard-00-00-rgv6u.mongodb.net:27017,therouteranger-shard-00-01-rgv6u.mongodb.net:27017,therouteranger-shard-00-02-rgv6u.mongodb.net:27017/test?ssl=true&replicaSet=TheRouteRanger-shard-0&authSource=admin")
# hourlyData <- mongo(collection = "hourlyData", db = "local")

stops <- c('PGP', 'Kent_Ridge_MRT', 'NUH', 'LT29', 'UHall', 'Opp_UHC', 'YIH', 'Central_Library', 'LT13', 'AS7', 'COM2', 'BIZ2', 'PGP_Hse_12', 'PGP_Hse_7')
days <- seq.POSIXt(from = as.POSIXct("2017-09-17"), to = as.POSIXct("2017-12-31"), by = "day")

getData <- function() {
  ls <- numeric()
  vol <- round(rtriang(17, min=2000, mode=2200, max=2500))
  
  for (i in 7:23) {
    ls <- c(ls, round(rtriang(1, min=vol[i-6]-25, mode=vol[i-6], max=vol[i-6]+35)))
  }
  list(ls)
}

for (stop in stops) {
  boarding <- getData()
  alighting <- getData()
  for (day in days) {
    boarding <- lapply(boarding, {function(x) x = x + round(rnorm(1, mean=0, sd=20))})
    alighting <- lapply(alighting, {function(x) x = x + round(rnorm(1, mean=0, sd=20))})
    hourlyData$insert(paste0('{"', stop, '":{"', as.Date.POSIXct(day), '":{"boarding":"', boarding,'", "alighting":"', alighting, '"}}}'))
  }
  print(stop)
}

# data<-hourlyData$find('{"COM2.2017-11-10.boarding": {"$exists":true}}')