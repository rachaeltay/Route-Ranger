library(mongolite)
library(jsonlite)
library(mc2d)
library(lubridate)
library(rlist)

hourlyData <- mongo(collection = "hourlyData", db = "trr", url = "mongodb://soraares:bt3103@therouteranger-shard-00-00-rgv6u.mongodb.net:27017,therouteranger-shard-00-01-rgv6u.mongodb.net:27017,therouteranger-shard-00-02-rgv6u.mongodb.net:27017/test?ssl=true&replicaSet=TheRouteRanger-shard-0&authSource=admin")

stops <- c('PGP', 'Kent_Ridge_MRT', 'NUH', 'LT29', 'UHall', 'Opp_UHC', 'YIH', 'Central_Library', 'LT13', 'AS7', 'COM2', 'BIZ2', 'PGP_Hse_12', 'PGP_Hse_7')
days <- seq.POSIXt(from = as.POSIXct("2017-09-01"), to = as.POSIXct("2017-09-30"), by = "day")

getData <- function() {
  morn <- rtriang(4, min=rnorm(1, 1000, 50), mode=rnorm(1, 1500, 50), max=rnorm(1, 2000, 50))
  peak1 <- rtriang(3, min=rnorm(1, 2000, 50), mode=rnorm(1, 2500, 50), max=rnorm(1, 3000, 50))
  afternoon <- rtriang(3, min=rnorm(1, 1000, 50), mode=rnorm(1, 1500, 50), max=rnorm(1, 2000, 50))
  peak2 <- rtriang(3, min=rnorm(1, 2000, 50), mode=rnorm(1, 2500, 50), max=rnorm(1, 3000, 50))
  night <- rtriang(4, min=rnorm(1, 1000, 50), mode=rnorm(1, 1500, 50), max=rnorm(1, 2000, 50))
  daily <- list(c(morn, peak1, afternoon, peak2, night))
  daily <- lapply(daily, {function(x) x= round(x)})
}

for (stop in stops) {
  for (day in days) {
    #peak hours 11-1 5-7
    boarding <- getData()
    alighting <- getData()
    hourlyData$insert(paste0('{"', stop, '":{"', as.Date.POSIXct(day), '":{"boarding":"', boarding,'", "alighting":"', alighting, '"}}}'))
  }
  print(stop)
}

# a<-hourlyData$find('{"COM2.2017-10-05.boarding": {"$exists":true}}')
# a$COM2$`2017-10-05`
