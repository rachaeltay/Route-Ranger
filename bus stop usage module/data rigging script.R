library(mongolite)
library(jsonlite)
library(mc2d)
library(lubridate)
library(rlist)

hourlyData <- mongo(collection = "hourlyData", db = "trr", url = "mongodb://soraares:bt3103@therouteranger-shard-00-00-rgv6u.mongodb.net:27017,therouteranger-shard-00-01-rgv6u.mongodb.net:27017,therouteranger-shard-00-02-rgv6u.mongodb.net:27017/test?ssl=true&replicaSet=TheRouteRanger-shard-0&authSource=admin")

stops <- c('PGP', 'Kent_Ridge_MRT', 'NUH', 'LT29', 'UHall', 'Opp_UHC', 'YIH', 'Central_Library', 'LT13', 'AS7', 'COM2', 'BIZ2', 'PGP_Hse_12', 'PGP_Hse_7')
days <- seq.POSIXt(from = as.POSIXct("2017-11-01"), to = as.POSIXct("2017-11-30"), by = "day")
vol <- c(2000, 2100, 2150 ,2250, 2300, 2250, 2300, 2300, 2150, 2200, 2250, 2300, 2200, 2150, 2150, 2100, 2100)

getData <- function() {
  # morn1 <- rtriang(2, min=2100, mode=2125, max=2150)
  # morn2 <- rtriang(2, min=2200, mode=2250, max=2250)
  # peak1 <- rtriang(3, min=2300, mode=2325, max=2375)
  # afternoon <- rtriang(3, min=2200, mode=2250, max=2300)
  # peak2 <- rtriang(3, min=2200, mode=2225, max=2250)
  # night1 <- rtriang(2, min=2100, mode=2125, max=2150)
  # night2 <- rtriang(2, min=2050, mode=2075, max=2200)
  ls <- numeric()
  
  for (i in 7:23) {
    ls <- c(ls, round(rtriang(1, min=vol[i-6]-25, mode=vol[i-6], max=vol[i-6]+35)))
  }
  
  # daily <- list(c(morn1, morn2, peak1, afternoon, peak2, night1, night2))
  # daily <- lapply(daily, {function(x) x= round(x)})
  list(ls)
}

for (stop in stops) {
  vol <- sample(vol)
  for (day in days) {
    #peak hours 11-1 5-7
    boarding <- getData()
    alighting <- getData()
    hourlyData$insert(paste0('{"', stop, '":{"', as.Date.POSIXct(day), '":{"boarding":"', boarding,'", "alighting":"', alighting, '"}}}'))
  }
  print(stop)
}

# data<-hourlyData$find('{"PGP.2017-11-03.boarding": {"$exists":true}}')
# a$COM2$`2017-10-05`
