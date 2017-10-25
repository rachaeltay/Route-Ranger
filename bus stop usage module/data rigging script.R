library(mc2d)
library(lubridate)
library(mongolite)

queryList <- mongo(url = "mongodb://soraares:bt3103@therouteranger-shard-00-00-rgv6u.mongodb.net:27017,therouteranger-shard-00-01-rgv6u.mongodb.net:27017,therouteranger-shard-00-02-rgv6u.mongodb.net:27017/test?ssl=true&replicaSet=TheRouteRanger-shard-0&authSource=admin", db = "trr", collection = "queryList")

set.seed(123)

hourly <- list(500,550,700,950,1000,1200,950,800,650,500,500,450,400,350,300,200,100)

for (i in 7:23) {
  normal <- sort(c(rnorm(60, 10, 0.075)))
  normal1 <- normal[31:60]
  normal2 <- normal[1:30]
  normal <- c(normal1, normal2)
  normal <- sapply(normal, {function(x) x = rnorm(1,x,1)})
  denom <- sum(normal)
  mins <- round(sapply(normal, {function(x) x = x/denom*hourly[[i-6]]}))
  
  for (k in 1:60) {
    for (l in 1:mins[[k]]) {
    date <- Sys.time()
    hour(date) <- i
    minute(date) <- k 
    
    query <- c(
      paste0('{
             "bus" : "A2",
             "stopId": "BIZ2",
             "busIdx": "",
             "realIdx": "",
             "sourceBusStop" : "BIZ2",
             "destinationBusStop" : "KentRidgeMRT",
             "timestamp" : {"$date": "',substring(date,0,10),'T',substring(date,12,19),'.772Z"}}'
      )
    )
    queryList$insert(query)
    }
  }
  print(i)
}
