library(mongolite)
library(jsonlite)
library(DT)
library(ggplot2)
library(shiny)
library(forecast)
library(TTR)
library(lubridate)
library(scales)

# queryList <- mongo(url = "mongodb://soraares:bt3103@therouteranger-shard-00-00-rgv6u.mongodb.net:27017,therouteranger-shard-00-01-rgv6u.mongodb.net:27017,therouteranger-shard-00-02-rgv6u.mongodb.net:27017/test?ssl=true&replicaSet=TheRouteRanger-shard-0&authSource=admin", db = "trr", collection = "queryList")
queryList <- mongo(db = "local", collection = "queryList")

server <- function(input, output) {
  getStartDate <- function(date) {
    if (input$timeFrame == "Monthly") {
      paste0(substr(date,0,8), "01", 'T00:00:00Z')
    } else {
      paste0(date, 'T00:00:00Z')
    }
  }
  
  getEndDate <- function(date) {
    if (input$timeFrame == "Monthly") {
      paste0(substr(date,0,8), "31", 'T00:00:00Z')
    } else {
      paste0(date, 'T00:00:00Z')
    }
  }
  
  getBoarding <- reactive({
    if (input$timeFrame != "Hourly") {
      shiny::validate(
        need(input$startDate2 < input$endDate, message='Start date must be earlier than end date')
      )
    }
    
    if (input$timeFrame == "Hourly") {
      query <- paste0('[{"$match": {"sourceBusStop":"', input$busStop,'", "timestamp":{"$gte":{"$date":"', getStartDate(input$startDate1), '"}, "$lt":{"$date":"', getEndDate(input$startDate1+1), '"}}}},
                      {"$project": {"hour": {"$hour":"$timestamp"}}},
                      {"$group":{"_id":{"hour":"$hour"},"count":{"$sum":1}}}]'
      )
    } else if (input$timeFrame == "Daily") {
      query <- paste0('[{"$match": {"sourceBusStop":"', input$busStop,'", "timestamp":{"$gte":{"$date":"', getStartDate(input$startDate2), '"}, "$lt":{"$date":"', getEndDate(input$endDate+1), '"}}}},
                      {"$project": {"day": {"$dayOfMonth":"$timestamp"}}},
                      {"$group":{"_id":{"day":"$day"},"count":{"$sum":1}}}]'
      )
    } else if (input$timeFrame == "Weekly") {
      query <- paste0('[{"$match": {"sourceBusStop":"', input$busStop,'", "timestamp":{"$gte":{"$date":"', getStartDate(input$startDate2), '"}, "$lt":{"$date":"', getEndDate(input$endDate+1), '"}}}},
                      {"$project": {"week": {"$week":"$timestamp"}}},
                      {"$group":{"_id":{"week":"$week"},"count":{"$sum":1}}}]'
      )
    } else if (input$timeFrame == "Monthly") {
      query <- paste0('[{"$match": {"sourceBusStop":"', input$busStop,'", "timestamp":{"$gte":{"$date":"', getStartDate(input$startDate2), '"}, "$lt":{"$date":"', getEndDate(input$endDate+1), '"}}}},
                      {"$project": {"month": {"$month":"$timestamp"}}},
                      {"$group":{"_id":{"month":"$month"},"count":{"$sum":1}}}]'
      )
    }
  })
  
  getAlighting <- reactive({
    if (input$timeFrame != "Hourly") {
      shiny::validate(
        need(input$startDate2 < input$endDate, message='Start date must be earlier than end date')
      )
    }
    
    if (input$timeFrame == "Hourly") {
      query <- paste0('[{"$match": {"destinationBusStop":"COM2", "timestamp":{"$gte":{"$date":"', getStartDate(input$startDate1),'"}, "$lt":{"$date":"', getEndDate(input$startDate1+1),'"}}}},
                      {"$project": {"hour": {"$hour":"$timestamp"}}},
                      {"$group":{"_id":{"hour":"$hour"},"count":{"$sum":1}}}]'
      )
    } else if (input$timeFrame == "Daily") {
      query <- paste0('[{"$match": {"destinationBusStop":"COM2", "timestamp":{"$gte":{"$date":"', getStartDate(input$startDate2),'"}, "$lt":{"$date":"', getEndDate(input$endDate+1),'"}}}},
                      {"$project": {"day": {"$dayOfMonth":"$timestamp"}}},
                      {"$group":{"_id":{"day":"$day"},"count":{"$sum":1}}}]'
      )
    } else if (input$timeFrame == "Weekly") {
      query <- paste0('[{"$match": {"destinationBusStop":"COM2", "timestamp":{"$gte":{"$date":"', getStartDate(input$startDate2),'"}, "$lt":{"$date":"', getEndDate(input$endDate+1),'"}}}},
                      {"$project": {"week": {"$week":"$timestamp"}}},
                      {"$group":{"_id":{"week":"$week"},"count":{"$sum":1}}}]'
      )
    } else if (input$timeFrame == "Monthly") {
      query <- paste0('[{"$match": {"destinationBusStop":"COM2", "timestamp":{"$gte":{"$date":"', getStartDate(input$startDate2),'"}, "$lt":{"$date":"', getEndDate(input$endDate+1),'"}}}},
                      {"$project": {"month": {"$month":"$timestamp"}}},
                      {"$group":{"_id":{"month":"$month"},"count":{"$sum":1}}}]'
      )
    }
  })
  
  getPlot <- eventReactive(input$genResult, {
    busStop <- isolate(input$busStop)
    timeFrame <- isolate(input$timeFrame)
    start1 <- getStartDate(input$startDate1)
    start2 <- getStartDate(input$startDate2)
    end <- getEndDate(input$endDate)
    
    boarding <- queryList$aggregate(isolate(getBoarding()))
    alighting <- queryList$aggregate(isolate(getAlighting()))
    
    boarding <- data.frame(boarding)
    boarding <- cbind(boarding, paste0("Boarding from ", busStop))
    colnames(boarding) <- c("timeFrame", "Count", "Group")
    alighting <- data.frame(alighting)
    alighting <- cbind(alighting, paste0("alighting from ", busStop))
    colnames(alighting) <- c("timeFrame", "Count", "Group")
    
    if (timeFrame == "Hourly") {
      boarding$timeFrame <- lapply(boarding$timeFrame$hour, {function(x) x = as.character(x)})
      boarding$timeFrame <- lapply(boarding$timeFrame, {function(x) if (nchar(x) < 2) {x = paste0(0,x)} else {x = x}})
      boarding$timeFrame <- lapply(boarding$timeFrame, {function(x) x = paste0(x,":00")})
      alighting$timeFrame <- lapply(alighting$timeFrame$hour, {function(x) x = as.character(x)})
      alighting$timeFrame <- lapply(alighting$timeFrame, {function(x) if (nchar(x) < 2) {x = paste0(0,x)} else {x = x}})
      alighting$timeFrame <- lapply(alighting$timeFrame, {function(x) x = paste0(x,":00")})
    } else if (timeFrame == "Daily") {
      boarding$timeFrame <- lapply(boarding$timeFrame$day, {function(x) x = as.character(x)})
      boarding$timeFrame <- lapply(boarding$timeFrame, {function(x) if (nchar(x) < 2) {x = paste0(0,x)} else {x = x}})
      boarding$timeFrame <- lapply(boarding$timeFrame, {function(x) x = paste0(substring(start2, 0, 8), x)})
      alighting$timeFrame <- lapply(alighting$timeFrame$day, {function(x) x = as.character(x)})
      alighting$timeFrame <- lapply(alighting$timeFrame, {function(x) if (nchar(x) < 2) {x = paste0(0,x)} else {x = x}})
      alighting$timeFrame <- lapply(alighting$timeFrame, {function(x) x = paste0(substring(start2, 0, 8), x)})
    } else if (timeFrame == "Weekly") {
      startWeek <- min(boarding$timeFrame$week)
      endWeek <- max(boarding$timeFrame$week)
      boarding$timeFrame <- lapply(boarding$timeFrame$week, {function(x) x = as.character(ymd(paste0(substring(start1, 0, 4), "01-01")) + lubridate::weeks(x))})
      alighting$timeFrame <- lapply(alighting$timeFrame$week, {function(x) x = as.character(ymd(paste0(substring(start1, 0, 4), "01-01")) + lubridate::weeks(x))})
    } else if (timeFrame == "Monthly") {
      boarding$timeFrame <- lapply(boarding$timeFrame$month, {function(x) x = as.character(x)})
      boarding$timeFrame <- lapply(boarding$timeFrame, {function(x) if (nchar(x) < 2) {x = paste0(0,x)} else {x = x}})
      boarding$timeFrame <- lapply(boarding$timeFrame, {function(x) x = paste0(substring(start2, 0, 4), "-", x,"-01")})
      alighting$timeFrame <- lapply(alighting$timeFrame$month, {function(x) x = as.character(x)})
      alighting$timeFrame <- lapply(alighting$timeFrame, {function(x) if (nchar(x) < 2) {x = paste0(0,x)} else {x = x}})
      alighting$timeFrame <- lapply(alighting$timeFrame, {function(x) x = paste0(substring(start2, 0, 4), "-", x,"-01")})  
    }
    
    boarding <- boarding[order(unlist(boarding$timeFrame)),]
    alighting <- alighting[order(unlist(alighting$timeFrame)),]
    combined <- rbind(alighting, boarding)
    combined <- combined[with(combined, order(combined$Group, unlist(combined$timeFrame))),]
    View(boarding)
    
    if (timeFrame == "Daily") {
      boardingSMAtimeFrame <- t(data.frame(tail(boarding$timeFrame, nrow(boarding)-2)))
      alightingSMAtimeFrame <- t(data.frame(tail(alighting$timeFrame, nrow(alighting)-2)))
      SMAtimeFrame <- list(rbind(alightingSMAtimeFrame, boardingSMAtimeFrame))
      SMAtimeFrame <- lapply(SMAtimeFrame[1], {function(x) x = as.POSIXct(x)+days(2)})
      
      boardingSMA <- data.frame(SMA(ts(boarding$Count), 3), paste0("Forecasted boarding from ", busStop))
      colnames(boardingSMA) <- c("Count", "Group")
      boardingSMA <- boardingSMA[3:nrow(boardingSMA),]
      alightingSMA <- data.frame(SMA(ts(alighting$Count), 3), paste0("Forecasted alighting from ", busStop))
      colnames(alightingSMA) <- c("Count", "Group")
      alightingSMA <- alightingSMA[3:nrow(alightingSMA),]
      
      combinedSMA <- rbind(alightingSMA, boardingSMA)
      combinedSMA <- cbind(SMAtimeFrame, combinedSMA)
      colnames(combinedSMA) <- c("timeFrame", "Count", "Group")
      combinedSMA$timeFrame <- sapply(combinedSMA$timeFrame, {function(x) x = as.character(x)})
      combined <- rbind(combined, combinedSMA)
    } else if (timeFrame == "Weekly") {
      TS <- ts(c(boarding$Count), frequency = 52, start = c(as.numeric(substring(start1, 0, 4)), as.numeric(startWeek)))
      # TS <- decompose(TS)
      print(TS)
    }
    
    # redo plot
    
    plot <- ggplot(combined, aes(x=unlist(timeFrame), y=Count, group=Group, color=Group))+geom_line(position=position_dodge(width=0.07))
    plot+labs(title=paste0('Number of riders boarding and alighting at ', busStop), y="num of riders", x="")+
    theme(panel.background=element_rect(fill="black"), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.text.x=element_text(angle = 45, hjust = 1))
  })
  
  output$plot <- renderPlot({
    getPlot()
    
  })

  getNumBoarding <- eventReactive(input$genResult, {
    boarding <- isolate(queryList$aggregate(getBoarding()))
    count <- sum(boarding$count)
  })

  getNumAlighting <- eventReactive(input$genResult, {
    alighting <- isolate(queryList$aggregate(getAlighting()))
    count <- sum(alighting$count)
  })

  output$boarding <- renderText({
    print(getNumBoarding())
  })

  output$alighting <- renderText({
    print(getNumAlighting())
  })
  
  
  # test outputs and functions
  getTable <- eventReactive(input$genResult, {
      
  })
  
  output$testText <- renderText({
    # getBoarding()
  })
  
  output$testTable <- DT::renderDataTable({
    # getTable()
  })
  
}
