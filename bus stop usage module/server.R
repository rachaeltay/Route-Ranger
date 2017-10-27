library(mongolite)
library(jsonlite)
library(DT)
library(ggplot2)
library(shiny)
library(forecast)
library(TTR)
library(lubridate)
library(scales)

#queryList <- mongo(url = "mongodb://soraares:bt3103@therouteranger-shard-00-00-rgv6u.mongodb.net:27017,therouteranger-shard-00-01-rgv6u.mongodb.net:27017,therouteranger-shard-00-02-rgv6u.mongodb.net:27017/test?ssl=true&replicaSet=TheRouteRanger-shard-0&authSource=admin", db = "trr", collection = "queryList")
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
      boarding$timeFrame <- lapply(boarding$timeFrame, {function(x) x = paste0(substring(start1, 0, 10), " ", x,":00:00")})
      alighting$timeFrame <- lapply(alighting$timeFrame$hour, {function(x) x = as.character(x)})
      alighting$timeFrame <- lapply(alighting$timeFrame, {function(x) if (nchar(x) < 2) {x = paste0(0,x)} else {x = x}})
      alighting$timeFrame <- lapply(alighting$timeFrame, {function(x) x = paste0(substring(start1, 0, 10), " ", x,":00:00")})
    } else if (timeFrame == "Daily") {
      boarding$timeFrame <- lapply(boarding$timeFrame$day, {function(x) x = as.character(x)})
      boarding$timeFrame <- lapply(boarding$timeFrame, {function(x) if (nchar(x) < 2) {x = paste0(0,x)} else {x = x}})
      boarding$timeFrame <- lapply(boarding$timeFrame, {function(x) x = paste0(substring(start1, 0, 8), x," 00:00:00")})
      alighting$timeFrame <- lapply(alighting$timeFrame$day, {function(x) x = as.character(x)})
      alighting$timeFrame <- lapply(alighting$timeFrame, {function(x) if (nchar(x) < 2) {x = paste0(0,x)} else {x = x}})
      alighting$timeFrame <- lapply(alighting$timeFrame, {function(x) x = paste0(substring(start1, 0, 8), x," 00:00:00")})
    } else if (timeFrame == "Weekly") {
      boarding$timeFrame <- boarding$timeFrame$week
      alighting$timeFrame <- alighting$timeFrame$week 
    } else if (timeFrame == "Monthly") {
      boarding$timeFrame <- lapply(boarding$timeFrame$month, {function(x) x = as.character(x)})
      boarding$timeFrame <- lapply(boarding$timeFrame, {function(x) if (nchar(x) < 2) {x = paste0(0,x)} else {x = x}})
      boarding$timeFrame <- lapply(boarding$timeFrame, {function(x) x = paste0(substring(start1, 0, 4), "-", x,"-01 00:00:00")})
      alighting$timeFrame <- lapply(alighting$timeFrame$month, {function(x) x = as.character(x)})
      alighting$timeFrame <- lapply(alighting$timeFrame, {function(x) if (nchar(x) < 2) {x = paste0(0,x)} else {x = x}})
      alighting$timeFrame <- lapply(alighting$timeFrame, {function(x) x = paste0(substring(start1, 0, 4), "-", x,"-01 00:00:00")})  
    }
    
    combined <- rbind(alighting, boarding)
    
    # redo plot
    plot <- ggplot(combined, aes(x=unlist(timeFrame), y=Count, group=Group, color=Group)) +geom_line(position=position_dodge(width=0.07))
    plot+labs(title=paste0('Number of riders boarding and alighting at ', busStop), y="# of riders", x="")+
      theme(panel.background=element_rect(fill="black"), panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(), axis.text.x=element_text(angle = 45, hjust = 1))+
      scale_y_continuous(labels = scales::unit_format("000", 1e-4)) #how to do this w/o hard coding?
  })
  
  output$plot <- renderPlot({
    getPlot()
  })

  getNumBoarding <- eventReactive(input$genResult, {
    boarding <- isolate(queryList$aggregate(getBoarding()))
    count <- sum(boarding$count)
    count
  })

  getNumAlighting <- eventReactive(input$genResult, {
    alighting <- isolate(queryList$aggregate(getAlighting()))
    count <- sum(alighting$count)
    count
  })

  output$boarding <- renderText({
    print(getNumBoarding())
  })

  output$alighting <- renderText({
    print(getNumAlighting())
  })
  
  
  # test outputs and functions
  getTable <- eventReactive(input$genResult, {
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
      boarding$timeFrame <- lapply(boarding$timeFrame, {function(x) x = paste0(substring(start1, 0, 10), " ", x,":00:00")})
      alighting$timeFrame <- lapply(alighting$timeFrame$hour, {function(x) x = as.character(x)})
      alighting$timeFrame <- lapply(alighting$timeFrame, {function(x) if (nchar(x) < 2) {x = paste0(0,x)} else {x = x}})
      alighting$timeFrame <- lapply(alighting$timeFrame, {function(x) x = paste0(substring(start1, 0, 10), " ", x,":00:00")})
    } else if (timeFrame == "Daily") {
      boarding$timeFrame <- lapply(boarding$timeFrame$day, {function(x) x = as.character(x)})
      boarding$timeFrame <- lapply(boarding$timeFrame, {function(x) if (nchar(x) < 2) {x = paste0(0,x)} else {x = x}})
      boarding$timeFrame <- lapply(boarding$timeFrame, {function(x) x = paste0(substring(start1, 0, 8), x," 00:00:00")})
      alighting$timeFrame <- lapply(alighting$timeFrame$day, {function(x) x = as.character(x)})
      alighting$timeFrame <- lapply(alighting$timeFrame, {function(x) if (nchar(x) < 2) {x = paste0(0,x)} else {x = x}})
      alighting$timeFrame <- lapply(alighting$timeFrame, {function(x) x = paste0(substring(start1, 0, 8), x," 00:00:00")})
    } else if (timeFrame == "Weekly") {
      boarding$timeFrame <- boarding$timeFrame$week
      alighting$timeFrame <- alighting$timeFrame$week 
    } else if (timeFrame == "Monthly") {
      boarding$timeFrame <- lapply(boarding$timeFrame$month, {function(x) x = as.character(x)})
      boarding$timeFrame <- lapply(boarding$timeFrame, {function(x) if (nchar(x) < 2) {x = paste0(0,x)} else {x = x}})
      boarding$timeFrame <- lapply(boarding$timeFrame, {function(x) x = paste0(substring(start1, 0, 4), "-", x,"-01 00:00:00")})
      alighting$timeFrame <- lapply(alighting$timeFrame$month, {function(x) x = as.character(x)})
      alighting$timeFrame <- lapply(alighting$timeFrame, {function(x) if (nchar(x) < 2) {x = paste0(0,x)} else {x = x}})
      alighting$timeFrame <- lapply(alighting$timeFrame, {function(x) x = paste0(substring(start1, 0, 4), "-", x,"-01 00:00:00")})  
    }
    
    combined <- rbind(alighting, boarding)
    combined <- combined[order(unlist(combined$timeFrame)),]
    
    if (timeFrame == "Hourly") {
      combined$timeFrame <- as.Date(combined$timeFrame)
    }

    # if (timeFrame != "Hourly") {
    #   # if break = hour then only time remaining
    #   combined$timeFrame <- as.Date(combined$timeFrame)
    #   boardingForecastTimeFrame <- data.frame(as.Date(tail(boarding$timeFrame,2))+2)
    #   boardingForecastTimeFrame <- rbind(list(tail(boarding$timeFrame, nrow(boarding)-2)), boardingForecastTimeFrame)
    #   alightingForecastTimeFrame <- data.frame(as.Date(tail(alighting$timeFrame,2))+2)
    #   alightingForecastTimeFrame <- rbind(list(tail(alighting$timeFrame, nrow(alighting)-2)), alightingForecastTimeFrame)
    # } else {
    #   boardingForecastTimeFrame <- data.frame(as.POSIXct(tail(boarding$timeFrame,2))+2*60*60)
    #   boardingForecastTimeFrame <- rbind(list(tail(boarding$timeFrame, nrow(boarding)-2)), boardingForecastTimeFrame)
    #   alightingForecastTimeFrame <- data.frame(as.POSIXct(tail(alighting$timeFrame,2))+2*60*60)
    #   alightingForecastTimeFrame <- rbind(list(tail(alighting$timeFrame, nrow(alighting)-2)), alightingForecastTimeFrame)
    # }
    # 
    # boardingSMA <- data.frame(boardingForecastTimeFrame, SMA(ts(boarding$Count), 3), paste0("Forecasted boarding from ", busStop))
    # colnames(boardingSMA) <- c("timeFrame", "Count", "Group")
    # boardingSMA <- boardingSMA[3:nrow(boardingSMA),]
    # alightingSMA <- data.frame(alightingForecastTimeFrame, SMA(ts(alighting$Count), 3), paste0("Forecasted alighting from ", busStop))
    # colnames(alightingSMA) <- c("timeFrame", "Count", "Group")
    # alightingSMA <- alightingSMA[3:nrow(alightingSMA),]
    # combinedSMA <- rbind(alightingSMA, boardingSMA)
    # combinedSMA$timeFrame <- format(combinedSMA$timeFrame)
    # 
    # combined <- rbind(combined, combinedSMA)
    # combined$timeFrame <- as.character(combined$timeFrame)
    # 
    # if (timeFrame == "Hourly") {
    #   combined$timeFrame <- substring(combined$timeFrame, 12, 16)
    # } else if (timeFrame == "Daily" || timeFrame == "Weekly") {
    #   combined$timeFrame <- substring(combined$timeFrame, 6, 10)
    # } else if (timeFrame == "Monthly") {
    #   combined$timeFrame <- substring(combined$timeFrame, 0, 7)
    # }
      
    View(combined)
    
    # plot <- ggplot(combined, aes(x=unlist(timeFrame), y=Count, group=Group, color=Group))  + geom_line()
    # + geom_line(position=position_dodge(width=0.07))
    # plot+labs(title=paste0('Number of riders boarding and alighting at ', busStop), y="# of riders", x="")+
    #   theme(panel.background=element_rect(fill="black"), panel.grid.major=element_blank(),
    #         panel.grid.minor=element_blank(), axis.text.x=element_text(angle = 45, hjust = 1))+
    #   scale_y_continuous(labels = scales::unit_format("000", 1e-4)) #how to do this w/o hard coding?
  })
  
  output$testText <- renderText({
    # getBoarding()
  })
  
  output$testTable <- DT::renderDataTable({
    getTable()
  })
  
}
  