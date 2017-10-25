library(mongolite)
library(jsonlite)
library(DT)
library(ggplot2)
library(shiny)
library(forecast)
library(TTR)
library(lubridate)

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
      query <- paste0('{"sourceBusStop":"', input$busStop, '","timestamp":{"$gte":{"$date":"', getStartDate(input$startDate1), '"},"$lt":{"$date":"', getEndDate(input$startDate1+1),'"}}}')
    } else if (input$timeFrame == "Monthly") {
      query <- paste0('{"sourceBusStop":"', input$busStop, '","timestamp":{"$gte":{"$date":"', getStartDate(input$startDate2), '"},"$lt":{"$date":"', getEndDate(input$endDate+1), '"}}}')
    } else {
      query <- paste0('{"sourceBusStop":"', input$busStop, '","timestamp":{"$gte":{"$date":"', getStartDate(input$startDate2), '"},"$lt":{"$date":"', getEndDate(input$endDate+1), '"}}}')
    }
  })
  
  getAlighting <- reactive({
    if (input$timeFrame != "Hourly") {
      shiny::validate(
        need(input$startDate2 < input$endDate, message='Start date must be earlier than end date')
      )
    }
    
    if (input$timeFrame == "Hourly") {
      query <- paste0('{"destinationBusStop":"', input$busStop, '","timestamp":{"$gte":{"$date":"', getStartDate(input$startDate1), '"},"$lt":{"$date":"', getEndDate(input$startDate1+1),'"}}}')
    } else if (input$timeFrame == "Monthly") {
      query <- paste0('{"destinationBusStop":"', input$busStop, '","timestamp":{"$gte":{"$date":"', getStartDate(input$startDate2), '"},"$lt":{"$date":"', getEndDate(input$endDate+1), '"}}}')
    } else {
      query <- paste0('{"destinationBusStop":"', input$busStop, '","timestamp":{"$gte":{"$date":"', getStartDate(input$startDate2), '"},"$lt":{"$date":"', getEndDate(input$endDate+1), '"}}}')
    }
  })
  
  getPlot <- eventReactive(input$genResult, {
    busStop <- isolate(input$busStop)
    timeFrame <- isolate(input$timeFrame)
    
    boarding <- queryList$find(isolate(getBoarding()))
    alighting <- queryList$find(isolate(getAlighting()))
    
    if (timeFrame == "Hourly") {
      boarding <- table(cut(boarding$timestamp, breaks="hour"))
      alighting <- table(cut(alighting$timestamp, breaks="hour"))
    } else if (timeFrame == "Daily") {
      boarding <- table(cut(boarding$timestamp, breaks="day"))
      alighting <- table(cut(alighting$timestamp, breaks="day"))
    } else if (timeFrame == "Weekly") {
      boarding <- table(cut(boarding$timestamp, breaks="week"))
      alighting <- table(cut(alighting$timestamp, breaks="week"))
    } else if (timeFrame == "Monthly") {
      boarding <- table(cut(boarding$timestamp, breaks="month"))
      alighting <- table(cut(alighting$timestamp, breaks="month"))
    }
    
    boarding <- data.frame(boarding)
    boarding$Group <- paste0("Boarding from ", busStop)
    colnames(boarding) <- c("timeFrame", "Count", "Group")
    alighting <- data.frame(alighting)
    alighting$Group <- paste0("Alighting from ", busStop)
    colnames(alighting) <- c("timeFrame", "Count", "Group")
    combined <- rbind(alighting, boarding)
    
    if (timeFrame != "Hourly") {
      # if break = hour then only time remaining
      combined$timeFrame <- as.Date(combined$timeFrame)
      boardingForecastTimeFrame <- data.frame(as.Date(tail(boarding$timeFrame,2))+2)
      boardingForecastTimeFrame <- rbind(list(tail(boarding$timeFrame, nrow(boarding)-2)), boardingForecastTimeFrame)
      alightingForecastTimeFrame <- data.frame(as.Date(tail(alighting$timeFrame,2))+2)
      alightingForecastTimeFrame <- rbind(list(tail(alighting$timeFrame, nrow(alighting)-2)), alightingForecastTimeFrame)
    } else {
      boardingForecastTimeFrame <- data.frame(as.POSIXct(tail(boarding$timeFrame,2))+2*60*60)
      boardingForecastTimeFrame <- rbind(list(tail(boarding$timeFrame, nrow(boarding)-2)), boardingForecastTimeFrame)
      alightingForecastTimeFrame <- data.frame(as.POSIXct(tail(alighting$timeFrame,2))+2*60*60)
      alightingForecastTimeFrame <- rbind(list(tail(alighting$timeFrame, nrow(alighting)-2)), alightingForecastTimeFrame)
    }
    
    boardingSMA <- data.frame(boardingForecastTimeFrame, SMA(ts(boarding$Count), 3), paste0("Forecasted boarding from ", busStop))
    colnames(boardingSMA) <- c("timeFrame", "Count", "Group")
    boardingSMA <- boardingSMA[3:nrow(boardingSMA),]
    alightingSMA <- data.frame(alightingForecastTimeFrame, SMA(ts(alighting$Count), 3), paste0("Forecasted alighting from ", busStop))
    colnames(alightingSMA) <- c("timeFrame", "Count", "Group")
    alightingSMA <- alightingSMA[3:nrow(alightingSMA),]
    combinedSMA <- rbind(alightingSMA, boardingSMA)
    combinedSMA$timeFrame <- format(combinedSMA$timeFrame)
    
    combined <- rbind(combined, combinedSMA)
    combined$timeFrame <- as.character(combined$timeFrame)
    
    if (timeFrame == "Hourly") {
      combined$timeFrame <- substring(combined$timeFrame, 12, 16)
    } else if (timeFrame == "Daily" || timeFrame == "Weekly") {
      combined$timeFrame <- substring(combined$timeFrame, 6, 10)
    } else if (timeFrame == "Monthly") {
      combined$timeFrame <- substring(combined$timeFrame, 0, 7)
    }

    plot <- ggplot(combined, aes(x=timeFrame, y=Count, group=Group, color=Group)) +geom_line(position=position_dodge(width=0.07))
    plot+labs(title=paste0('Number of riders boarding and alighting at ', busStop), y="# of riders", x="")+
      theme(panel.background=element_rect(fill="black"), panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(), axis.text.x=element_text(angle = 45, hjust = 1))+
      scale_y_continuous(breaks=seq(0:10000)) #how to do this w/o hard coding?
  })
  
  output$plot <- renderPlot({
    getPlot()
  })
  
  getNumBoarding <- eventReactive(input$genResult, {
    isolate(nrow(queryList$find(getBoarding())))
  })
  
  getNumAlighting <- eventReactive(input$genResult, {
    isolate(nrow(queryList$find(getAlighting())))
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
    
    boarding <- queryList$find(isolate(getBoarding()))
    alighting <- queryList$find(isolate(getAlighting()))
    
    if (timeFrame == "Hourly") {
      boarding <- table(cut(boarding$timestamp, breaks="hour"))
      alighting <- table(cut(alighting$timestamp, breaks="hour"))
    } else if (timeFrame == "Daily") {
      boarding <- table(cut(boarding$timestamp, breaks="day"))
      alighting <- table(cut(alighting$timestamp, breaks="day"))
    } else if (timeFrame == "Weekly") {
      boarding <- table(cut(boarding$timestamp, breaks="week"))
      alighting <- table(cut(alighting$timestamp, breaks="week"))
    } else if (timeFrame == "Monthly") {
      boarding <- table(cut(boarding$timestamp, breaks="month"))
      alighting <- table(cut(alighting$timestamp, breaks="month"))
    }
    
    boarding <- data.frame(boarding)
    boarding$Group <- paste0("Boarding from ", busStop)
    colnames(boarding) <- c("timeFrame", "Count", "Group")
    alighting <- data.frame(alighting)
    alighting$Group <- paste0("Alighting from ", busStop)
    colnames(alighting) <- c("timeFrame", "Count", "Group")
    combined <- rbind(alighting, boarding)
    
    if (timeFrame != "Hourly") {
      # if break = hour then only time remaining
      combined$timeFrame <- as.Date(combined$timeFrame)
      boardingForecastTimeFrame <- data.frame(as.Date(tail(boarding$timeFrame,2))+2)
      boardingForecastTimeFrame <- rbind(list(tail(boarding$timeFrame, nrow(boarding)-2)), boardingForecastTimeFrame)
      alightingForecastTimeFrame <- data.frame(as.Date(tail(alighting$timeFrame,2))+2)
      alightingForecastTimeFrame <- rbind(list(tail(alighting$timeFrame, nrow(alighting)-2)), alightingForecastTimeFrame)
    } else {
      boardingForecastTimeFrame <- data.frame(as.POSIXct(tail(boarding$timeFrame,2))+2*60*60)
      boardingForecastTimeFrame <- rbind(list(tail(boarding$timeFrame, nrow(boarding)-2)), boardingForecastTimeFrame)
      alightingForecastTimeFrame <- data.frame(as.POSIXct(tail(alighting$timeFrame,2))+2*60*60)
      alightingForecastTimeFrame <- rbind(list(tail(alighting$timeFrame, nrow(alighting)-2)), alightingForecastTimeFrame)
    }
    
    boardingSMA <- data.frame(boardingForecastTimeFrame, SMA(ts(boarding$Count), 3), paste0("Forecasted boarding from ", busStop))
    colnames(boardingSMA) <- c("timeFrame", "Count", "Group")
    boardingSMA <- boardingSMA[3:nrow(boardingSMA),]
    alightingSMA <- data.frame(alightingForecastTimeFrame, SMA(ts(alighting$Count), 3), paste0("Forecasted alighting from ", busStop))
    colnames(alightingSMA) <- c("timeFrame", "Count", "Group")
    alightingSMA <- alightingSMA[3:nrow(alightingSMA),]
    combinedSMA <- rbind(alightingSMA, boardingSMA)
    combinedSMA$timeFrame <- format(combinedSMA$timeFrame)
    
    combined <- rbind(combined, combinedSMA)
    combined$timeFrame <- as.character(combined$timeFrame)

    if (timeFrame == "Hourly") {
      combined$timeFrame <- substring(combined$timeFrame, 12, 16)
    } else if (timeFrame == "Daily" || timeFrame == "Weekly") {
      combined$timeFrame <- substring(combined$timeFrame, 6, 10)
    } else if (timeFrame == "Monthly") {
      combined$timeFrame <- substring(combined$timeFrame, 0, 7)
    }
  })
  
  output$testText <- renderText({
    getBoarding()
  })
  
  output$testTable <- DT::renderDataTable({
    getTable()
  })
  
}
