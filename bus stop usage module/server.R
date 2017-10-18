library(shiny)
library(mongolite)
library(jsonlite)
library(DT)
library(ggplot2)

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
  
  getBoarding <- eventReactive(input$genResult, {
    if (input$timeFrame == "Hourly") {
      query <- paste0('{"sourceBusStop":"', isolate(input$busStop), '","timestamp":{"$gte":{"$date":"', getStartDate(input$startDate1), '"},"$lt":{"$date":"', getEndDate(input$startDate1+1),'"}}}')
    } else if (input$timeFrame == "Monthly") {
      query <- paste0('{"sourceBusStop":"', isolate(input$busStop), '","timestamp":{"$gte":{"$date":"', getStartDate(input$startDate2), '"},"$lt":{"$date":"', getEndDate(input$endDate+1), '"}}}')
    } else {
      query <- paste0('{"sourceBusStop":"', isolate(input$busStop), '","timestamp":{"$gte":{"$date":"', getStartDate(input$startDate2), '"},"$lt":{"$date":"', getEndDate(input$endDate+1), '"}}}')
    }
    queryList$find(query)
  })
  
  getAlighting <- eventReactive(input$genResult, {
    if (input$timeFrame == "Hourly") {
      query <- paste0('{"destinationBusStop":"', isolate(input$busStop), '","timestamp":{"$gte":{"$date":"', getStartDate(input$startDate1), '"},"$lt":{"$date":"', getEndDate(input$startDate1+1),'"}}}')
    } else if (input$timeFrame == "Monthly") {
      query <- paste0('{"destinationBusStop":"', isolate(input$busStop), '","timestamp":{"$gte":{"$date":"', getStartDate(input$startDate2), '"},"$lt":{"$date":"', getEndDate(input$endDate+1), '"}}}')
    } else {
      query <- paste0('{"destinationBusStop":"', isolate(input$busStop), '","timestamp":{"$gte":{"$date":"', getStartDate(input$startDate2), '"},"$lt":{"$date":"', getEndDate(input$endDate+1), '"}}}')
    }
    queryList$find(query)
  })
  
  output$boarding <- renderText({
    print(nrow(getBoarding()))
  })
  
  output$alighting <- renderText({
    print(nrow(getAlighting()))
  })
  
  output$plot <- renderPlot({
    busStop <- isolate(input$busStop)
    timeFrame <- isolate(input$timeFrame)
    boarding <- getBoarding()
    alighting <- getAlighting()
    
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
  
  output$testText <- renderText({
    
  })
  
  output$testTable <- DT::renderDataTable({
    
  })
}
