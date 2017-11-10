#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(jsonlite)
library(mongolite)
library(ggplot2)

# databaseName <- "myshinydatabase"
# databaseUrl <- "mongodb://127.0.0.1:27017"

databaseName <- "trr"
databaseUrl <- "mongodb://soraares:bt3103@therouteranger-shard-00-00-rgv6u.mongodb.net:27017,therouteranger-shard-00-01-rgv6u.mongodb.net:27017
,therouteranger-shard-00-02-rgv6u.mongodb.net:27017/test?ssl=true&replicaSet=TheRouteRanger-shard-0&authSource=admin"

dbDyResponses <- mongo(collection = "dynamicResponses",db = databaseName, url = databaseUrl )
dbDyAvgVol <- mongo(collection = "dynamicAvgVol",db = databaseName, url = databaseUrl )
dbAvgVol <- mongo(collection = "avgVol",db = databaseName, url = databaseUrl )

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         actionButton("insertData", "Insert Data")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         selectInput("busId", "Bus ID:", 1:62),
         plotOutput("riggedData"),
         plotOutput("realTimeData")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  # observeEvent(input$insertData, { #Insert data immediately when app starts
  #   #Stopped at busID: 30 
  #   #Max busID: 62
  #   data <- generateRiggedData(3, 200)
  #   for(i in 1:5) {
  #    data <- rbind(data, generateRiggedData(i, 200))
  #    print("count:")
  #    print(nrow(data))
  #   }
  #   ####### Write to a JSON file ####### 
  #   # data_JSON <- toJSON(data, auto_unbox = TRUE)
  #   # print("JSON")-
  #   # print(data_JSON)
  #   # write(data_JSON, file="rigged avgVol(ZJ).JSON")
  #   
  #   print("READY")
  #   insertAvgVol(data)
  #   print("DONE")
  # })
  
  output$riggedData <- renderPlot({

    data <- generateRiggedData(1, 200)
    for(i in 2:5) { # Depends on No. of busId, 10 for demo purposes
    data <- rbind(data, generateRiggedData(i, 200))
    print("count:")
    print(nrow(data))
  }
    observeEvent(input$insertData, {
      
      ####### Write to a JSON file ####### 
      data_JSON <- toJSON(data, auto_unbox = TRUE)
      print("JSON")
      print(data_JSON)
      # write(data_JSON, file="rigged avgVol(ZJ).JSON")
      
      ########## Uncomment this to insert data ############# 
      #insertAvgVol(data) 
      print("DONE")
    })

    ggplot(data[grep(input$busId, data$busId),], aes(x=factor(startStop, levels=unique(startStop)), y=avgVol)) +
      geom_boxplot(outlier.shape = NA) +
      scale_y_continuous(limits = c(0, 40)) +
      labs(x = "Bus Stops") +
      ggtitle(paste0(input$busService)) +
      theme(axis.text.x = element_text(color="black", size=10, angle=45, vjust = 0.5))
  })

  # output$realTimeData <- renderPlot({
  # 
  #   data <- generateRiggedData("sample", 50)
  #   #data_JSON <- toJSON(data, auto_unbox = TRUE)
  #   #print(data_JSON)
  #   #write(data_JSON, file="rigged avgVol(ZJ).JSON")
  #   ggplot(data, aes(x= factor(startStop, levels=unique(startStop)), y = avgVol, ymin = 0, ymax = 40, fill = avgVol)) +
  #     scale_y_continuous(limit = c(0, 40), expand = c(0,0)) + scale_x_discrete(expand = c(0,0)) +
  #     geom_bar(stat = "identity") + labs(x = "Bus Stops") +
  #     theme(axis.text.x = element_text(color="black", size=10, angle=45, vjust = 0.5)) +
  #     scale_fill_continuous(low="green", high="red") +
  #     theme(plot.title = element_text(family = "Comic Sans MS", color="tomato", face="bold", size=28))
  # })
  
  output$test <- renderPlot({
    
    data <- generateRiggedData()
    
    ggplot(data, aes(timestamp, busCapacity), ymin = 1, ymax = 40) + 
      geom_line() +
      scale_x_datetime(breaks = date_breaks("1 week"), date_labels = "%H:%M") +  #Scales the axis
      theme(axis.text.x = element_text(color="black", size=10, angle=45, vjust = 0.5)) +
      labs(x = "Time/s", y="Number of people on the bus")+geom_point() +
      scale_color_brewer(palette = "RdBu") +
      theme(panel.background=element_rect(fill="lightblue"))
  })
  
  generateRiggedData <- function(busId, numOfSample, type = "") {
    
    inMean <- 10
    inSd <- 3
    counter <- 0
    
    stops <- data.frame("startStop" = c("PGP","Kent_Ridge_MRT","NUH", "LT29" , "UHall", "Opp_UHC", "YIH", "Central_Library", "LT13", "AS7", "COM2",
                                        "BIZ2", "PGP_Hse_12", "PGP_Hse_7", "PGP"))
    busStop <- data.frame("startStop" = "")[FALSE,]
    busCapacity <- data.frame("avgVol" = "")[FALSE,]
    timestamp <- data.frame("timestamp")
    # randPlot <- rnorm(runif(length(stops$startStop), pnorm(0, mean=0, sd=1), pnorm(1, mean=0, sd=1)), mean=0, sd=1)
    # randPlot <- data.frame(randPlot)
    # print(randPlot)
    # plot(randPlot)
    for(stop in stops$startStop) { #loops through each busStop
      
      for (i in 1:(numOfSample))  { #generates busStop for each value
        busStop <- rbind(busStop, data.frame("startStop" = stop))
        
      }
      #print(head(busStop))
      busCapacity <- rbind(busCapacity, data.frame("avgVol" = rnorm(numOfSample, mean = inMean, sd = inSd)))
      
      #print(stop == stops[(nrow(stops)/2-3),])
      if(counter == 0){
        inMean <- inMean + runif(1, 0.5, 1.5)*8 # Randomly increase mean
        inSd <- inSd*runif(1, 0.3, 2) # Randomly change sd (increase/decrease)
        if(stop == stops[nrow(stops)/2-3,]) {
          counter <- 1
        }
      }
      else {
        inMean <- inMean - runif(1, 0.5, 1.5)*3 # Randomly decrease mean
        inSd <- inSd*runif(1, 0.3, 2) # Randomly change sd (increase/decrease)
      }
    }
    data <- cbind("busId" = rep(busId,length(busStop)), "busService" = rep("A1",length(busStop)), 
                  busStop, busCapacity, "timestamp" = rep(myTimestamp(),length(busStop)))
                
    if(type == "sample") {
      realTimeAvgVol <- data.frame("startStop" = "", "avgVol" = 0)[FALSE,]
      for(stop in stops$startStop) {
        temp <- data[data$startStop == stop,]
        avgVol <- ceiling(aggData(temp$avgVol))
        realTimeAvgVol <- rbind(realTimeAvgVol, data.frame("startStop" = stop, "avgVol" = avgVol))
        if(stop == stops[nrow(stops)/2,]) {
          break
        }
      }
      return(realTimeAvgVol)
    }
    return(data)
  }
  
  #Pre-cond: JSON of avgVol of buses
  #Post-cond: Inserts JSON of avgVOl of buses into database
  insertAvgVolTrend <- function(data) {
    dbAvgVolTrend$insert(data)
  }# End of insertAvgVolTrend
  
  #Pre-cond: JSON of avgVol of buses
  #Post-cond: Inserts JSON of avgVOl of buses into database
  insertAvgVol <- function(data) {
    dbAvgVol$insert(data)
  }# End of insertAvgVol
  
  #Pre-cond: JSON file of user response
  #Post-cond: Average Bus Capacity as integer
  aggData <- function(data) {
    #Rounds off average value to 2 decimal places
    avgVol <- round(mean(data), 2)
    avgVol
  }# End of aggData()
  
  #Pre-cond: nil
  #Post-cond: returns ISODate format of date and time
  myTimestamp <- function() {
    year <- format(Sys.time(), "%Y")
    month <- format(Sys.time(), "%m")
    day <- format(Sys.time(), "%d")
    hour<- format(Sys.time(), "%H")
    min <- format(Sys.time(), "%M")
    sec <- format(Sys.time(), "%OS")
    newDate <- ISOdate(year, month, day, hour, min, sec)
  }# End of myTimestamp()
}

# Run the application 
shinyApp(ui = ui, server = server)

