########################## Crowdsourcing of Bus Volume ########################## 

library(shiny)
library(data.table)
library(DT)
library(mongolite)
library(jsonlite)
library(ggplot2)

#### ASSUMPTIONS ####
# 1. App users only submit busCapacity when they are onboard the bus
# 2. Riders submits the busCapacity immediately after boarding the bus
# 3. All new riders submit busCapacity within 30s after bus moves off
# 4. Every busstop has at least one rider to submit the busCapacity, except for last stop

#### POTENTIAL PROBLEMS ####
# 1. Unable to handle for multiple buses of the same bus service
#    (Probably having a BusID will help - Solved)


# Define the response we want to save from the form
dyAvgVolTable <- c("busId","busService", "startStop", "endStop", "dyAvgVol", "timestamp")
avgVolTable <- c("busId", "busService", "startStop", "avgVol", "timestamp")
responseTable <- c("busId", "busService", "startStop", "endStop", "busCapacity", "timestamp")
# Shiny app with crowdsourcing form

databaseName <- "trrdb"
databaseUrl <- "mongodb://localhost"

dbDyResponses <- mongo(collection = "dynamicResponses",db = databaseName, url = databaseUrl )

dbResponses <- mongo(collection = "responses",db = databaseName, url = databaseUrl )

dbDyAvgVol <- mongo(collection = "dynamicAvgVol",db = databaseName, url = databaseUrl )

dbAvgVol <- mongo(collection = "avgVol",db = databaseName, url = databaseUrl )

#BEN Database
#Load testRoute <- database with route details
#routeidx <- mongo(db="trrdb", collection="testRoute", url = "mongodb://localhost")
routeidx <- mongo(db=databaseName, collection="testRoute", url = databaseUrl)

#Load testTime <- database with starting time
#stime <-mongo(db="trrdb", collection="testTime", url = "mongodb://localhost")
stime <-mongo(db=databaseName, collection="testTime", url = databaseUrl)

#Load finalstops (second last and last stop)
#finale <-mongo(db="trrdb", collection="end", url = "mongodb://localhost")
finale <-mongo(db=databaseName, collection="end", url = databaseUrl)

#To Save Query DB (ZONGJIE DO NOT TOUCH)
querydb <- mongo(db="trrdb", collection="queryBase", url= databaseUrl)

ui = navbarPage( "Route Range", fluid = TRUE,
                 tabPanel("Crowdsourcing",
                          column(4, selectInput(inputId = "busService", label = "Choose Your Bus", c("A1"="A1","A2"="A2","D1"="D1","D2"="D2")),
                                 
                                 selectInput(inputId = "startStop", label = "Choose Your Starting Bus Stop", c("PGP"="PGP",
                                                                                                               "Kent_Ridge_MRT"="Kent_Ridge_MRT",
                                                                                                               "NUH"="NUH",
                                                                                                               "LT29"="LT29",
                                                                                                               "UHall"="UHall",
                                                                                                               "Opp_UHC"="Opp_UHC",
                                                                                                               "YIH"="YIH",
                                                                                                               "Central_Library"="Central_Library",
                                                                                                               "LT13"="LT13",
                                                                                                               "AS7"="AS7",
                                                                                                               "COM2"="COM2",
                                                                                                               "BIZ2"="BIZ2",
                                                                                                               "PGP_Hse_14_15"="PGP_Hse_14_15",
                                                                                                               "PGP_Hse_12"="PGP_Hse_12",
                                                                                                               "PGP_Hse_7"="PGP_Hse_7",
                                                                                                               "Opp_HSSML"="Opp_HSSML",
                                                                                                               "Opp_NUSS"="Opp_NUSS",
                                                                                                               "Ventus"="Ventus",
                                                                                                               "Computer_Centre"="Computer_Centre",
                                                                                                               "Opp_YIH"="Opp_YIH",
                                                                                                               "Museum"="Museum",
                                                                                                               "UHC"="UHC",
                                                                                                               "Opp_UHall"="Opp_UHall",
                                                                                                               "S17"="S17",
                                                                                                               "Opp_NUH"="Opp_NUH",
                                                                                                               "Opp_Kent_Ridge_MRT"="Opp_Kent_Ridge_MRT",
                                                                                                               "PGPR"="PGPR",
                                                                                                               "CP11"="CP11",
                                                                                                               "UTown"="UTown")),
                                 
                                 selectInput(inputId = "endStop", label = "Choose Your Destination Bus Stop", c("PGP"="PGP",
                                                                                                                "Kent_Ridge_MRT"="Kent_Ridge_MRT",
                                                                                                                "NUH"="NUH",
                                                                                                                "LT29"="LT29",
                                                                                                                "UHall"="UHall",
                                                                                                                "Opp_UHC"="Opp_UHC",
                                                                                                                "YIH"="YIH",
                                                                                                                "Central_Library"="Central_Library",
                                                                                                                "LT13"="LT13",
                                                                                                                "AS7"="AS7",
                                                                                                                "COM2"="COM2",
                                                                                                                "BIZ2"="BIZ2",
                                                                                                                "PGP_Hse_14_15"="PGP_Hse_14_15",
                                                                                                                "PGP_Hse_12"="PGP_Hse_12",
                                                                                                                "PGP_Hse_7"="PGP_Hse_7",
                                                                                                                "Opp_HSSML"="Opp_HSSML",
                                                                                                                "Opp_NUSS"="Opp_NUSS",
                                                                                                                "Ventus"="Ventus",
                                                                                                                "Computer_Centre"="Computer_Centre",
                                                                                                                "Opp_YIH"="Opp_YIH",
                                                                                                                "Museum"="Museum",
                                                                                                                "UHC"="UHC",
                                                                                                                "Opp_UHall"="Opp_UHall",
                                                                                                                "S17"="S17",
                                                                                                                "Opp_NUH"="Opp_NUH",
                                                                                                                "Opp_Kent_Ridge_MRT"="Opp_Kent_Ridge_MRT",
                                                                                                                "PGPR"="PGPR",
                                                                                                                "CP11"="CP11",
                                                                                                                "UTown"="UTown")),
                                 actionButton(inputId = "submitQ", label = "Submit"),
                                 
                                 sliderInput("busCapacity", "Bus Capacity", 1, 3, 0),
                                 actionButton("submitV", "submit"), 
                                 actionButton("clear", "Clear"), 
                                 h2(textOutput("stats"), style = "color: Blue;"),
                                 textOutput("var"), 
                                 textOutput("timestamp"),
                                 plotOutput("ma"),
                                 hr()
                          ),
                          column(8, offset = 0, plotOutput("graph")),
                          column(4, actionButton("update", "Update Plot")),
                          column(12, dataTableOutput("responses", width = 300), tags$hr())
                          
                 ),
                 tabPanel("New Tab"
                          #plotOutput("busVol")
                          #dataTableOutput("stats", width = 300, tags$hr())
                          #dataTableOutput("tData", width = 300, tags$hr())
                 )
)

server = function(input, output, session) {
  
  #STARTBEN ------------------------------------------------------->
  
  observeEvent(input$submitQ,{
    
    # getCurrentTime = reactive({
    #   #timeNow Manipulation ------------------------------------------------ben
    #   getTime() # Changed - ZJ
    #   #end of getting timeNow-----------------------------------------------ben
    # })
    # 
    # #NEW CHANGES 
    # gettimeOnly = reactive({
    #   #timeOOnly Manipulation ------------------------------------------------ben
    #   datetime <- Sys.time()
    #   datetime <- as.character(datetime)
    #   tnow<- as.character(substr(datetime,12,16))
    #   return(tnow)
    #   #end of getting timeonly-----------------------------------------------ben
    # })
    # 
    # 
    # getdateOnly = reactive({
    #   #timeOOnly Manipulation ------------------------------------------------ben
    #   datetime <- Sys.time()
    #   datetime <- as.character(datetime)
    #   dnow<- as.character(substr(datetime,1,10))
    #   return(dnow)
    #   #end of getting timeonly-----------------------------------------------ben
    # })
    # 
    # getTimeQ <- reactive({return(gettimeOnly())})
    # getDateQ <- reactive({return(getdateOnly())})
    # #END OF TIME CHANGES
    
    #Start of select data -------------------------------------------------->
    
    selectdata = reactive({
      ctr <- 0
      realeta <- 0
      flag =TRUE
      
      firstbus <- loadtime(isolate(input$busService))
      firstbusTime <- firstbus["start"][1,] #first bus time
      
      stopIndex <- loadindex(isolate(input$startStop))
      currIndex <- stopIndex[isolate(input$busService)][1,]
      if (currIndex == 0) { realeta <- "Wrong Inputs"}
      
      else {
        while(flag){
          eta <- (firstbusTime + (15*ctr) + ((currIndex-1)*5)) - getTime()
          if(eta > 0) {
            flag = FALSE
            realeta <- as.character(eta)
          }#endif
          
          else {
            ctr <- ctr +1
          }#endelse
        }#end of while
      }#end of else
      
      return(realeta)
    }) #end of selectdata -------------------------------------------->
    
    #START GetBUs ---------------------------------------------------->
    getBus = reactive({
      getBusId() # Changed - ZJ
    }) #end of getBUS -------------------------------------------->
    

    #for zongjie part
    queryTable <- c("bus","stopId","busIdx","realidx","timeArr","pETA","rETA","sourceBusStop","destinationBusStop","timestamp")
    
    insertQuery <- eventReactive(input$submitQ, {
      
      queryTable$bus <- isolate(input$busService)
      queryTable$stopId <- isolate(input$startStop)
      queryTable$busIdx <- (getBus())%%7 #numeric
      queryTable$realidx <-  getBus()
      # queryTable$dateQ <- as.character(getDateQ())#added
      # queryTable$timeQ <- as.character(getTimeQ()) #
      queryTable$timeArr <- ""
      queryTable$pETA <- as.numeric(selectdata())
      queryTable$rETA <- 0
      
      queryTable$sourceBusStop <- isolate(input$startStop)
      queryTable$destinationbusStop <- isolate(input$endStop)
      queryTable$timestamp <- Sys.time()
      #queryTable$timestamp <- queryTable$timestamp <- paste0('{"$date": "',substring(as.character(Sys.time()), 0, 10),'T', substring(as.character(Sys.time()), 12, 19), 'Z','"}')
      
      
      insertData <- toJSON(queryTable[c("bus","stopId","busIdx","realidx","timeArr","pETA","rETA","sourceBusStop","destinationBusStop","timestamp")],auto_unbox = TRUE)
    })
    #end of sending data to db
    
    #send data to db START
    if(selectdata() == "Wrong Inputs"){}
    else{saveResponses(insertQuery())}
    #send data to db END
   
    #output
    output$timestamp <- renderText({ 
      input$submitQ
      string <-  Sys.time()
      paste(string)
    })
    
    output$var <- renderText({ 
      input$submitQ
      realeta = selectdata()
      paste("Your ETA is", realeta, "minutes")
    })
    #endOutput
    
  })#end of Q
  
    observeEvent(input$submitV, {
      
      getActualTime = reactive({
        dtime <- Sys.time()
        dtime <- as.character(dtime)
        anowhr <- as.numeric(substr(dtime,12,13))
        anowmin <- as.numeric(substr(dtime,15,16))
        atimeNow <- (anowhr*60)+(anowmin)
        return(atimeNow)
      })# get time now in minutes
      
      arrTime <- getActualTime() #use in for loop
      
      queryData <- loadquery() #generate list of query
      numQuery <- nrow(queryData) #number of queries(rows)
      #print(numQuery)
      #print(queryData)
      #print(queryData[1,])
      
      getMins <- function(data) {
        hour <- as.numeric(substr(data,12,13))
        min <- as.numeric(substr(data,15,16))
        timequery <- (hour*60)+(min)
        return(timequery)
      }#end of getMins, throw in hh:mm to get back mins
      
      whatBusIdx <- function(date) {
        ctr <- 0
        realeta <- 0
        flag =TRUE
        
        firstbus <- loadtime(isolate(input$busService))
        firstbusTime <- firstbus["start"][1,] #first bus time
        whatTime <- getMins(date)
        stopIndex <- loadindex(isolate(input$startStop))
        currIndex <- stopIndex[isolate(input$busService)][1,]
        if (currIndex == 0) { realeta <- "Wrong Inputs"}
        
        else {
          while(flag){
            eta <- (firstbusTime + (15*ctr) + ((currIndex-1)*5)) - whatTime
            if(eta > 0) {
              flag = FALSE
              realeta <- as.character(eta)
            }#endif
            
            else {
              ctr <- ctr +1
            }#endelse
          }#end of while
        }#end of else
        #add new modulo
        return(ctr)
      }
      #return the correct busIdx
      
      for (i in 1:numQuery){ #
        instance <- queryData[i,]
        qtime <- getMins(queryData[i,]["timestamp"][1,])
        queryData[i,]["timeArr"][1,] <- arrTime
        if (as.character(substr((queryData[i,]["timestamp"][1,]),1,10)) == getDateQ()){
          if (queryData[i,]["realidx"][1,]==whatBusIdx(queryData[i,]["timestamp"][1,])){
            reta <- arrTime - qtime
            queryData[i,]["rETA"][1,] <- reta
          }
        }
      }
      
      print(queryData)
      
      dataF<- data.frame(cbind(queryData["timestamp"],queryData["rETA"]))
      
      dataF<- cbind(ma=0,dataF)
      
      findUnique<- function(data, num){
        val <- 0
        comp <- 0
        ctr <- 0
        for(i in 1:num) {
          if (ctr >= 3){break}
          v <- data[num-i,]["rETA"][1,]
          if (comp <= v){
            comp <- v
            ctr <- ctr +1
            val <- val + comp
            
          }
          else{}
        }
        final <-  val/ctr
        return(final)
      }#findUnique end
      
      if(nrow(dataF)>4){
        for (row in 4:nrow(dataF)) {
          dataF[row,]["ma"][1,] <- findUnique(dataF,row)
        }#end of row manipulation
        dataF[1,]["ma"][1,] <- dataF[4,]["ma"][1,]
        dataF[2,]["ma"][1,] <- dataF[4,]["ma"][1,]  
        dataF[3,]["ma"][1,] <- dataF[4,]["ma"][1,]  
        
      }
      
      #print(dataF)
      output$ma <- renderPlot(
        ggplot(data=dataF,aes(x=timestamp,color="black",group="black"))+geom_line(aes(y=rETA),color="black")
        +geom_line(aes(y=ma),color="red")
      )
      

      
      
      # dataF<- data.frame(queryData$timeQ,queryData$rETA)
      # 
      # plotMA <- function(){
      #   plot(dataF)
      #   sma <- ma(dataF,order=3)
      #   lines(sma,col="red")
      # }
      # 
      #  
      # plotMA()
      
    }) #end of observerEvent submitV
    

    
    
  
  
  #ENDBEN --------------------------------------------------------->
  
  loadStops <- function(filter) { #all the bus stops
    allStopsAvail <- finale$find(query = toString(toJSON(list(key="key"),auto_unbox = TRUE)))
    answer <- list(serviceAVail["list"][1,]) 
  }#loadStops
  
  loadService <- function(filter) { #load bus available
    serviceAvail <- routeidx$find(query = toString(toJSON(list(key="key"),auto_unbox = TRUE)))
    num <-  nrow(serviceAvail)
    vector <- c()
    pointer <- serviceAvail[1,]["A1"][1,]
    if(pointer > 0) { vector[1] <- "A1" }
    pointer <- serviceAvail[2,]["A2"][1,]
    if(pointer > 0) { vector[2] <- "A2" }
    pointer <- serviceAvail[3,]["D1"][1,]
    if(pointer > 0) { vector[3] <- "D1" }
    pointer <- serviceAvail[4,]["D2"][1,]
    if(pointer > 0) { vector[4] <- "D2" }
    
    return(list(vector))
    
  }#loadService
  
  
  
  
  #Pre-cond: Waits for submitV button to be depressed
  #Post-Cond: Returns a JSON file of user responses
  formData <- eventReactive(input$submitV, {
    
    responseTable$busId <- getBusId()
    responseTable$busService <- input$busService
    responseTable$startStop <- input$startStop
    responseTable$endStop <- input$endStop
    responseTable$busCapacity <- input$busCapacity
    responseTable$timestamp <- myTimestamp()
    # Converts List to JSON, and unboxes each element to primitive
    data <- toJSON(responseTable[c("busId", "busService", "startStop", "endStop",
                                   "busCapacity", "timestamp")], auto_unbox = TRUE)
  })
  
  #Waits for Submit button to be depressed
  observeEvent(input$submitV, {
    # Remove response(s) where startStop of User equals the destination stop
    # of other users AND the busService of all responses are the same. 
    # (When the bus reached the endStop of other users)
    dbDyResponses$remove(query = toString(toJSON(list(endStop = input$startStop,
                                                      busService = input$busService,
                                                      busId = getBusId()),
                                                 auto_unbox = TRUE))) #TBC filter include BusId
    insertResponse(formData()) #Inserting data into database
    insertDyResponse(formData()) #Inserting data into database
  })
  
  #Pre-cond: Waits for submit button to be depressed
  #Post-Cond: Retrieves all user responses, and returns a JSON file with
  #           the calculated avgVol 
  dyAvgVolData <- eventReactive(input$submitV, {
    dyAvgVolTable$busId <- getBusId()
    dyAvgVolTable$busService <- input$busService
    dyAvgVolTable$dyAvgVol <- aggData(loadDyResponses("all", getBusId())) #loadDyResponses(filterBusService, busId)
    dyAvgVolTable$startStop <- input$startStop
    dyAvgVolTable$endStop <- input$endStop
    dyAvgVolTable$timestamp <- myTimestamp() #ISODate timestamp
    # Converts List to JSON, and unboxes each element from List to primitive
    data <- toJSON(dyAvgVolTable[c("busId","busService", "startStop", "endStop",
                                   "dyAvgVol", "timestamp")], auto_unbox = TRUE)
  })
  
  #Convenient Deletion of all rows - will be removed
  observeEvent(input$clear, {
    
    data <- querydb$remove('{}')
    
    dbDyResponses$remove(query = "{}")
    dbResponses$remove(query = "{}")
    dbDyAvgVol$remove(query = "{}")
    dbAvgVol$remove(query = "{}")
    # Inserts buffer data
    dbDyAvgVol$insert(toJSON(list(busId = 0, busService = input$busService, startStop = "", endStop = " ", 
                                  dyAvgVol = 0), auto_unbox = TRUE))
    dbDyAvgVol$insert(toJSON(list(busId = 0, busService = input$busService, startStop = "", endStop = " ", 
                                  dyAvgVol = 0), auto_unbox = TRUE))
    dbDyAvgVol$insert(toJSON(list(busId = 0, busService = input$busService, startStop = "", endStop = " ", 
                                  dyAvgVol = 0), auto_unbox = TRUE))
  })
  
  #Pre-cond: Integer in millisecond
  #Post-cond: Reactive countdown timer 
  autoInvalidate <- function(timeMS) {
    invalidateLater(timeMS, session)
  }
  
  observe({
    #Pre-cond: Waits for submitV button to be depressed
    # #Post-cond: Renders data table of all previous responses
    output$responses <- renderDataTable({
      input$submitV
      loadDyResponses("all", getBusId()) # #loadDyResponses(filterBusService, busId)
    })
  })
  
  #Pre-cond: Waits for submitV button to be depressed
  #Post-cond: render plot of avgVol over time/ busstop
  output$graph <- renderPlot({
    input$update
    autoInvalidate(10000) #10000ms = 10s
    # if not (Secondlast Stop && number of rows in average is 3)
    if(!(input$startStop == getSecondLastStop(input$busService) && 
         nrow(loadDyAvgVol(input$busService)) == 3)) { #loadDyResponses(filterBusService, busId)
      isolate(insertDyAvgVol(dyAvgVolData()))
    }
    average <- loadDyAvgVol(input$busService)
    print("average")
    print(average)
    ########## Algorithm to extract sampled avgVol at each busStop for plotting ##########
    print("True/False")
    print(nrow(average) > 2 && #Ensures that the last 2 entries are equal and the last 3rd entry is different to determine when to sample
            (average['dyAvgVol'][nrow(average),] == average['dyAvgVol'][nrow(average)-1,] &&
               average['dyAvgVol'][nrow(average)-1,] != average['dyAvgVol'][nrow(average)-2,] ))
    
    if(average['startStop'][nrow(average)-1,] == average['endStop'][nrow(average)-1,]) { #check if false data has been inserted
      print("######################## in2 ########################")
      insertAvgVol(toJSON(list(busId = getBusId(), busService = average['busService'][1,], startStop = getLastStop(input$busService),  
                               avgVol = 1, timestamp = myTimestamp()), auto_unbox = TRUE)) #inserts value for avgVol at Last Stop. TBC
      dbDyAvgVol$remove(query = "{}") #clears false data
      # Inserts buffer data
      dbDyAvgVol$insert(toJSON(list(busId = 0, busService = input$busService, startStop = "", endStop = " ", 
                                    dyAvgVol = 0), auto_unbox = TRUE))
      dbDyAvgVol$insert(toJSON(list(busId = 0, busService = input$busService, startStop = "", endStop = " ", 
                                    dyAvgVol = 0), auto_unbox = TRUE))
      dbDyAvgVol$insert(toJSON(list(busId = 0, busService = input$busService, startStop = "", endStop = " ", 
                                    dyAvgVol = 0), auto_unbox = TRUE))
    }
    else if(nrow(average) > 2 && #Ensures that the last 2 entries are equal and the last 3rd entry is different to determine when to sample
            (average['dyAvgVol'][nrow(average),] == average['dyAvgVol'][nrow(average)-1,] &&
             average['dyAvgVol'][nrow(average)-1,] != average['dyAvgVol'][nrow(average)-2,] )) {
      
      avgVolTable$busId <- average['busId'][nrow(average),]
      avgVolTable$busService <- average['busService'][nrow(average),]
      avgVolTable$startStop <- average['startStop'][nrow(average),]
      avgVolTable$avgVol <- average['dyAvgVol'][nrow(average),]
      avgVolTable$timestamp <- average['timestamp'][nrow(average),]
      data <- toJSON(avgVolTable[c("busId", "busService", "startStop", "avgVol", 
                                   "timestamp")], auto_unbox = TRUE)
      insertAvgVol(data)
    }
    plotAvgVol <- loadAvgVol(input$busService) #loadAvgVol(filterBusService)
    #Checks when the bus has reached second last stop
    if(nrow(plotAvgVol) != 0 && plotAvgVol['startStop'][nrow(plotAvgVol),] == getSecondLastStop(input$busService)) { #second last stop. TBC
      print("######################## in1 ########################")
      dbDyAvgVol$remove(query = toString(toJSON(list(busService = input$busService, busId = getBusId()), auto_unbox = TRUE))) #TBC filter include BusId
      dbDyAvgVol$insert(toJSON(list(busId = 0, busService = input$busService, startStop = "last", endStop = "last", 
                                    dyAvgVol = 1), auto_unbox = TRUE)) #Inserts false data to signal reaching last stop
    }
    ####################################################################################
    print("plotAvgVol")
    print(plotAvgVol)
    
    if(nrow(plotAvgVol) != 0) {
      subPlot <- (subset(plotAvgVol, busId == getBusId()))
      print(subset(plotAvgVol, busId == getBusId()))
      print(subPlot)
      
      if(nrow(subPlot) != 0){
        ggplot(data=subPlot, aes(x= factor(startStop, levels=unique(startStop)), y = avgVol, ymin = 1, ymax = 3)) + 
          scale_y_continuous(limit = c(0, 3), expand = c(0,0)) + scale_x_discrete(expand = c(0,0)) +
          geom_bar(stat = "identity", fill = "grey") + labs(x = "Bus Stops") +
          ggtitle(paste0(input$busService)) + 
          theme(plot.title = element_text(family = "Comic Sans MS", color="tomato", face="bold", size=28))
      }
    }
  })
  
  
  ###################### FUNCTIONS ######################
  
  #Pre-cond: Waits for submitV button to be depressed
  #Post-cond: Renders mean value of Bus Capacity as Text
  output$stats <- renderText({
    input$submitV 
    avg <- aggData(loadDyResponses(input$busService, getBusId())) # #loadDyResponses(filterBusService, busId)
    print(avg)
    paste("Mean: ", avg)
  })
  
  getLastStop <- function(busService){ # Changed - ZJ
    busStop <- finale$find(query = toString(toJSON(list(busService = input$busService), auto_unbox = TRUE)))['last'][1,]
  }
  
  getSecondLastStop <- function(busService){ # Changed - ZJ
    busStop <- finale$find(query = toString(toJSON(list(busService = input$busService), auto_unbox = TRUE)))['secondlast'][1,]
  }
  
  #Pre-cond: nil
  #Psot-cond: returns ISODate format of date and time
  myTimestamp <- function() {
    year <- format(Sys.time(), "%Y")
    month <- format(Sys.time(), "%m")
    day <- format(Sys.time(), "%d")
    hour<- format(Sys.time(), "%H")
    min <- format(Sys.time(), "%M")
    sec <- format(Sys.time(), "%OS")
    newDate <- ISOdate(year, month, day, hour, min, sec)
  }
  
  #Pre-cond: JSON of user responses
  #Post-cond: Inserts JSON of user response into database
  insertDyResponse <- function(data) {
    dbDyResponses$insert(data)
  }
  
  #Pre-cond: JSON of user responses
  #Post-cond: Inserts JSON of user response into database
  insertResponse <- function(data) {
    dbResponses$insert(data)
  }
  
  #Pre-cond: JSON of avgVol of buses
  #Post-cond: Inserts JSON of avgVOl of buses into database
  insertDyAvgVol <- function(data) {
    dbDyAvgVol$insert(data)
  }
  
  #Pre-cond: JSON of avgVol of buses
  #Post-cond: Inserts JSON of avgVOl of buses into database
  insertAvgVol <- function(data) {
    dbAvgVol$insert(data)
  }
  
  #Pre-cond: Allows querying by "A1", "A2", "D1", "D2", "all"
  #Post-cond: Reads responses by user with specified query
  #           and returns a dataframe of the result
  loadDyResponses <- function(filterBusService, busId) {
    # Query entries based on filter 
    if(filterBusService == "all" && busId == "all"){
      data <- dbDyResponses$find()
    }
    else {
      data <- dbDyResponses$find(query = toString(toJSON(list(busService = input$busService,
                                                              busId = getBusId()), auto_unbox = TRUE)))
    } 
  }
  
  #Pre-cond: JSON of avgVol of buses
  #Post-cond: Inserts JSON of avgVOl of buses into database
  loadResponses <- function(filterBusService, busId) { 
    # Query entries based on filter 
    if(filterBusService == "all" && busId == "all"){
      data <- dbDyResponses$find()
    }
    else {
      data <- dbDyResponses$find(query = toString(toJSON(list(busService = input$busService,
                                                              busId = getBusId()), auto_unbox = TRUE)))
    } 
  }
  
  #Pre-cond: Allows querying by "A1", "A2", "D1", "D2", "all"
  #Post-cond: Reads avgVol of buses with specified query
  #           and returns a dataframe of the result
  loadDyAvgVol <- function(filterBusService) {
    # Query entries based on filter 
    if(filterBusService == "all"){
      data <- dbDyAvgVol$find()
    }
    else {
      data <- dbDyAvgVol$find(query = toString(toJSON(list(busService = input$busService), auto_unbox = TRUE)))
    } 
  }

  #Pre-cond: Allows querying by "A1", "A2", "D1", "D2", "all"
  #Post-cond: Reads responses by user with specified query
  #           and returns a dataframe of the result
  loadAvgVol <- function(filterBusService) {
    # Query entries based on filter 
    if(filterBusService == "all"){
      data <- dbAvgVol$find()
    }
    else {
      data <- dbAvgVol$find(query = toString(toJSON(list(busService = input$busService), auto_unbox = TRUE)))
    } 
  }
  
  #Pre-cond: JSON file of user response
  #Post-cond: Average Bus Capacity as integer
  aggData <- function(data) {
    #Rounds off average value to 2 decimal places
    avgVol <- round(mean(data$busCapacity), 2)
    avgVol
  }
  
  getBusId <- function() { # Changed - ZJ
    ctr <- 0
    realeta <- 0
    flag =TRUE
    
    firstbus <- loadtime(isolate(input$busService))
    firstbusTime <- firstbus["start"][1,] #first bus time
    
    stopIndex <- loadindex(isolate(input$startStop))
    currIndex <- stopIndex[isolate(input$busService)][1,]
    if (currIndex == 0) { realeta <- "Wrong Inputs"}
    
    else {
      while(flag){
        eta <- (firstbusTime + (15*ctr) + ((currIndex-1)*5)) - getTime()
        if(eta > 0) {
          flag = FALSE
          realeta <- as.character(eta)
        }#endif
        
        else {
          ctr <- ctr +1
        }#endelse
      }#end of while
    }#end of else
   #add new modulo
    return(ctr)
  }#end of get Bus Idx
  
  getCurrentTime = reactive({
    #timeNow Manipulation ------------------------------------------------ben
    getTime() # Changed - ZJ
    #end of getting timeNow-----------------------------------------------ben
  })
  
  #NEW CHANGES 
  gettimeOnly = reactive({
    #timeOOnly Manipulation ------------------------------------------------ben
    datetime <- Sys.time()
    datetime <- as.character(datetime)
    tnow<- as.character(substr(datetime,12,16))
    return(tnow)
    #end of getting timeonly-----------------------------------------------ben
  })
  
  
  getdateOnly = reactive({
    #timeOOnly Manipulation ------------------------------------------------ben
    datetime <- Sys.time()
    datetime <- as.character(datetime)
    dnow<- as.character(substr(datetime,1,10))
    return(dnow)
    #end of getting timeonly-----------------------------------------------ben
  })
  
  getTimeQ <- reactive({return(gettimeOnly())})
  getDateQ <- reactive({return(getdateOnly())})
  #END OF TIME CHANGES
  
  
  getTime <- function(){ # Changed - ZJ
    datetime <- Sys.time()
    datetime <- as.character(datetime)
    nowhr <- as.numeric(substr(datetime,12,13))
    nowmin <- as.numeric(substr(datetime,15,16))
    timeNow <- (nowhr*60)+(nowmin)
    return(timeNow)
  }
  
  ############## Changed position of function - by ZJ ##############
  #query for required data ---------------------------------------------ben
  loadtime <- function(filter) {
    timeData <- stime$find(query = toString(toJSON(list(bus=input$busService),auto_unbox = TRUE)))
  }#end of loadtime
  
  loadindex <- function(filter) {
    stopIdx <- routeidx$find(query = toString(toJSON(list(stopId=input$startStop),auto_unbox = TRUE)))   
  }#end of loadindex
  
  loadquery <- function(filter) {
    query <- querydb$find(query = toString(toJSON(list(bus=input$busService,stopId=input$startStop,busIdx=(getBusId()%%7)),auto_unbox = TRUE)))
    }#end of load query
  
  #end of db query -----------------------------------------------------ben
  
  #Functions to save data to db ----------------------------------------saveDB/Ben
  
  saveResponses <- function(data) {
    querydb$insert(data)
  }#end of save responses
  
  #ENDFunctions saveDB--------------------------------------------------saveDB/Ben
  ###############################################################
}

shinyApp(ui = ui, server = server)