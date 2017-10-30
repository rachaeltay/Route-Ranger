library(mongolite)
library(jsonlite)
library(DT)
library(ggplot2)
library(shiny)
library(forecast)
library(TTR)
library(shiny)
library(shinythemes)
library(ggthemes)
library(shinydashboard)
library(lubridate)
library(httr)
library(jsonlite)
library(ggplot2)
library(DT)
# put in AM and PM w date

library(smooth)
# %>% pipeline from magrittr
library(magrittr)
library(googleAuthR)
library(shinyjs)
library(mongolite)
library(scales)
# colour gradient in the graph
library(viridis)
library(forecast)
library(TTR)
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

# databaseName <- "myshinydatabase"
# databaseUrl <- "mongodb://127.0.0.1:27017"

databaseName <- "trr"
databaseUrl <- "mongodb://soraares:bt3103@therouteranger-shard-00-00-rgv6u.mongodb.net:27017,therouteranger-shard-00-01-rgv6u.mongodb.net:27017,therouteranger-shard-00-02-rgv6u.mongodb.net:27017/test?ssl=true&replicaSet=TheRouteRanger-shard-0&authSource=admin"

# databaseName <- "trr"
# databaseUrl <- "mongodb://soraares:bt3103@therouteranger-shard-00-00-rgv6u.mongodb.net:27017,therouteranger-shard-00-01-rgv6u.mongodb.net:27017
# ,therouteranger-shard-00-02-rgv6u.mongodb.net:27017/test?ssl=true&replicaSet=TheRouteRanger-shard-0&authSource=admin"

# dbDyResponses <- mongo(collection = "dynamicResponses",db = databaseName, url = databaseUrl )
# 
# dbResponses <- mongo(collection = "responses",db = databaseName, url = databaseUrl )
# 
# dbDyAvgVol <- mongo(collection = "dynamicAvgVol",db = databaseName, url = databaseUrl )
# 
# dbAvgVol <- mongo(collection = "avgVol",db = databaseName, url = databaseUrl )

dbDyResponses <- mongo(collection = "dynamicResponses",db = databaseName, url = databaseUrl )

dbResponses <- mongo(collection = "responses",db = databaseName ,url = databaseUrl)

dbDyAvgVol <- mongo(collection = "dynamicAvgVol",db = databaseName ,url = databaseUrl)

dbAvgVol <- mongo(collection = "avgVolTrend",db ="trr", url = databaseUrl)

#Google Authentication
options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email",
                                        "https://www.googleapis.com/auth/userinfo.profile"))
options("googleAuthR.webapp.client_id" = "682524538636-26vgeiltv82qiapjk63mg79ltrtscovc.apps.googleusercontent.com")
options("googleAuthR.webapp.client_secret" = "SVsY07OxK6yQeaqtEcSIFPsh")


#BEN Database
#Load testRoute <- database with route details
#routeidx <- mongo(db="trrdb", collection="testRoute", url = "mongodb://localhost")

#routeidx <- mongo(db=databaseName, collection="testRoute", url = databaseUrl)
routeidx <- mongo(url = "mongodb://soraares:bt3103@therouteranger-shard-00-00-rgv6u.mongodb.net:27017,therouteranger-shard-00-01-rgv6u.mongodb.net:27017,therouteranger-shard-00-02-rgv6u.mongodb.net:27017/test?ssl=true&replicaSet=TheRouteRanger-shard-0&authSource=admin", db = "trr", collection = "testRoute")

#Load testTime <- database with starting time
#stime <-mongo(db="trrdb", collection="testTime", url = "mongodb://localhost")
#stime <-mongo(db=databaseName, collection="testTime", url = databaseUrl)
stime <- mongo(url = "mongodb://soraares:bt3103@therouteranger-shard-00-00-rgv6u.mongodb.net:27017,therouteranger-shard-00-01-rgv6u.mongodb.net:27017,therouteranger-shard-00-02-rgv6u.mongodb.net:27017/test?ssl=true&replicaSet=TheRouteRanger-shard-0&authSource=admin", db = "trr", collection = "testTime")

#Load finalstops (second last and last stop)
#finale <-mongo(db="trrdb", collection="end", url = "mongodb://localhost")
#finale <-mongo(db=databaseName, collection="end", url = databaseUrl)
finale <- mongo(url = "mongodb://soraares:bt3103@therouteranger-shard-00-00-rgv6u.mongodb.net:27017,therouteranger-shard-00-01-rgv6u.mongodb.net:27017,therouteranger-shard-00-02-rgv6u.mongodb.net:27017/test?ssl=true&replicaSet=TheRouteRanger-shard-0&authSource=admin", db = "trr", collection = "end")

#To Save Query DB (ZONGJIE DO NOT TOUCH)
#queryList <- mongo(db=databaseName, collection="queryList", url= databaseUrl)
#queryList <- mongo(db=databaseName, collection="queryBase", url= databaseUrl)
queryList <- mongo(url = "mongodb://soraares:bt3103@therouteranger-shard-00-00-rgv6u.mongodb.net:27017,therouteranger-shard-00-01-rgv6u.mongodb.net:27017,therouteranger-shard-00-02-rgv6u.mongodb.net:27017/test?ssl=true&replicaSet=TheRouteRanger-shard-0&authSource=admin", db = "trr", collection = "queryList")

# querydb <- mongo(db="local", collection="queryList")

server <- function(input, output, session) {
  
  ####################   Dashboard Boxes   ####################
  output$startStopBox <- renderValueBox({
    valueBox(
      startStop, "Recent Stop", icon = icon("bus"),
      color = "blue"
    )
  })
  
  output$avgVolBox <- renderValueBox({
    valueBox(
      # get last value of avgVol
      dfAvgVol$startStop$avgVol[-1], "Estimated Bus Capacity", icon = icon("adjust", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  ####################   Google Login   ####################
  
  ## Global variables needed throughout the app
  rv <- reactiveValues(
    login = FALSE
  )
  
  showModal(modalDialog(
    title = "Welcome to Route Ranger",
    googleAuthUI("gauth_login"),
    # footer = NULL,
    easyClose = FALSE
  ))
  
  observe({
    if (rv$login) {
      removeModal()
    }
  })
  
  ## Authentication
  accessToken <- callModule(googleAuth, "gauth_login",
                            login_class = "btn btn-primary",
                            logout_class = "btn btn-warning")
  
  userDetails <- reactive({
    validate(
      need(accessToken(), "not logged in")
    )
    rv$login <- TRUE
    print("true")
    with_shiny(get_user_info, shiny_access_token = accessToken())
  })
  
  ## Display user's Google display name after successful login
  output$display_username <- renderText({
    validate(
      need(userDetails(), "getting user details")
    )
    userDetails()$displayName
  })
  
  ## Workaround to avoid shinyapps.io URL problems
  observe({
    if (rv$login) {
      shinyjs::onclick("gauth_login-googleAuthUi",
                       shinyjs::runjs("window.location.href = 'https://yourdomain.shinyapps.io/appName';"))
    }
  })
  
  
  ####################   Dashboard Plot   ####################
  loadStart <- function(data){
    query <- data.frame(queryList$find())
    num <- nrow(query)
    
    data <- query[num,]["stopId"][1,]
    print("data")
    #print(data)
    return(data)
  }
  
  loadStops <- function(filter) { #all the bus stops
    allStopsAvail <- finale$find(query = toString(toJSON(list(key="key"),auto_unbox = TRUE)))
    answer <- list(allStopsAvail["list"][1,]) 
    print("answer")
    return(answer)
    #print(answer)
  }#loadStops
  
  # retrieve most recent query if not default to KR
  # with dbAvgVol is my db
  startStop <- loadStart()
  busStops <- loadStops()
  #print("busStops")
  #print(busStops)
  # View(busStops)
  # print(startStop)
  # print(busStops)
  
  initialiseData <- function(bus){
    
    # for every bus stop I create a dataframe and populate data
    dfAvgVol <- list()
    for (busStop in unlist(busStops)){
      #print(busStop)
      #dfAvgVol[busStop] <- dbAvgVol$find(paste0('{"startStop": "', busStop, '"}'))
      print(busStop)
      #NEED TO CHECK WITH RACH AND ZJ WHAT DATA THEY WANT
      dfAvgVol[[busStop]] <- dbAvgVol$find(query = toString(toJSON(list( stopId=busStop,busService = bus), auto_unbox = TRUE)))
      
      # if() {
      #   dfAvgVol[busStop]
      # }
      #print(dfAvgVol)
    }
    # print("dfAvgVol")
    # print(dfAvgVol)
    return(dfAvgVol)
  }
  
  # Initialize my_data
  print('Initialised')
  dfAvgVol <- initialiseData("A1")
  #print(dfAvgVol)
  #print(dfAvgVol[["COM2"]])
  #Update every hour update all bus stop dataframes
  updateData <- function(bus){
    
    for (busStop in unlist(busStops)){
      # pull from mongodb average data update of each stop
      # replace dataframe with new containing added data
      a <- dbAvgVol$find(query = toString(toJSON(list(startId = busStop, 
                                                      busService = bus),
                                                 auto_unbox = TRUE)))
      
      #print("A")
      #print(a)
      if(nrow(a)<1){dfAvgVol[[busStop]] <- list()}
      else{
        dfAvgVol[[busStop]] <- a
        #print(dfAvgVol)
      }
    }
    ans <-  data.frame(dfAvgVol)
    #print("Check dfavgvol")
    #View(ans)
    
    # retrieve possibly new starting stop
    return(TRUE)
  }
  
  # Plot the current hours data along with forecast
  
  output$forecastCurrent <- renderPlot({
    
    print("Render Plot")
    invalidateLater(1800000, session) # invalidate every 30 minutes
    print("Update")
    updateData("A1")
    
    
    
    startStop <- loadStart() 
    # startStop <- "PGP"
    
    
    print(typeof(dfAvgVol))
    # View(dfAvgVol)
    
    # time across a day
    
    # Time Series Object
    busCapTS <- dfAvgVol[[startStop]]$avgVol
    # Convert to Dataframe
    busCapTS <- data.frame(as.double(busCapTS))
    timeStampTS <- data.frame(dfAvgVol[[startStop]]$timestamp,stringsAsFactors = FALSE)
    # View(timeStampTS)
    colnames(timeStampTS) <- c("timestamp")
    timeStampTS[[1]] <- strptime(timeStampTS[[1]], "%Y-%m-%d %H:%M:%S")
    
    avgVolTS <- cbind(busCapTS, timeStampTS, col="current")
    colnames(avgVolTS) <- c("avgVol", "timestamps", 'type')
    
    busCapForecast <- ma(ts(busCapTS),order=3)
    #print(busCapForecast)
    busCapForecast <- data.frame(busCapForecast)
    # add 2 hours since k=3
    timeStampForecast <- data.frame(dfAvgVol[[startStop]]$timestamp,stringsAsFactors = FALSE) #+ 2*60*60 #you cant add it as it is a list of timestamp not just one
    colnames(timeStampForecast) <- c("timestamp")
    #convert column to POSIXct so that ggplot can  scale
    timeStampForecast[[1]] <- strptime(timeStampForecast[[1]], "%Y-%m-%d %H:%M:%S")
    
    
    avgVolForecast <- cbind(busCapForecast, timeStampForecast, col='forecast')
    #print(avgVolForecast)
    colnames(avgVolForecast) <- c("avgVol", "timestamps", 'type')
    avgVolForecast <- avgVolForecast[3:nrow(avgVolForecast)-1,]
    
    busCapCombi <- rbind(avgVolTS, avgVolForecast)
    #convert column to POSIXct so that ggplot can  scale
    busCapCombi[[2]] <- strptime(busCapCombi[[2]], "%Y-%m-%d %H:%M:%S")
    #print(busCapCombi)
    
    ggplot(busCapCombi, aes(x=timestamps, colour="forecast",group=type))+ #, ymin = 1, ymax = 40
      geom_line(aes(y=avgVol), color="red") +
      theme_economist() + scale_color_economist() +
      scale_color_manual(values=wes_palette(n=5, name="Zissou")) +
      scale_x_datetime(breaks = date_breaks("1 hour"), labels=date_format("%I%p"))+ #Scales the axis
      labs(x="Time", y="Number of people on the bus")+
      theme(panel.background=element_rect(fill="lightblue"))
  })
  
  
  output$forecastAcrossWeek <- renderPlot({
    # holt winter across a week
    
    # store date for moving average
    # ma.forecast <- ma(busCapTS, order=3)
    # ma.TTR <- SMA(busCapTS, 3)
    # plot(ma.forecast)
    # ggplot(ma.forecast, aes(time,ma.forecast))
    # ggplot(ma.TTR)
    
    # # literally multiple freq to create a week  (Time 1 to 7, Monday to Sunday)
    # avgVolTS <- ts(dfAvgVol$startStop$avgVol, start=1, end=7, frequency = 14)
    # # plot(avgVolTS)
    # decomp <- decompose(avgVolTS)
    # plot(decomp)
    # # triple exponential - models level, trend, and seasonal components
    # fit <- HoltWinters(decomp$seasonal)
    # # plot(fit)
    # # predictive accuracy
    # print(accuracy(forecast(fit)))
    
    #plot the seasonality in decomposition using Holt Winter for prediction
    # ggsdc(busCapCombi, aes(x = timestamps, y = avgVol, colour = forecast), method = "stl") + geom_line()
    
  })
  
  ####################   Changing Select Inputs by Bus Service  ####################
  observe({
    if(input$busService == "A1") {
      updateSelectInput(session, "startStop",
                        label = paste("Choose Your Starting Bus Stop"),
                        choices = c("PGP","Kent_Ridge_MRT","NUH", "LT29" , "UHall", "Opp_UHC", "YIH", "Central_Library", "LT13", "AS7", "COM2",
                                    "BIZ2", "PGP_Hse_12", "PGP_Hse_7", "PGP")
                        #selected = tail(input$startStop)
      )
      updateSelectInput(session, "endStop",
                        label = paste("Choose Your Destination Bus Stop"),
                        choices = c("PGP","Kent_Ridge_MRT","NUH", "LT29" , "UHall", "Opp_UHC", "YIH", "Central_Library", "LT13", "AS7", "COM2",
                                    "BIZ2", "PGP_Hse_12", "PGP_Hse_7", "PGP")
                        #selected = tail(input$endStop)
      )
    }
    else if(input$busService == "A2") {
      updateSelectInput(session, "startStop",
                        label = paste("Choose Your Starting Bus Stop"),
                        choices = c("PGP", "PGP_Hse_14_15", "PGP_Hse_12", "Opp_HSSML", "Opp_NUSS", "COM2", "Ventus", "Computer_Centre",
                                    "Opp_YIH", "Museum", "UHC", "Opp_UHall", "S17", "Opp_NUH", "Opp_Kent_Ridge_MRT", "PGPR")
                        #selected = tail(input$startStop)
      )
      updateSelectInput(session, "endStop",
                        label = paste("Choose Your Destination Bus Stop"),
                        choices = c("PGP", "PGP_Hse_14_15", "PGP_Hse_12", "Opp_HSSML", "Opp_NUSS", "COM2", "Ventus", "Computer_Centre",
                                    "Opp_YIH", "Museum", "UHC", "Opp_UHall", "S17", "Opp_NUH", "Opp_Kent_Ridge_MRT", "PGPR")
                        #selected = tail(input$endStop)
      )
    }
    else if(input$busService == "D1") {
      updateSelectInput(session, "startStop",
                        label = paste("Choose Your Starting Bus Stop"),
                        choices = c("Opp_HSSML", "Opp_NUSS", "COM2", "Ventus", "Computer_Centre", "Opp_YIH", "UTown", "Museum", "YIH",
                                    "Central_Library", "LT13", "AS7", "COM2", "BIZ2" )
                        #selected = tail(input$startStop)
      )
      updateSelectInput(session, "endStop",
                        label = paste("Choose Your Destination Bus Stop"),
                        choices = c("Opp_HSSML", "Opp_NUSS", "COM2", "Ventus", "Computer_Centre", "Opp_YIH", "UTown", "Museum", "YIH",
                                    "Central_Library", "LT13", "AS7", "COM2", "BIZ2" )
                        #selected = tail(input$endStop)
      )
    }
    else if(input$busService == "D2") {
      updateSelectInput(session, "startStop",
                        label = paste("Choose Your Starting Bus Stop"),
                        choices = c("PGP", "Kent_Ridge_MRT", "LT29", "UHall", "Opp_UHC", "Museum", "UTown", "UHC", "Opp_UHall", "S17",
                                    "Opp_Kent_Ridge_MRT", "PGPR", "CP11" )
                        #selected = tail(input$startStop)
      )
      updateSelectInput(session, "endStop",
                        label = paste("Choose Your Destination Bus Stop"),
                        choices = c("PGP", "Kent_Ridge_MRT", "LT29", "UHall", "Opp_UHC", "Museum", "UTown", "UHC", "Opp_UHall", "S17",
                                    "Opp_Kent_Ridge_MRT", "PGPR", "CP11" )
                        #selected = tail(input$endStop)
      )
    }
  })#End of observe
  ##############################################################
  
  output$click <- renderText({
    "Click to refresh, otherwise wait 10s after Submit Crowd Level"
  })# End of output$click
  
  #STARTBEN ------------------------------------------------------->
  
  observeEvent(input$submitQ,{
    
    getCurrentTime = reactive({
      #timeNow Manipulation ------------------------------------------------ben
      getTime() # Changed - ZJ
      #end of getting timeNow-----------------------------------------------ben
    })
    
    #Start of select data -------------------------------------------------->
    pastQ <- loadquery()
    if(nrow(pastQ)==0) {}
    else{
      pQ<- data.frame(cbind(pastQ["timestamp"],pastQ["rETA"]))
      print(pQ)
      
      pQuery<- cbind(ma=0,pQ)
      print(pQuery)
      
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
      
      if(nrow(pQuery)>4){
        for (row in 4:nrow(pQuery)) {
          pQuery[row,]["ma"][1,] <- findUnique(pQuery,row)
        }#end of row manipulation
        pQuery[1,]["ma"][1,] <- pQuery[4,]["ma"][1,]
        pQuery[2,]["ma"][1,] <- pQuery[4,]["ma"][1,]  
        pQuery[3,]["ma"][1,] <- pQuery[4,]["ma"][1,]  
        
      }
      
      output$ma <- renderPlot(
        ggplot(data=pQuery,aes(x=timestamp,color="black",group="black"))+geom_line(aes(y=rETA),color="black")
        +geom_line(aes(y=ma),color="red")+
          theme_economist() + scale_color_economist() +
          scale_color_manual(values=wes_palette(n=5, name="Zissou")) +
          #scale_x_datetime(breaks = date_breaks("1 hour"), labels=date_format("%H:%M"))+ #Scales the axis
          labs(x = "Time Of Query", y="Actual ETA")+
          theme(panel.background=element_rect(fill="lightblue"))
      )
    }#end of else
    #Start of select data ------
    
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
    queryTable <- c("bus","stopId","busIdx","realidx","pETA","timeArr","rETA","sourceBusStop","destinationBusStop","timestamp")
    
    insertQuery <- eventReactive(input$submitQ, {
      
      queryTable$bus <- isolate(input$busService)
      queryTable$stopId <- isolate(input$startStop)
      queryTable$busIdx <- (getBus())%%7 #numeric
      queryTable$realidx <-  getBus()
      queryTable$pETA <- as.numeric(selectdata())
      queryTable$timeArr <- ""
      queryTable$rETA <- 0
      
      queryTable$sourceBusStop <- isolate(input$startStop)
      queryTable$destinationbusStop <- isolate(input$endStop)
      queryTable$timestamp <- Sys.time()
      
      
      insertData <- toJSON(queryTable[c("bus","stopId","busIdx","realidx","pETA","timeArr","rETA","sourceBusStop","destinationBusStop","timestamp")],auto_unbox = TRUE)
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
    
  })#end of q
  
  
  observeEvent(input$submitV, {
    
    getActualTime = reactive({
      dtime <- Sys.time()
      dtime <- as.character(dtime)
      anowhr <- as.numeric(substr(dtime,12,13))
      anowmin <- as.numeric(substr(dtime,15,16))
      atimeNow <- (anowhr*60)+(anowmin)
      return(atimeNow)
    })
    
    arrTime <- getActualTime() #use in for loop
    
    queryData <- loadquery() #generate list
    numQuery <- nrow(queryData)
    print(numQuery)
    #print(queryData)
    
    #print(queryData[1,])
    
    getMins <- function(data) {
      hour <- as.numeric(substr(data,12,13))
      min <- as.numeric(substr(data,15,16))
      timequery <- (hour*60)+(min)
      return(timequery)
    }#end of getMins
    
    getMins <- function(data) {
      hour <- as.numeric(substr(data,12,13))
      min <- as.numeric(substr(data,15,16))
      timequery <- (hour*60)+(min)
      return(timequery)
    }#end of getMins
    
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
    
    print(dataF)
    output$ma <- renderPlot(
      ggplot(data=dataF,aes(x=timestamp,color="black",group="black"))+geom_line(aes(y=rETA),color="black")
      +geom_line(aes(y=ma),color="red")
    )
    
    
    
    
  } ) #end of observerEvent submitV
  
  
  #ENDBEN --------------------------------------------------------->
  
  
  #Pre-cond: Waits for submitV button to be depressed
  #Post-Cond: Returns a JSON file of user responses
  formData <- eventReactive(input$submitV, {
    
    responseTable$busId <- getBusId()
    responseTable$busService <- isolate(input$busService)
    responseTable$startStop <- isolate(input$startStop)
    responseTable$endStop <- isolate(input$endStop)
    responseTable$busCapacity <- isolate(input$busCapacity)
    responseTable$timestamp <- myTimestamp()
    # Converts List to JSON, and unboxes each element to primitive
    data <- toJSON(responseTable[c("busId", "busService", "startStop", "endStop",
                                   "busCapacity", "timestamp")], auto_unbox = TRUE)
  })# End of formData
  
  #Waits for Submit button to be depressed
  observeEvent(input$submitV, {
    # Remove response(s) where startStop of User EQUALS the destination stop of other users 
    # AND the busService of all responses are the same 
    # AND busId of all responses are the same
    # (When the bus reached the endStop of other users)
    dbDyResponses$remove(query = toString(toJSON(list(endStop = isolate(input$startStop),
                                                      busService = isolate(input$busService),
                                                      busId = getBusId()),
                                                 auto_unbox = TRUE))) #TBC filter include BusId
    insertDyResponse(formData()) #Inserting data into database
    print("Submitted")
  })# End of observeEvent
  
  #Pre-cond: Waits for submit button to be depressed
  #Post-Cond: Retrieves all user responses, and returns a JSON file with
  #           the calculated avgVol 
  dyAvgVolData <- eventReactive(input$submitV, {
    dyAvgVolTable$busId <- getBusId()
    dyAvgVolTable$busService <- isolate(input$busService)
    #loadDyResponses(filterBusService, filterBusId, date)
    dyAvgVolTable$dyAvgVol <- ceiling(aggData(loadDyResponses("all", getBusId(), toString(as.Date(Sys.Date())))$busCapacity)/3*40) 
    dyAvgVolTable$startStop <- isolate(input$startStop)
    dyAvgVolTable$endStop <- isolate(input$endStop)
    dyAvgVolTable$timestamp <- myTimestamp() #ISODate timestamp
    # Converts List to JSON, and unboxes each element from List to primitive
    data <- toJSON(dyAvgVolTable[c("busId","busService", "startStop", "endStop",
                                   "dyAvgVol", "timestamp")], auto_unbox = TRUE)
  })# End of dyAvgVolData
  
  # #Convenient Deletion of all rows - will be removed
  # observeEvent(input$clear, {
  #   
  #   data <- queryList$remove('{}')
  #   
  #   dbDyResponses$remove(query = "{}")
  #   dbResponses$remove(query = "{}")
  #   dbDyAvgVol$remove(query = "{}")
  #   #dbAvgVol$remove(query = "{}")
  #   # Inserts buffer data
  #   buffer()
  # })# End of observeEvent
  
  # observe({
  #   #Pre-cond: Waits for submitV button to be depressed
  #   # #Post-cond: Renders data table of all previous responses
  #   output$responses <- renderDataTable({
  #     isolate(input$submitV)
  #     loadDyResponses("all", getBusId(), toString(as.Date(Sys.Date()))) #loadDyResponses(filterBusService, filterBusId, date)
  #   })# End of output$responses
  # })# End of observe
  
  #Pre-cond: Integer in millisecond
  #Post-cond: Reactive countdown timer 
  autoInvalidate <- function(timeMS) {
    invalidateLater(timeMS, session)
  }
  
  # observe({
  #   #Pre-cond: Waits for submitV button to be depressed
  #   # #Post-cond: Renders data table of all previous responses
  #   output$responses <- renderDataTable({
  #     isolate(input$submitV)
  #     loadDyResponses("all", getBusId()) # #loadDyResponses(filterBusService, busId)
  #   })
  # })
  
  #Pre-cond: Waits for submitV button to be depressed
  #Post-cond: render plot of avgVol over time/ busstop
  output$graph <- renderPlot({
    #print("Initialising Plot")
    input$update
    invalidateLater(10000, session) #10000ms = 10s
    dyAvgVol <- loadDyAvgVol(isolate(input$busService), getBusId(), toString(as.Date(Sys.Date())))#loadDyAvgVol(filterBusService, filterBusId, date)
    #print("Loading Plot")
    if(nrow(dyAvgVol) == 0) {
      buffer()
    }# End of if
    # if not (Secondlast Stop && number of rows in average is 3)
    if(!(isolate(input$startStop) == getSecondLastStop(isolate(input$busService)) && 
         nrow(dyAvgVol) == 3)) { 
      isolate(insertDyAvgVol(dyAvgVolData()))
    }# End of if
    average <- loadDyAvgVol(isolate(input$busService), getBusId(), toString(as.Date(Sys.Date())))#loadDyAvgVol(filterBusService, filterBusId, date)
    ########## Algorithm to extract sampled avgVol at each busStop for plotting ##########
    # print("average")
    # print(average)
    #check if false data has been inserted
    if(average['startStop'][nrow(average)-1,] == "last" &&
       average['endStop'][nrow(average)-1,] == "last") { 
      #inserts busCapacity for avgVol at Last Stop
      insertAvgVol(toJSON(list(busId = getBusId(), busService = average['busService'][1,], 
                               startStop = getLastStop(isolate(input$busService)),  
                               avgVol = 1, timestamp = myTimestamp()), auto_unbox = TRUE)) 
      dbDyAvgVol$remove(query = "{}") #clears false data
      # Inserts buffer data
      buffer()
    }# End of if
    #Ensures that last 2 entries in dyAvgVol are equal and 3rd last entry is different, to determine when to sample
    else if(nrow(average) > 2 && 
            (average['dyAvgVol'][nrow(average),] == average['dyAvgVol'][nrow(average)-1,] &&
             average['dyAvgVol'][nrow(average)-1,] != average['dyAvgVol'][nrow(average)-2,] )) {
      
      avgVolTable$busId <- average['busId'][nrow(average),]
      avgVolTable$busService <- average['busService'][nrow(average),]
      avgVolTable$startStop <- average['startStop'][nrow(average),]
      avgVolTable$avgVol <- average['dyAvgVol'][nrow(average),]
      avgVolTable$timestamp <- average['timestamp'][nrow(average),]
      data <- toJSON(avgVolTable[c("busId", "busService", "startStop", "avgVol", 
                                   "timestamp")], auto_unbox = TRUE)
      insertAvgVol(data) #insert sampled data
    }# End of elseif
    plotAvgVol <- loadAvgVol(isolate(input$busService), getBusId(), toString(as.Date(Sys.Date()))) #loadAvgVol(filterBusService, filterBusId, date)
    # Checks when the bus has reached second last stop
    if(nrow(plotAvgVol) != 0 && 
       plotAvgVol['startStop'][nrow(plotAvgVol),] == getSecondLastStop(isolate(input$busService))) { 
      dbDyAvgVol$remove(query = toString(toJSON(list(busService = isolate(input$busService), busId = getBusId()), 
                                                auto_unbox = TRUE))) 
      #Inserts false data to indicate bus has reached last stop
      dbDyAvgVol$insert(toJSON(list(busId = getBusId(), busService = isolate(input$busService), startStop = "last", endStop = "last", 
                                    dyAvgVol = 1, timestamp = myTimestamp()), auto_unbox = TRUE)) 
    }# End of if
    ####################################################################################
    
    if(nrow(plotAvgVol) != 0){
      ggplot(data=plotAvgVol, aes(x= factor(startStop, levels=unique(startStop)), y = avgVol, ymin = 0, ymax = 40, fill = avgVol)) + 
        scale_y_continuous(limit = c(0, 40), expand = c(0,0)) + scale_x_discrete(expand = c(0,0)) +
        geom_bar(stat = "identity") + labs(x = "Bus Stops") +
        ggtitle(paste0(isolate(input$busService)," ", "Bus ID: ", "(", getBusId(), ")")) + 
        theme(axis.text.x = element_text(color="black", size=10, angle=45, vjust = 0.5)) +
        scale_fill_continuous(low="green", high="red") +
        theme(plot.title = element_text(family = "Comic Sans MS", color="tomato", face="bold", size=28))
    }# End of if
  })# End of output$graph
  
  output$avgVolPerRoute <- renderPlot({
    #loadAvgVol(filterBusService, filterBusId, date = "")
    data <- loadAvgVol(isolate(input$busService), as.numeric(input$busId)) # *** use for real data ***
    # print("data")
    # print(data)
    ggplot(data, aes(x=factor(startStop, levels=unique(startStop)), y=avgVol)) +
      geom_boxplot(outlier.shape = NA) +
      geom_smooth(method = "auto", aes(group = 1), se = TRUE, span = 0.5) +
      scale_y_continuous(limits = c(0, 40)) +
      labs(x = "Bus Stops") +
      ggtitle(paste0(isolate(input$busService))) +
      theme(axis.text.x = element_text(color="black", size=12, angle=90, vjust = 0.5)) +
      theme(plot.title = element_text(family = "Comic Sans MS", color="tomato", face="bold", size=28))
  })# End of output$avgVolTrendPlot
  
  # output$realTimeRiggedData <- renderPlot({
  #   
  #   stops <- data.frame("startStop" = c("PGP","Kent_Ridge_MRT","NUH", "LT29" , "UHall", "Opp_UHC", "YIH", "Central_Library", "LT13", "AS7", "COM2",
  #                                       "BIZ2", "PGP_Hse_12", "PGP_Hse_7", "PGP"))
  #   #loadAvgVol(filterBusService, filterBusId, date = "")
  #   data <- loadAvgVol(input$busService, as.numeric(input$busId)) # *** use for real data ***
  #   realTimeAvgVol <- data.frame("startStop" = "", "avgVol" = 0)[FALSE,]
  #   for(stop in stops$startStop) {
  #     temp <- data[data$startStop == stop,] #filter rows of the same stop
  #     avgVol <- min(max(0,ceiling(aggData(temp$avgVol))), 40) #get average of the avgVolTrend for same stop
  #     realTimeAvgVol <- rbind(realTimeAvgVol, data.frame("startStop" = stop, "avgVol" = avgVol)) #new dataframe for plotting
  #     if(stop == stops[nrow(stops)-3,]) { # *** Determines at which bus stop to discontinue graph ***
  #       break
  #     }# End of if
  #   }# End of for
  #   ggplot(realTimeAvgVol, aes(x= factor(startStop, levels=unique(startStop)), y = avgVol, ymin = 0, ymax = 40, fill = avgVol)) +
  #     scale_y_continuous(limit = c(0, 40), expand = c(0,0)) + scale_x_discrete(expand = c(0,0)) +
  #     geom_bar(stat = "identity") + labs(x = "Bus Stops") +
  #     theme(axis.text.x = element_text(color="black", size=12, angle=90, vjust = 0.5)) +
  #     ggtitle(paste0(input$busService)) +
  #     scale_fill_continuous(low="green", high="red") +
  #     theme(plot.title = element_text(family = "Comic Sans MS", color="tomato", face="bold", size=28))
  # })# End of output$realTimeRiggedData
  
  
  ###################### FUNCTIONS ######################
  
  #Pre-cond: Waits for submitV button to be depressed
  #Post-cond: Renders mean value of Bus Capacity as Text
  # output$stats <- renderText({
  #   input$submitV 
  #   avg <- aggData(loadDyResponses(input$busService, getBusId(), toString(as.Date(Sys.Date())))) #loadDyResponses(filterBusService, filterBusId, date)
  #   print(avg)
  #   paste("Mean: ", avg)
  # })# End of output$stats
  
  getLastStop <- function(busService){ # Changed - ZJ
    busStop <- finale$find(query = toString(toJSON(list(busService = isolate(input$busService)), auto_unbox = TRUE)))['last'][1,]
  }# End of getLastStop()
  
  getSecondLastStop <- function(busService){ # Changed - ZJ
    busStop <- finale$find(query = toString(toJSON(list(busService = isolate(input$busService)), auto_unbox = TRUE)))['secondlast'][1,]
  }# End of getSecondLastStop()
  
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
  
  #Pre-cond: JSON of user responses
  #Post-cond: Inserts JSON of user response into database
  insertDyResponse <- function(data) {
    dbDyResponses$insert(data)
  }# End of insertDyResponse
  
  #Pre-cond: JSON of avgVol of buses
  #Post-cond: Inserts JSON of avgVOl of buses into database
  insertDyAvgVol <- function(data) {
    dbDyAvgVol$insert(data)
  }# End of insertDyAvgVol
  
  #Pre-cond: JSON of avgVol of buses
  #Post-cond: Inserts JSON of avgVOl of buses into database
  insertAvgVol <- function(data) {
    dbAvgVol$insert(data)
  }# End of insertAvgVol
  
  #Pre-cond: Allows querying by "A1", "A2", "D1", "D2", "all"
  #Post-cond: Reads responses by user with specified query
  #           and returns a dataframe of the result
  loadDyResponses <- function(filterBusService, filterBusId, date = "") {
    # Query entries based on filter 
    if(filterBusService == "all"){
      data <- dbDyResponses$find()
    }# End of if
    else {
      data <- dbDyResponses$find(query = toString(toJSON(list(busService = filterBusService,
                                                              busId = filterBusId), auto_unbox = TRUE)))
      if(date != "") {
        data <- data[grep(date, data$timestamp),] # *** use this if data with accurate timestamp is available ***  
      }
    }# End of else
    data
  }# End of loadDyResponses
  
  #Pre-cond: Allows querying by "A1", "A2", "D1", "D2", "all"
  #Post-cond: Reads avgVol of buses with specified query
  #           and returns a dataframe of the result
  loadDyAvgVol <- function(filterBusService, filterBusId = 0, date = "") {
    # Query entries based on filter 
    if(filterBusService == "all"){
      data <- dbDyAvgVol$find()
    }# End of if
    else {
      data <- dbDyAvgVol$find(query = toString(toJSON(list(busService = filterBusService,
                                                           busId = filterBusId), auto_unbox = TRUE)))
      if(date != "") {
        data <- data[grep(date, data$timestamp),] # *** use this if data with accurate timestamp is available ***  
      }
    }# End of else
    data
  }# End of loadDyAvgVol
  
  #Pre-cond: Allows querying by "A1", "A2", "D1", "D2", "all"
  #Post-cond: Reads responses by user with specified query
  #           and returns a dataframe of the result
  loadAvgVol <- function(filterBusService, filterBusId = 0, date = "") {
    # Query entries based on filter 
    if(filterBusService == "all"){
      data <- dbAvgVol$find()
    }# End of if
    else {
      data <- dbAvgVol$find(query = toString(toJSON(list(busService = filterBusService,
                                                         busId = filterBusId), auto_unbox = TRUE)))
      if(date != "") {
        data <- data[grep(date, data$timestamp),] # *** use this if data with accurate timestamp is available ***  
      }
    }# End of else
    data
  }# End of loadAvgVol
  
  #Pre-cond: JSON file of user response
  #Post-cond: Average Bus Capacity as integer
  aggData <- function(data) {
    #Rounds off average value to 2 decimal places
    avgVol <- round(mean(data), 2)
    avgVol
  }# End of aggData()
  
  buffer <- function() {
    dbDyAvgVol$insert(toJSON(list(busId = getBusId(), busService = isolate(input$busService), startStop = "", endStop = " ", 
                                  dyAvgVol = 0, timestamp = myTimestamp()), auto_unbox = TRUE))
    dbDyAvgVol$insert(toJSON(list(busId = getBusId(), busService = isolate(input$busService), startStop = "", endStop = " ", 
                                  dyAvgVol = 0, timestamp = myTimestamp()), auto_unbox = TRUE))
    dbDyAvgVol$insert(toJSON(list(busId = getBusId(), busService = isolate(input$busService), startStop = "", endStop = " ", 
                                  dyAvgVol = 0, timestamp = myTimestamp()), auto_unbox = TRUE))
  }# End of buffer
  
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
  }
  
  getMins <- function(data) {
    hour <- as.numeric(substr(data,12,13))
    min <- as.numeric(substr(data,15,16))
    timequery <- (hour*60)+(min)
    return(timequery)
  }#end of getMins
  
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
  }#return correct busIdx of the timeQ
  
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
  }
  
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
    stopIdx <- routeidx$find(query = toString(toJSON(list(stopId=isolate(input$startStop)),auto_unbox = TRUE)))   
  }#end of loadindex
  
  loadquery <- function(filter) {
    query <- queryList$find(query = toString(toJSON(list(bus=input$busService,stopId=isolate(input$startStop),busIdx=(getBusId()%%7)),auto_unbox = TRUE)))
  }#end of load query
  
  
  
  #new function for rachael to get start stop
  loadStart <- reactive({
    query <- queryList$find()
    num <- nrow(query)
    data <- query[num,]["stopId"][1,]
    return(data)
  })
  
  
  #end of db query -----------------------------------------------------ben
  
  #Functions to save data to db ----------------------------------------saveDB/Ben
  
  saveResponses <- function(data) {
    queryList$insert(data)
  }#end of save responses
  
  #ENDFunctions saveDB--------------------------------------------------saveDB/Ben
  ###############################################################
  
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
    
    if (timeFrame == "Daily") {
      boardingSMAtimeFrame <- t(data.frame(tail(boarding$timeFrame, nrow(boarding)-2)))
      alightingSMAtimeFrame <- t(data.frame(tail(alighting$timeFrame, nrow(alighting)-2)))
      SMAtimeFrame <- list(rbind(alightingSMAtimeFrame, boardingSMAtimeFrame))
      SMAtimeFrame <- lapply(SMAtimeFrame[1], {function(x) x = as.POSIXct(x)+days(1)})
      
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
}
