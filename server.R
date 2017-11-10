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
library(lubridate)
library(smooth)
library(magrittr)
library(googleAuthR)
library(shinyjs)
library(mongolite)
library(scales)
library(viridis)
library(forecast)
library(TTR)

library(reshape2)
library(hydroGOF)
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

databaseName <- "trr"
databaseUrl <- "mongodb://soraares:bt3103@therouteranger-shard-00-00-rgv6u.mongodb.net:27017,therouteranger-shard-00-01-rgv6u.mongodb.net:27017,therouteranger-shard-00-02-rgv6u.mongodb.net:27017/test?ssl=true&replicaSet=TheRouteRanger-shard-0&authSource=admin"

dbAvgVol <- mongo(collection = "avgVol",db = databaseName, url = databaseUrl )

dbDyResponses <- mongo(collection = "dynamicResponses",db = databaseName, url = databaseUrl )

dbResponses <- mongo(collection = "responses",db = databaseName ,url = databaseUrl)

dbDyAvgVol <- mongo(collection = "dynamicAvgVol",db = databaseName ,url = databaseUrl)

dbAvgVolTrend <- mongo(collection = "avgVolTrend",db =databaseName, url = databaseUrl)

#Google Authentication
options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email",
                                        "https://www.googleapis.com/auth/userinfo.profile"))
options("googleAuthR.webapp.client_id" = "682524538636-26vgeiltv82qiapjk63mg79ltrtscovc.apps.googleusercontent.com")
options("googleAuthR.webapp.client_secret" = "SVsY07OxK6yQeaqtEcSIFPsh")


#BEN Database
routeidx <- mongo(url = "mongodb://soraares:bt3103@therouteranger-shard-00-00-rgv6u.mongodb.net:27017,therouteranger-shard-00-01-rgv6u.mongodb.net:27017,therouteranger-shard-00-02-rgv6u.mongodb.net:27017/test?ssl=true&replicaSet=TheRouteRanger-shard-0&authSource=admin", db = "trr", collection = "testRoute")

#Load testTime <- database with starting time
stime <- mongo(url = "mongodb://soraares:bt3103@therouteranger-shard-00-00-rgv6u.mongodb.net:27017,therouteranger-shard-00-01-rgv6u.mongodb.net:27017,therouteranger-shard-00-02-rgv6u.mongodb.net:27017/test?ssl=true&replicaSet=TheRouteRanger-shard-0&authSource=admin", db = "trr", collection = "testTime")

#Load finalstops (second last and last stop)
finale <- mongo(url = "mongodb://soraares:bt3103@therouteranger-shard-00-00-rgv6u.mongodb.net:27017,therouteranger-shard-00-01-rgv6u.mongodb.net:27017,therouteranger-shard-00-02-rgv6u.mongodb.net:27017/test?ssl=true&replicaSet=TheRouteRanger-shard-0&authSource=admin", db = "trr", collection = "end")

#To Save Query DB (ZONGJIE DO NOT TOUCH)
queryList <- mongo(url = "mongodb://soraares:bt3103@therouteranger-shard-00-00-rgv6u.mongodb.net:27017,therouteranger-shard-00-01-rgv6u.mongodb.net:27017,therouteranger-shard-00-02-rgv6u.mongodb.net:27017/test?ssl=true&replicaSet=TheRouteRanger-shard-0&authSource=admin", db = "trr", collection = "queryList")
hourlyData <- mongo(url = "mongodb://soraares:bt3103@therouteranger-shard-00-00-rgv6u.mongodb.net:27017,therouteranger-shard-00-01-rgv6u.mongodb.net:27017,therouteranger-shard-00-02-rgv6u.mongodb.net:27017/test?ssl=true&replicaSet=TheRouteRanger-shard-0&authSource=admin", db = "trr", collection = "hourlyData")

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
      ###################################### fix for last value
      # get last value of avgVol
      dayTable$avgVol[[-1]], "Estimated Bus Capacity", icon = icon("adjust", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  ####################   Google Login   ####################
  
  # for the button to pop up modal
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "Welcome to Route Ranger",
      easyClose = TRUE
    ))
  })
  
  ## Global variables needed throughout the app
  rv <- reactiveValues(
    login = FALSE
  )
  
  showModal(modalDialog(
    title = "Welcome to The Route Ranger",
    tags$caption("Log in with your google account to keep track of your travelling history!"),
    googleAuthUI("gauth_login"),
    p(
      tags$b("README"),
      tags$h4("Dashboard Tab"),
      tags$b("For Riders:"),
      tags$li("View estimated bus capacity across day or week and its forecast"),
      tags$li("Login and logout of the application"),
      tags$b("For NUS Bus Admin:"),
      tags$li("Track usage of bus services across different stops across time")
    ),
    p(
      tags$h4("Buses Tab"),
      tags$b("For Riders:"),
      tags$li("Get the waiting time and volume of their desired bus service at the current location"),
      tags$b("For NUS Bus Admin:"),
      tags$li("Track discrepancy between scheduled bus timings and actual"),
      tags$li("Track spread of bus volume across time, for each stop")
      ),
    p(
      tags$h4("Bus Stops Tab"),
      tags$b("For Riders:"),
      tags$li("Determine the crowdedness of each bus stop at different bus stops"),
      tags$b("For NUS Bus Admin:"),
      tags$li("Track usage of bus stops to determine if any bus stop is overused at any point in time"),
      tags$a(googleAuthUI("gauth_login"))
    ),
    easyClose = TRUE
  ))
  
  observe({
    if (rv$login) {
      removeModal()
    }
  })
  
  ## Authentication
  accessToken <- callModule(googleAuth, "gauth_login",
                            login_class = "btn btn-link",
                            logout_class = "btn btn-warning")
  
  userDetails <- reactive({
    validate(
      need(accessToken(), "not logged in")
    )
    rv$login <- TRUE
    # print("true")
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
    # print("data")
    #print(data)
    return(data)
  }
  
  loadBus <- function(data){
    query <- data.frame(queryList$find())
    num <- nrow(query)
    
    data <- query[num,]["bus"][1,]
    print("data")
    #print(data)
    return(data)
  }
  
  loadStops <- function(filter) { #all the bus stops
    allStopsAvail <- finale$find(query = toString(toJSON(list(key="key"),auto_unbox = TRUE)))
    answer <- list(allStopsAvail["list"][1,]) 
    return(answer)
    #print(answer)
  }#loadStops
  
  loadService <- function(stopname) {
    enroute <- routeidx$find(query = toString(toJSON(list(stopId=stopname),auto_unbox = TRUE)))
    avail <- list()
    # print(enroute)
    ctr <- 1
    if (enroute["A1"][1,] > 0 ) { 
      avail[[ctr]] <- "A1"
      ctr <- ctr +1 }
    if (enroute["A2"][1,] > 0 ) { 
      avail[[ctr]] <- "A2"
      ctr <- ctr +1 }
    if (enroute["D1"][1,] > 0 ) { 
      avail[[ctr]] <- "D1"
      ctr <- ctr +1}
    if (enroute["D2"][1,] > 0 ) { 
      avail[[ctr]] <- "D2"}
    
    return(avail)
  }#end of loadService
  
  # Retrieve most recent query if not default to KR
  # With dbAvgVolTrend is my db
  startStop <- loadStart()
  busReq <- loadBus()
  busStops <- loadStops()

  
  initialiseData <- function(bus,stop){
    
    # For every bus stop create a dataframe and populate data
    dfAvgVol <- list()

    # Extracting the avgVol data
    
    ###################################### flag redundant?
    flag <-  TRUE
    temp <- list()
    df <- dbAvgVolTrend$find(query = toString(toJSON(list( startStop=stop,busService = bus), auto_unbox = TRUE)))
    # print(df)
    dfAvgVol[[stop]]<-df
    flag <- FALSE
    
    return(dfAvgVol)
    
  }
  
  # Initialize my_data
  # print('Initialised')
  dfAvgVol <- initialiseData(busReq,startStop)

  # Update every hour update all bus stop dataframes
  
  updateData <- function(bus){
    
    for (busStop in unlist(busStops)){
      # pull from mongodb average data update of each stop
      # replace dataframe with new containing added data
      a <- dbAvgVolTrend$find(query = toString(toJSON(list(startId = busStop, 
                                                           busService = bus),
                                                      auto_unbox = TRUE)))
      
      # ensure not empty
      if(nrow(a)<1){dfAvgVol[[busStop]] <- list()}
      else{
        dfAvgVol[[busStop]] <- a
        #print(dfAvgVol)
      }
    }
    
    # Retrieve possibly new starting stop
    return(TRUE)
  }
  
  
  # Plot the current hours data along with forecast
  output$forecastCurrent <- renderPlot({
    
    # print("Render Plot")
    invalidateLater(1800000, session) # invalidate every 30 minutes
    #print("Update")

    data <- pdf[grep(today, ts),]
    #print(data)
    #View(data)
    #ensure 1day 
    print(typeof(dfAvgVol))
    
    # Ensure only one day data
    # dataframe used to plot
    dayTable <- dfAvgVol[[startStop]] 
    print("dayTable")
    print(dayTable)
    numR <- nrow(dayTable)
    ts <-  dayTable$timestamp

    # time across a day
    today <- toString(as.Date("2017-10-23 07:00:00 MYT")) #change to Sys.date() once rigged
    data <- dayTable[grep(today, ts),]
   
    # Time Series Object
    busCapTS <- data$avgVol
    # Convert to Dataframe
    busCapTS <- data.frame(as.double(busCapTS))
    timeStampTS <- data.frame(data$timestamp,stringsAsFactors = FALSE)
    # View(timeStampTS)
    colnames(timeStampTS) <- c("timestamp")
    timeStampTS[[1]] <- strptime(timeStampTS[[1]], "%Y-%m-%d %H:%M:%S")
    
    busavail <- data.frame(data$busService)
    
    avgVolTS <- cbind(busCapTS, timeStampTS,busavail, col="current")
    colnames(avgVolTS) <- c("avgVol", "timestamps", "bus","Type")
    
    busCapForecast <- ma(ts(busCapTS),order=3)
    #print(busCapForecast)
    busCapForecast <- data.frame(busCapForecast)
    
    # add 2 hours since k=3
    timeStampForecast <- data.frame(sapply(data$timestamp, function(x) x+2*60*60),stringsAsFactors = FALSE) #+ 2*60*60 #you cant add it as it is a list of timestamp not just one
    colnames(timeStampForecast) <- c("timestamp")
    #convert column to POSIXct so that ggplot can  scale
    timeStampForecast[[1]] <- strptime(timeStampForecast[[1]], "%Y-%m-%d %H:%M:%S")
    
    
    avgVolForecast <- cbind(busCapForecast, timeStampForecast,busavail, col='forecast')
    #print(avgVolForecast)
    colnames(avgVolForecast) <- c("avgVol", "timestamps", "bus","Type")
    avgVolForecast <- avgVolForecast[3:nrow(avgVolForecast)-1,]
    
    busCapCombi <- rbind(avgVolTS, avgVolForecast)
    #convert column to POSIXct so that ggplot can  scale
    busCapCombi[[2]] <- strptime(busCapCombi[[2]], "%Y-%m-%d %H:%M:%S")
    #print(busCapCombi)
    
    ggplot(busCapCombi, aes(x=timestamps,color=Type))+ #, ymin = 1, ymax = 40
      geom_line(aes(y=avgVol),size=1.5) +
      theme_economist() +
      scale_color_manual(labels = c("Current", "Forecast"), values = c("#1AA6B7", "#FE424D")) +
      scale_x_datetime(breaks = date_breaks("2 hours"), labels=date_format("%I%p"))+ #Scales the axis
      labs(x = "Time", y="Estimated number of people on the bus")
  })
  
  
  
output$forecastAcrossWeek <- renderPlot({
    # holt winter across a week

    #plot using Holt Winter for prediction
    # create timeseries of a week
    # print(dfAvgVol[[startStop]]$avgVol)
    avgVolTS <- ts(dfAvgVol[[startStop]]$avgVol,start=1, end=5,frequency = 4)
    # triple exponential - models level, trend, and seasonal components
    hw <- HoltWinters(avgVolTS)
    # create forecast using predict
    forecast<-predict(hw,  n.ahead=3,  prediction.interval=T,  level=0.95)

    # seperate hw and forecast into respective dataframes
    forecastData<-data.frame(time=round(time(forecast),  3),  value_forecast=as.data.frame(forecast)$fit,  dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
    fittedData<-data.frame(time=round(time(hw$fitted),  3),  value_fitted=as.data.frame(hw$fitted)$xhat)
    actualData<-data.frame(time=round(time(hw$x),  3),  Actual=c(hw$x))

    # create graph
    datasets<-merge(actualData,  fittedData,  by='time',  all=TRUE)
    datasets<-merge(datasets,  forecastData,  all=TRUE,  by='time')
    datasets[is.na(datasets$dev),  ]$dev<-0

    datasets$Fitted<-c(rep(NA,  nrow(datasets)-(nrow(forecastData) + nrow(fittedData))),  fittedData$value_fitted,  forecastData$value_forecast)

    # using the reshape function to combine all the datasets, melt data so that each row is a unique id-variable combination
    datasetsCombi <- melt(datasets[, c('time', 'Actual', 'Fitted')], id='time')

    ggplot(datasetsCombi,  aes(x=time,  y=value)) +
      geom_ribbon(data=datasets, aes(x=time, y=Fitted, ymin=Fitted-dev,  ymax=Fitted + dev),  alpha=.2,  fill='#56B4E9') +
      geom_line(aes(colour=variable), size=1.5) +
      scale_color_manual(labels = c("Current", "Forecast"), values = c("#1AA6B7", "#FE424D")) +
      scale_x_continuous(breaks = c(1,2,3,4,5), labels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')) +
      xlab('Day of the Week') + ylab('Estimated number of people on the bus') +
      theme_economist_white(base_size=10,gray_bg=FALSE)

  })

  #Getting Time now to react to queries incoming
  getMins <- function(data) {
    hour <- as.numeric(substr(data,12,13))
    min <- as.numeric(substr(data,15,16))
    timequery <- (hour*60)+(min)
    return(timequery)
  }#end of getMins

  instant <- Sys.time()
  insta <- getMins(instant)
  #BEN------------------------------------------


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

    timeN <- function(t) {
      now <- as.character(Sys.time())
      hr <- as.numeric(substr(now,12,13))
      min <- as.numeric(substr(now,15,16))

      if(min<10) {min <- paste("0",as.character(min))}

      last <- "AM"
      if (hr > 12){
        hr <- as.character(hr-12)
        last <- "PM"
      }
      else {hr<- as.character(hr)}
      full <- paste(hr,":",min," ",last)
      return(full)
    }

    if(insta > 1380 || insta <435) {
      pastQ <- loadquery()
      # print(pastQ)
      
      if(nrow(pastQ)==0) {}
      else{
        pQ<- data.frame(cbind(pastQ["timestamp"],pastQ["rETA"],pastQ["pETA"]))
        # print(pQ)
        # print("pQ")
        forecast <- pastQ["rETA"]
        movAvg <- as.data.frame(ma(forecast,order=3))
        print(movAvg)
        df <- data.frame(cbind(pQ,movAvg))
        df <- df[4:nrow(df)-1,]
        df<- cbind(df["timestamp"],sapply(df["rETA"],function(x) as.numeric(x)),df["V1"],sapply(df["pETA"],function(x) as.numeric(x)))
        df[[1]] <- strptime(df[[1]], "%Y-%m-%d %H:%M:%S")


        output$ma <- renderPlot(
          ggplot(data=df,aes(x=timestamp,y=V1,color="black",group="black"))+geom_line(aes(y=rETA),color="black")
          +geom_line(aes(y=V1),color="red")+geom_line(aes(y=pETA),color="blue")+
            theme_economist() + scale_color_economist() +
            scale_x_datetime(breaks = date_breaks("1 week"))+ #Scales the axis
            labs(x = "Time Of Query", y="Actual ETA")+
            theme(panel.background=element_rect(fill="lightblue"))
        )

        output$todayDate <- renderValueBox({
          valueBox(
            as.character(timeN()), "Time", icon = icon("clock-o"),
            color = "blue"
          )
        })

        output$error <- renderValueBox({
          valueBox(
            paste("Mean Square Error :" ,"0", "min(s)"), "Mean Square Error", icon = icon("times-rectangle"),
            color = "blue"
          )
        })

        output$ETA <- renderValueBox({
          valueBox(
            "Not Available", "Waiting Time",
            color = "blue"
          )
        })
      }#end of else


      #endOutput

    }#end of IF

    else{

      getCurrentTime = reactive({
        #timeNow Manipulation ------------------------------------------------ben
        getTime() # Changed - ZJ
        #end of getting timeNow-----------------------------------------------ben
      })

      #Start of select data -------------------------------------------------->
      pastQ <- loadquery()
      # print(pastQ)
      
      if(nrow(pastQ)==0) {}
      else{
        pQ<- data.frame(cbind(pastQ["timestamp"],pastQ["rETA"],pastQ["pETA"]))
        # print(pQ)
        # print("pQ")

        forecast <- pastQ["rETA"]
        movAvg <- as.data.frame(ma(forecast,order=3))
        # print(movAvg)
        df <- data.frame(cbind(pQ,movAvg))
        df <- df[4:nrow(df)-1,]
        df<- cbind(df["timestamp"],sapply(df["rETA"],function(x) as.numeric(x)),df["V1"],sapply(df["pETA"],function(x) as.numeric(x)))
        df[[1]] <- strptime(df[[1]], "%Y-%m-%d %H:%M:%S")

        # View(df)
        # View(df)
        real <- df["rETA"][1:nrow(df),]
        actual <- df["V1"][1:nrow(df),]

        error <- mse(actual,real)
        err <- substr(as.character(error),1,4)


        output$ma <- renderPlot(
          ggplot(data=df,aes(x=timestamp,y=V1,color="black",group="black"))+geom_line(aes(y=rETA),color="black")
          +geom_line(aes(y=V1),color="red")+geom_line(aes(y=pETA),color="blue")+
            theme_economist() + scale_color_economist() +
            scale_x_datetime(breaks = date_breaks("1 week"))+ #Scales the axis
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
      queryTable <- c("bus","stopId","busIdx","realIdx","pETA","timeArr","rETA","destinationBusStop","timestamp")

      insertQuery <- eventReactive(input$submitQ, {

        queryTable$bus <- isolate(input$busService)
        queryTable$stopId <- isolate(input$startStop)
        queryTable$busIdx <- as.character((getBus())%%7) #numeric
        queryTable$realIdx <-  as.character(getBus())
        queryTable$pETA <- as.numeric(selectdata())
        queryTable$timeArr <- ""
        queryTable$rETA <- 0

        queryTable$destinationbusStop <- isolate(input$endStop)
        queryTable$timestamp <- Sys.time()


        insertData <- toJSON(queryTable[c("bus","stopId","busIdx","realIdx","pETA","timeArr","rETA","destinationBusStop","timestamp")],auto_unbox = TRUE)
      })
      #end of sending data to db

      #send data to db START
      if(selectdata() == "Wrong Inputs"){}
      else{saveResponses(insertQuery())}
      #send data to db END

      timeN <- function(t) {
        now <- as.character(Sys.time())
        hr <- as.numeric(substr(now,12,13))
        min <- as.numeric(substr(now,15,16))
        last <- "AM"
        if (hr > 12){
          hr <- as.character(hr-12)
          last <- "PM"
        }
        else {hr<- as.character(hr)}
        full <- paste(hr,":",min," ",last)
        return(full)
      }

      output$todayDate <- renderValueBox({
        valueBox(
          as.character(timeN()), "Time", icon = icon("clock-o"),
          color = "blue"
        )
      }) #END OF TIME VB

      output$error <- renderValueBox({
        valueBox(
          paste("Mean Square Error :" ,as.character(err), "min(s)"), "Mean Square Error", icon = icon("times-rectangle"),
          color = "blue"
        )
      })#end of MSE VB

      output$ETA <- renderValueBox({
        valueBox(
          paste("ETA:", as.character(selectdata()), "min(s)"), "Waiting Time",
          color = "blue"
        )
      })#end of ETA VB


    }#end of else
  })#end of q


  if(insta > 1380 || insta <435){}
  else{

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
      # print(numQuery)
      #print(queryData)

      #print(queryData[1,])

      getMins <- function(data) {
        hour <- as.numeric(substr(data,12,13))
        min <- as.numeric(substr(data,15,16))
        timequery <- (hour*60)+(min)
        return(timequery)
      }#end of getMins

      #making changes
      for (i in 1:numQuery){ #
        instance <- queryData[i,]
        dbtime <- queryData[i,]["timestamp"][1,]
        qtime <- getMins(queryData[i,]["timestamp"][1,])
        queryData[i,]["timeArr"][1,] <- arrTime
        if (as.character(substr((queryData[i,]["timestamp"][1,]),1,10)) == getDateQ()){
          if(as.numeric(substr((queryData[i,]["timestamp"][1,]),12,13)) == getTimeQ()){
          if (queryData[i,]["realIdx"][1,]==whatBusIdx(queryData[i,]["timestamp"][1,])){
            reta <- arrTime - qtime
            queryData[i,]["rETA"][1,] <- reta

            updateQ <- c(paste0('{
               "bus" : "', queryData[i,]["bus"][1,], '",
               "stopId": "', queryData[i,]["stopId"][1,], '",
               "busIdx": "', queryData[i,]["busIdx"][1,], '",
               "timestamp" :  "', queryData[i,]["timestamp"][1,], '"
                                }'
            ))

            setArr <- c(paste0( '{ "$set": { "timeArr": "',as.character(arrTime),'"  } }'  ))
            setETA <- c(paste0( '{ "$set": { "rETA": "',as.character(reta),'"  } }'  ))

            queryList$update(updateQ, setArr, multiple = FALSE)
            queryList$update(updateQ, setETA, multiple = FALSE)

            }
          }
        }
        #start updating mongo


      }#end
      #
      # print(queryData)
      # dataF<- data.frame(cbind(queryData["timestamp"],queryData["rETA"]))

      pastQ <- loadquery()
      # print(pastQ)
      
      if(nrow(pastQ)==0) {}
      else{
        pQ<- data.frame(cbind(pastQ["timestamp"],pastQ["rETA"],pastQ["pETA"]))
        # print(pQ)
        # print("pQ")

        forecast <- pastQ["rETA"]
        movAvg <- as.data.frame(ma(forecast,order=3))
        # print(movAvg)
        df <- data.frame(cbind(pQ,movAvg))
        df <- df[4:nrow(df)-1,]
        df<- cbind(df["timestamp"],sapply(df["rETA"],function(x) as.numeric(x)),df["V1"],sapply(df["pETA"],function(x) as.numeric(x)))
        df[[1]] <- strptime(df[[1]], "%Y-%m-%d %H:%M:%S")

        # View(df)
        # View(df)
        real <- df["rETA"][1:nrow(df),]
        actual <- df["V1"][1:nrow(df),]

        error <- mse(actual,real)
        err <- substr(as.character(error),1,4)
        # #print(typeof(error))
        # output$error <- renderText({
        #   input$submitQ
        #   #realeta = selectdata()
        #   paste("Mean Square Error :" ,as.character(error), "minutes")
        # })



        output$ma <- renderPlot(
          ggplot(data=df,aes(x=timestamp,y=V1,color="black",group="black"))+geom_line(aes(y=rETA),color="black")
          +geom_line(aes(y=V1),color="red")+geom_line(aes(y=pETA),color="blue")+
            theme_economist() + scale_color_economist() +
            scale_x_datetime(breaks = date_breaks("1 week"))+ #Scales the axis
            labs(x = "Time Of Query", y="Actual ETA")+
            theme(panel.background=element_rect(fill="lightblue"))
        )

        output$ETA <- renderValueBox({
          valueBox(
            "ETA: Arrived", "Waiting Time",
            color = "blue"
          )
        })#end of ETA VB



    #end of observerEvent submitV
} }) }
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
    # print("Submitted")
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
  #   #dbAvgVolTrend$remove(query = "{}")
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

  output$avgVolPerRoute <- renderPlot({ #Updated
    #loadAvgVol(filterBusService, filterBusId, date = "")
    busId <- as.numeric(input$busId)
    temp <- seconds_to_period(7*60*60+30*60+15*60*(busId-1))
    startTime <- sprintf('%02d:%02d', temp@hour, minute(temp))

    data <- loadAvgVol(isolate(input$busService), busId) # * use for real data *
    ggplot(data, aes(x=factor(startStop, levels=unique(startStop)), y=avgVol)) +
      geom_boxplot(outlier.shape = NA) +
      geom_smooth(method = "auto", aes(group = 1), se = TRUE, span = 0.5) +
      scale_y_continuous(limits = c(0, 40)) +
      labs(x = "Bus Stops") +
      ggtitle(paste0(isolate(input$busService), " (Start Time: ", startTime, ")")) +
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
    tnow<- as.character(substr(datetime,12,13))
    return(as.numeric(tnow))
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
    query <- queryList$find(query = toString(toJSON(list(bus=input$busService,stopId=isolate(input$startStop),busIdx=(as.character(getBusId()%%7))),auto_unbox = TRUE)))
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

  getBusStopData <- reactive({
    shiny::validate(
      need(hour(Sys.time() + hours(8)) > 7, message="No data available before 8 am")
    )
    date <- substring(Sys.time(), 0, 10)
    query <- paste0('{"', input$busStop, '.', date, '": {"$exists":true}}')
    hourlyData$find(query)
  })
  
  getMinMaxBoarding <- function(busStop) {
    hour <- hour(Sys.time() + hours(8)) - 7
    
    data <- hourlyData$find(paste0('{"', busStop, '": {"$exists":true}','}'))
    ls <- character()
    for (i in 1:nrow(data)) {
      tmp <- data[[1]][[i]]
      ls <- c(ls, tmp[[1]][complete.cases(tmp[[1]])])
    }
  }

  getEndDate <- function(date) {
    if (input$timeFrame == "Monthly") {
      paste0(substr(date,0,8), "31", 'T00:00:00Z')
    } else {
      paste0(date, 'T00:00:00Z')
    }
  }
  
  getMinMaxAlighting <- function(busStop) {
    hour <- hour(Sys.time() + hours(8)) - 7
    
    data <- hourlyData$find(paste0('{"', busStop, '": {"$exists":true}','}'))
    ls <- character()
    for (i in 1:nrow(data)) {
      tmp <- data[[1]][[i]]
      ls <- c(ls, tmp[[2]][complete.cases(tmp[[2]])])
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
  }

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
    hour <- hour(Sys.time() + hours(8)) - 7
    
    busStop <- isolate(input$busStop)
    
    data <- isolate(getBusStopData())
    
    boarding <- data[[1]][[1]][[1]]
    boarding <- substring(boarding, 3, nchar(boarding)-1)
    boarding <- strsplit(boarding,', ')
    boarding <- unlist(boarding)
    boarding <- boarding[1:hour]
    boarding <- sapply(boarding, {function(x) x = as.numeric(x)})
    boarding <- data.frame(boarding, "boarding")
    colnames(boarding) <- c("Count", "Group")
    alighting <- data[[1]][[1]][[2]]
    alighting <- substring(alighting, 3, nchar(alighting)-1)
    alighting <- strsplit(alighting,', ')
    alighting <- unlist(alighting)
    alighting <- alighting[1:hour]
    alighting <- sapply(alighting, {function(x) x = as.numeric(x)})
    alighting <- data.frame(alighting, "alighting")
    colnames(alighting) <- c("Count", "Group")
    
    startDate <- as.POSIXct(paste0(substring(Sys.time(), 0 ,10), ":00:00:00"))
    hours <- seq.POSIXt(from = startDate, to = startDate + days(1), by = "hour")
    hours <- hours[1:hour+7]
    hours <- sapply(hours, {function(x) x = substring(x, 12,16)})
    Hours <- c(hours, hours)
    
    combined <- data.frame(Hours, rbind(boarding, alighting))
    
    boardingMinMax <- getMinMaxBoarding(busStop)
    alightingMinMax <- getMinMaxAlighting(busStop)
    combinedMax <- c(boardingMinMax[[1]], alightingMinMax[[1]])
    combinedMin <- c(boardingMinMax[[2]], alightingMinMax[[2]])
    
    cbPalette <- c("#1AA6B7", "#FE424D")
    
    plot <- ggplot(combined, aes(x=Hours, y=Count, group=Group, color=Group))+
      geom_ribbon(aes(ymax=combinedMax, ymin=combinedMin),  fill = "grey", colour = "grey")+
      geom_line(aes(y=Count), size=1.5)+
      scale_colour_manual(values=cbPalette)
    
    plot+
      labs(title="", y="", x="")+
      theme(panel.background=element_rect(fill="white"), panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(), axis.text.x=element_text(angle = 45, hjust = 1),
            legend.key.size = unit(0.5, "cm"), legend.position = 'top', legend.background = element_rect(colour = 'black'),
            panel.border = element_rect(colour = "black", fill=NA, size=1))+
      guides(colour = guide_legend(nrow = 2))
  })

  output$plot <- renderPlot({
    getPlot()

  })

  getNumBoarding <- eventReactive(input$genResult, {
    hour <- hour(Sys.time() + hours(8)) - 7
    
    data <- isolate(getBusStopData())
    boarding <- data[[1]][[1]][[1]]
    boarding <- substring(boarding, 3, nchar(boarding)-1)
    boarding <- strsplit(boarding,', ')
    boarding <- unlist(boarding)
    boarding <- boarding[1:hour]
    boarding <- sapply(boarding, {function(x) x = as.numeric(x)})
    sum(boarding)
  })

  getNumAlighting <- eventReactive(input$genResult, {
    hour <- hour(Sys.time() + hours(8)) - 7
    
    data <- isolate(getBusStopData())
    alighting <- data[[1]][[1]][[2]]
    alighting <- substring(alighting, 3, nchar(alighting)-1)
    alighting <- strsplit(alighting,', ')
    alighting <- unlist(alighting)
    alighting <- alighting[1:hour]
    alighting <- sapply(alighting, {function(x) x = as.numeric(x)})
    sum(alighting)
  })

  output$boarding <- renderText({
    print(getNumBoarding())
  })

  output$alighting <- renderText({
    print(getNumAlighting())
  })
}
