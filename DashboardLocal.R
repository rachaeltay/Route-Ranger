library(shiny)
library(shinythemes)
library(shinydashboard)
library(httr)
library(jsonlite)
library(ggplot2)
library(DT)
# put in AM and PM w date
library(lubridate)
library(dplyr)
library(smooth)
# %>% pipeline from magrittr
library(magrittr)
library(googleAuthR)
library(shinyjs)
library(mongolite)
library(scales)
# colour gradient in the graph
library(viridis)
library(wesanderson)
library(ggthemes)
library(forecast)
library(TTR)
library(reshape)

# for decomp
library(ggseas)

# library(rsconnect)
# rsconnect::deployApp('App-1.Rproj')

#library(curl) # make the jsonlite suggested dependency explicit
#installed httr as dependency
# r <- GET( link )
# raise <- content()
# View()

options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email",
                                        "https://www.googleapis.com/auth/userinfo.profile"))
options("googleAuthR.webapp.client_id" = "682524538636-26vgeiltv82qiapjk63mg79ltrtscovc.apps.googleusercontent.com")
options("googleAuthR.webapp.client_secret" = "SVsY07OxK6yQeaqtEcSIFPsh")

#######################  Get Database and Data  #######################

# create data for avg vol against time
#  - each day create a new set of data for each stop
#  - every minute pull avg volume and place in data frame with timestamp
#  - time is from 0715 to 2300, but we want to plot some data so we start from 0800 instead

#######################  Retrieve Journey/Query by User  #######################

recentJourney <- function() {
  # journey <- dbQuery$find(query)
  # return (startStop <- journey...)
}

avgVolData <- mongo(collection = "avgVol", db = "routeranger")

#######################  Dashboard  #######################
ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Route Ranger"),
                    ## Sidebar content
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                        menuItem("Buses", tabName = "widgets", icon = icon("bus")),
                        menuItem("Bus Stops", tabName = "widgets", icon = icon("map-marker"))
                      )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        # First tab content
                        tabItem(tabName = "dashboard",
                                useShinyjs(),
                                fluidRow(
                                  box(
                                    status = 'primary',
                                    width = 7, 
                                    plotOutput("forecastCurrent")),
                                  valueBoxOutput(width = 5,"startStopBox"),
                                  
                                  valueBoxOutput(width = 5,"avgVolBox"),
                                  p(actionButton("show", "Login/Logout"), align = "center", style = "padding-left:10px")
                                ),
                                fluidRow(
                                  box(status = 'primary',
                                      # textOutput("display_username"),
                                      "Forecast of the Week",
                                      width = 12,
                                      plotOutput("forecastAcrossWeek"))
                                )
                        ),
                        
                        tabItem(tabName = "widgets",
                                h2("Widgets tab content")
                        )
                      )
                    )
)


server <- function(input, output, session){
  
  ####################   Dashboard Boxes   ####################
  output$startStopBox <- renderValueBox({
    valueBox(
      "Kent Ridge MRT", "Recent Stop", icon = icon("bus"),
      color = "light-blue"
    )
  })
  
  output$avgVolBox <- renderValueBox({
    valueBox(
      # get last value of avgVol
      dfAvgVol$PGP$avgVol[-1], "Estimated Bus Capacity", icon = icon("adjust", lib = "glyphicon"),
      color = "teal"
    )
  })
  
  ####################   Google Login   ####################
  
  # for the button
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "Welcome to Route Ranger",
      googleAuthUI("gauth_login"),
      # footer = NULL,
      easyClose = FALSE
    ))
  })
  
  ## Global variables needed throughout the app
  rv <- reactiveValues(
    login = FALSE
  )
  
  showModal(modalDialog(
    title = "Welcome to Route Ranger",
    googleAuthUI("gauth_login"),
    # p(" "),
    # p("
    #   What our app is used for:"),
    # p("Dashboard"),
    #   p("You can view etc"),
    # 
    #   p("Buses 
    #   Find bus services etc
    #   "),
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
  
  busStops <- list('KentRidgeMRT', 'PGP',"KR")
  # busStops <- loadStops()
  # startStop <- recentJourney()
  avgVolData <- mongo(collection = "avgVol", db = "routeranger")
  
  initialiseData <- function(){
    
    dfAvgVol <- list()
    
    for (i in 1:length(busStops)){
      
      avgVolData <- data.frame(avgVol = sample(15:40, size=17, replace=TRUE)) #Added column name, "avgVol"
      timestamps <- seq(from = as.POSIXct("2010-10-16 07:00:00"),
                        to=as.POSIXct("2010-10-16 23:00:00"),
                        by="hour")
      data <- cbind(avgVolData, timestamps)
      
      # name of bus stop as index, double squared brackets reference dataframe columns
      dfAvgVol[[busStops[[i]]]] <- data
      
      # dfAvgVol[[busStops[[i]]]] <- avgVolData$find(paste0('{"startStop": "', busStops[i], '"}'))
    }
    
    return(dfAvgVol)
  }
  
  # Initialize my_data
  print('Initialised')
  dfAvgVol <- initialiseData()
  
  # Update every hour update all bus stop dataframes
  updateData <- function(){
    
    for (i in 1:length(dfAvgVol)){
      # pull from mongodb average data update of each stop
      # replace dataframe with new containing added data
      # dfAvgVol[[busStops[[i]]]] <- avgVolData$find(paste0('{"startStop": "', busStops[i], '"}'))
    }
    
    # retrieve possibly new starting stop
    # startStop <- recentJourney()
  }
  
  # Plot the current hours data along with forecast
  output$forecastCurrent <- renderPlot({
    
    print("Render Plot")
    invalidateLater(1800000, session) # invalidate every minute
    print("Update")
    updateData()
    print(dfAvgVol$PGP)
    
    # time across a day
    
    # Time Series Object
    busCapTS <- ts(dfAvgVol$PGP$avgVol, start=7, end=23)
    # Convert to Dataframe
    busCapTS <- data.frame(as.double(busCapTS))
    timeStampTS <- data.frame(dfAvgVol$PGP$timestamps)
    avgVolTS <- cbind(busCapTS, timeStampTS, 'current')
    colnames(avgVolTS) <- c("avgVol", "timestamps", 'forecast')
    
    busCapForecast <- SMA(ts(dfAvgVol$PGP$avgVol, start=7, end=23),3)
    busCapForecast <- data.frame(busCapForecast)
    # add 2 hours since k=3
    timeStampForecast <- data.frame(dfAvgVol$PGP$timestamps + + 2*60*60)
    avgVolForecast <- cbind(busCapForecast, timeStampForecast, 'forecast')
    colnames(avgVolForecast) <- c("avgVol", "timestamps", 'forecast')
    avgVolForecast <- avgVolForecast[3:nrow(avgVolForecast),]
    
    busCapCombi <- rbind(avgVolTS, avgVolForecast)
    
    ggplot(busCapCombi, aes(timestamps, avgVol, colour=forecast), ymin = 1, ymax = 40) + 
      geom_line(size=0.8) +
      theme_economist() +
      scale_color_economist(labels = c("Current", "Forecast")) +
      scale_x_datetime(breaks = date_breaks("2 hours"), date_labels = "%I%p") +  #Scales the axis
      labs(x = "Time", y="Estimated number of people on the bus")
    # theme(panel.background=element_rect(fill="lightblue"))
  })
  
  
  output$forecastAcrossWeek <- renderPlot({
    # holt winter across a week
    
    # store date for moving average
    # ma.forecast <- ma(busCapTS, order=3)
    # ma.TTR <- SMA(busCapTS, 3)
    # plot(ma.forecast)
    # ggplot(ma.forecast, aes(time,ma.forecast))
    # ggplot(ma.TTR)
    # predictive accuracy
    print(accuracy(forecast(fit)))
    
    #plot using Holt Winter for prediction
    # literally multiple freq to create a week  (Time 1 to 7, Monday to Sunday)
    avgVolTS <- ts(dfAvgVol$PGP$avgVol, start=1, end=5, frequency = 25)
    # plot(avgVolTS)
    decomp <- decompose(avgVolTS)
    # plot(decomp)
    # triple exponential - models level, trend, and seasonal components
    hw <- HoltWinters(avgVolTS)
    # create forecast using predict
    forecast<-predict(hw,  n.ahead=10,  prediction.interval=T,  level=0.95)
    
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
      geom_line(aes(colour=variable), size=0.8) +
      # made changes
      # scale_color_manual(labels = c("Current", "Forecast"), values = c("#000000","#0072B2")) +
      scale_x_continuous(breaks = c(1,2,3,4,5), labels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')) +
      # geom_vline(x=max(actualData$time),  lty=2) +
      xlab('Day of the Week') + ylab('Estimated number of people on the bus') + 
      # theme_economist(dkpanel=TRUE,stata=TRUE) +
      theme_economist_white(base_size=10,gray_bg=FALSE) +
      scale_colour_economist(labels = c("Current", "Forecast"))
    
  })
}