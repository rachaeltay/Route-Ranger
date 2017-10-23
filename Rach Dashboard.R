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
library(forecast)
library(TTR)
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

dbAvgVol <- mongo(collection = "avgVol", db = "routeranger")

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
                                # want to include bus stop name in header
                                h2("Recent Bus Journey at Kent Ridge MRT"),
                                fluidRow(
                                  box(
                                    # title = "Bus Crowdness", background = "blue", solidHeader = TRUE,
                                    plotOutput("volAgainstTime")),
                                  box(
                                    title = "Recent Journey", background = "light-blue",
                                    plotOutput("plot1", height = 300))
                                )
                        )
                        
                      )
                    )
)


server <- function(input, output, session){
  
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
  
  initialiseData <- function(){
      
      
      busStops <- list('KentRidgeMRT', 'PGP',"KR")
      # startStop <- recentJourney()
      
      dfAvgVol <- list()
      
      for (i in 1:length(busStops)){
        
        avgVolData <- data.frame(busCapacity = sample(15:40, size=17, replace=TRUE)) #Added column name, "busCapacity"
        timestamps <- seq(from = as.POSIXct("2010-10-16 07:00:00"),
                          to=as.POSIXct("2010-10-16 23:00:00"),
                          by="hour")
        data <- cbind(avgVolData, timestamps)
        
        # name of bus stop as index, double squared brackets reference dataframe columns
        # dfAvgVol[[busStops[[i]]]] <- data
        
        dfAvgVol[[busStops[[i]]]] <- dbAvgVol$find(query = toString(toJSON(list(busStop = busStops[i]), auto_unbox = TRUE)))
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
      dfAvgVol[[busStops[[i]]]] <- dbAvgVol$find(query = toString(toJSON(list(busStop = busStops[i]), auto_unbox = TRUE)))
    }
    print("dfAvgVol")
    print(busStop)
    
    # retrieve possibly new starting stop
    # startStop <- recentJourney()
  }
  
  # Plot the current hours data
  output$volAgainstTime <- renderPlot({
    print("Render Plot")
    print(dfAvgVol)
    invalidateLater(60000, session) # invalidate every minute
    print("Update")
    updateData()
    ggplot(dfAvgVol$KR, aes(timestamps, busCapacity, colour=busService), ymin = 1, ymax = 40) + 
      # , colour=busCapacity, group = cat
      # scale_colour_viridis(option = "A") +
      geom_line() +
      scale_x_datetime(breaks = date_breaks("1 hours"), date_labels = "%I%p") +  #Scales the axis
      labs(x = "Time", y="Number of people on the bus") +
      theme(panel.background=element_rect(fill="lightblue")) 
  })
  
  timeSeriesAvgVol <- function(){
    
    # eg. see seasonal trend between days
    # eg. cyclical trend in months (near exam period less ppl come to sch or attend classes..?)
    
    # store data in time series object
    # time across a day
    busCapTS <- ts(dfAvgVol$PGP$busCapacity, start=8, end=23)
    plot(busCapTS)
    
    # store date for moving average
    ma.forecast <- ma(busCapTS, order=3)
    ma.TTR <- SMA(busCapTS, 3)
    plot(ma.forecast)
    plot(ma.TTR)
    
    # need to do this across a longer time period with more cyclical pattern
    
    # literally multiple freq lol
    busCapTS <- ts(dfAvgVol$KR$busCapacity, start=8, end=23, frequency = 5)
    # simple exponential - models level (e.g. mean)
    fit <- HoltWinters(busCapTS, beta=FALSE, gamma=FALSE)
    # double exponential - models level and trend
    fit <- HoltWinters(busCapTS, gamma=FALSE)
    # triple exponential - models level, trend, and seasonal components
    fit <- HoltWinters(busCapTS)
    # predictive accuracy
    accuracy(forecast(fit))
  }
}

  
shinyApp(ui, server)
