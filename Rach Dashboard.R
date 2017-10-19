library(shiny)
library(shinythemes)
library(shinydashboard)
library(httr)
library(jsonlite)
library(ggplot2)
library(DT)
library(lubridate)
library(dplyr)
library(smooth)
# %>% pipeline from magrittr
library(magrittr)
library(googleAuthR)
library(shinyjs)
library(mongolite)

#######################  Retrieve Journey/Query by User  #######################

# recentJourney <- function() {
#   journey <- dbQuery$find(query)
#   return startingStop <- journey...
# }

#######################  Dashboard  #######################
ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Route Ranger"),
                    dashboardSidebar(
                      
                      ## Sidebar content
                      dashboardSidebar(
                        sidebarMenu(
                          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                          menuItem("Buses", tabName = "widgets", icon = icon("bus")),
                          menuItem("Bus Stops", tabName = "widgets", icon = icon("map-marker"))
                        )
                      )
                      
                    ),
                    
                    dashboardBody(
                      tabItems(
                        # First tab content
                        tabItem(tabName = "dashboard",
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
                        ),
                        
                        # Second tab content
                        tabItem(tabName = "buses"
                        )
                      )
                    )
)


server <- function(input, output, session){
  # Function to get new observations
  get_data <- function(){
    
    # # initialise dataframe at beginning of day if sys.time 0800 now
    # if (format(Sys.time(), "%X") == "08:00:00"){
    #   
    #   # length is count of list
    #   # generate avgVol dataframes
    #   # for (i in length(busStops)){
    #   #   # generate list of dataframe names (which are avgVol_busStopName_busService)
    #   #   df.names <- paste(paste('avgVol_',busStops[i],sep=""),1:length(busServices),sep="")
    #   #   for (i in length(busServices)){
    #   #     d.frame <- data.frame()
    #   #     assign(paste(df.names[i],'_',busServices[i],sep=""), d.frame)
    #   #   }
    #   # }
    #   
    #   dbAvgVol <- mongo(collection = "avgVol", db = "local")
    #   
    #   # generate list of dataframe names (which are avgVol1, avgVol2 ... )
    #   df.names <- paste('avgVol',1:length(busStops),sep="")
    #   
    #   for (i in length(busStops)){
    #     d.frame <- data.frame(dbAvgVol$find(query = paste('{"startingStop":', busStops[i], ' }', sep="")))
    #     # assign dataframe with bus stop name ( avgVol1_kentridge etc. )
    #     assign(paste(df.names[i],'_',busStops[i],sep=""), d.frame)
    #   }
    #   
    #   # collate list of dataframes
    #   df <- list(ls(pattern='avgVol*'))
    #   
    #   # create dictionary
    #   # foo <- list(a=1,b=2,c=3)
    #   # split variable name by underscore
    #   # split.var <- strsplit(my.var, "_")
    # }
    # 
    # # retrieve starting stop
    # startingStop <- recentJourney()
    
    # need to figure out to rbind only when there's data
    # create 2 rows of data (first is timestamp, second is avgVol)
    avgVol <- data.frame(busCapacity = sample(15:40, size=17, replace=TRUE)) #Added column name, "busCapacity"
    timestamps <- seq(from = as.POSIXct("2010-10-16 07:00:00"), 
                      to=as.POSIXct("2010-10-16 23:00:00"),
                      by="hour")
    data <- cbind(avgVol, timestamps)
  }
  
  # Initialize my_data
  volTime <<- get_data()
  
  # Update every hour update all bus stop dataframes
  update_data <- function(){
    # if its end of the day then stop updating
    # if (format(Sys.time(), "%X") == "08:00:00"){
    # } else {
    # pull from mongodb average data update
    # avgData <- dbAvgVol$find(query)
    
    # create new dataframe of each bus services (filter by timestamp and bus service)
    # data <- dbAvgVol$find(query = toString(toJSON(list(busService = input$busService), auto_unbox = TRUE)))
    
    # replace dataframe with new one containing added data
    volTime <- rbind(volTime,volTime) 
    # }
  }
  
  # Plot the current hours data
  output$volAgainstTime <- renderPlot({
    print("Render")
    print(volTime)
    invalidateLater(1800000, session) # invalidate every 30 minutes
    update_data()
    ggplot(volTime, aes(timestamps, busCapacity), ymin = 1, ymax = 40) + geom_line() +
      scale_x_datetime(breaks = date_breaks("1 hours"), date_labels = "%I%p") +  #Scales the axis
      labs(x = "Time", y="Number of people on the bus")+geom_point() +
      scale_color_brewer(palette = "RdBu") + theme(panel.background=element_rect(fill="lightblue")) 
  })
  
}

shinyApp(ui, server)
