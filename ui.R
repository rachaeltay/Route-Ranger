library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)


stops <- c('PGP', 'Kent_Ridge_MRT', 'NUH', 'LT29', 'UHall', 'Opp_UHC', 'YIH', 'Central_Library', 'LT13', 'AS7', 'COM2', 'BIZ2', 'PGP_Hse_12', 'PGP_Hse_7')
timeIntervals <- c("Monthly", "Weekly", "Daily", "Hourly")
allStops <- c("PGP","Kent_Ridge_MRT","NUH","LT29","UHall","Opp_UHC","YIH","Central_Library","LT13","AS7","COM2",
              "BIZ2","PGP_Hse_14_15","PGP_Hse_12","PGP_Hse_7","Opp_HSSML","Opp_NUSS","Ventus","Computer_Centre",
              "Opp_YIH","Museum","UHC","Opp_UHall","S17","Opp_NUH","Opp_Kent_Ridge_MRT","PGPR","CP11","UTown")


ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Route Ranger"),
                    ## Sidebar content
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                        menuItem("Buses", tabName = "bus", icon = icon("bus")),
                        menuItem("Bus Stops", tabName = "busstop", icon = icon("map-marker"))
                      )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        # Dashboard content
                        tabItem(tabName = "dashboard",
                                useShinyjs(),
                                fluidRow(
                                  valueBoxOutput(width = 6,"startStopBox"),
                                  
                                  valueBoxOutput(width = 6,"avgVolBox")
                                ),
                                fluidRow(
                                  box(
                                    width = 12,
                                    plotOutput("forecastCurrent")),
                                  box(
                                    width = 12,
                                    plotOutput("forecastAcrossWeek"))
                                ),
                                actionButton("show", "Login/Logout")
                        ),
                        
                        tabItem(tabName = "bus",
                                fluidRow(
                                  box(selectInput(inputId = "busService", label = "Choose Your Bus", c("A1","A2","D1","D2")),
                                      selectInput(inputId = "startStop", label = "Choose Your Starting Bus Stop", choices = allStops),
                                      selectInput(inputId = "endStop", label = "Choose Your Destination Bus Stop", choices = allStops),
                                      actionButton(inputId = "submitQ", label = "Submit Query"),
                                      #actionButton("clear", "Clear"),
                                      
                                      sliderInput(inputId = 'busCapacity', 
                                                  label = div(style='width:300px;', 
                                                              div(style='float:left;', 'Not Crowded'), 
                                                              div(style='float:right;', 'Very Crowded')), 
                                                  min = 1, max = 3, value = 0, width = '300px'),
                                      actionButton("submitV", "Submit Crowd Level") 
                                      #actionButton("clear", "Clear")
                                      #h2(textOutput("stats"), style = "color: Blue;")
                                  ),
                                  box(textOutput("var"), 
                                      textOutput("timestamp"),
                                      plotOutput("ma")
                                  )
                                ),
                                fluidRow(
                                  # column(2, hr(), actionButton("update", "Update Plot")
                                  # ),
                                  # column(4, hr(), textOutput("click")
                                  # ),
                                  column(6, actionButton("update", "Update Plot"),
                                         textOutput("click")
                                  ),
                                  column(6, selectInput("busId", "Bus ID", choices = 1:62)),
                                  box(plotOutput("graph")   
                                  ),
                                  box(plotOutput("avgVolPerRoute")
                                  )
                                  #dataTableOutput("responses", width = 300)
                                )
                        ),
                        
                        
                        tabItem(tabName = "busstop",
                                box(selectInput("busStop", "Bus Stop", sort(stops), selected="COM2"),
                                   selectInput("timeFrame", "Time Frame", timeIntervals, selected="Daily"),
                                   conditionalPanel("input.timeFrame == 'Hourly'", dateInput("startDate1", "Choose a Day")),
                                   conditionalPanel("input.timeFrame != 'Hourly'", dateInput("startDate2", "Starting Date"), dateInput("endDate", "Ending Date")),
                                   actionButton("genResult" , "Show Stop Usage!"),
                                   status = "primary"
                                ),
                                splitLayout(
                                  box("Boarding:", textOutput("boarding"), status = "primary"),
                                  box("Alighting:", textOutput("alighting"), status = "primary")
                                ),
                                box(plotOutput("plot"), status = "primary"
                                )
                        )
                        )
                      )
                    )