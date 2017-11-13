library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)

stops <- c('PGP', 'Kent_Ridge_MRT', 'NUH', 'LT29', 'UHall', 'Opp_UHC', 'YIH', 'Central_Library', 'LT13', 'AS7', 'COM2', 'BIZ2', 'PGP_Hse_12', 'PGP_Hse_7')
timeIntervals <- c("Monthly", "Weekly", "Daily", "Hourly")
allStops <- c("PGP","Kent_Ridge_MRT","NUH","LT29","UHall","Opp_UHC","YIH","Central_Library","LT13","AS7","COM2",
              "BIZ2","PGP_Hse_14_15","PGP_Hse_12","PGP_Hse_7","Opp_HSSML","Opp_NUSS","Ventus","Computer_Centre",
              "Opp_YIH","Museum","UHC","Opp_UHall","S17","Opp_NUH","Opp_Kent_Ridge_MRT","PGPR","CP11","UTown")


ui <- dashboardPage(skin = "blue",
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
                                  box(
                                    status = 'primary',
                                    width = 7, 
                                    plotOutput("forecastCurrent")),
                                  valueBoxOutput(width = 5,"startStopBox"),
                                  
                                  valueBoxOutput(width = 5,"avgVolBox"),
                                  p(actionButton("show", "Login/Logout"), align = "center", style = "padding-left:10px")
                                ),
                                fluidRow(
                                  box(
                                    status = 'primary',
                                    # textOutput("display_username"),
                                    tags$b("Forecast of the Week"),
                                    width = 12,
                                    plotOutput("forecastAcrossWeek"))
                                )
                        ),
                        
                        tabItem(tabName = "bus",
                                fluidRow(
                                  valueBoxOutput(width = 3,"todayDate"),
                                  valueBoxOutput(width = 3,"ETA"),
                                  valueBoxOutput(width = 6,"error")
                                ),
                                fluidRow(
                                  box(title = "Query Form", status = "warning", solidHeader = FALSE, collapsible = TRUE,
                                      width = 4, 
                                      selectInput(inputId = "busService", label = "Choose Your Bus", c("A1","A2","D1","D2")),
                                      selectInput(inputId = "startStop", label = "Choose Your Starting Bus Stop", choices = allStops),
                                      selectInput(inputId = "endStop", label = "Choose Your Destination Bus Stop", choices = allStops),
                                      actionButton(inputId = "submitQ", label = "Submit Query"),
                                      #actionButton("clear", "Clear"),
                                      
                                      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, 
                                                      .js-irs-0 .irs-bar {background: orange;
                                                      border-top-color: orange; border-bottom-color: orange;
                                                      border-left-color: orange; border-right-color: orange;
                                                      }")),
                                      sliderInput(inputId = 'busCapacity', 
                                                  label = div(style='width:300px;', 
                                                              div(style='float:left;', 'Not Crowded'), 
                                                              div(style='float:right; margin-right: 7px;', 'Very Crowded')), 
                                                  min = 1, max = 3, value = 0, width = '300px'),
                                      actionButton("submitV", "Submit Crowd Level") 
                                      #actionButton("clear", "Clear")
                                      #h2(textOutput("stats"), style = "color: Blue;")
                                      ),
                                  box(title = "rETA", status = "primary", solidHeader = FALSE, collapsible = TRUE,
                                      width = 8,
                                      plotOutput("ma")
                                  )
                                ),
                                fluidRow(
                                  box(title = "Volume of Incoming Bus", status = "primary", solidHeader = FALSE, collapsible = TRUE,
                                      plotOutput("graph"),
                                      actionButton("update", "Update Plot"),
                                      textOutput("click")
                                  ),
                                  box(title = "Spread of Bus Volume per Stop", status = "primary", solidHeader = FALSE, collapsible = TRUE,
                                      selectInput("busId", "Trip ID", choices = 1:62),
                                      plotOutput("avgVolPerRoute")
                                  )
                                  #dataTableOutput("responses", width = 300)
                                )
                        ),
                        
                        
                        tabItem(tabName = "busstop",
                                fluidRow(
                                  box(selectInput("busStop", "Select a bus stop to see its usage", sort(stops)),
                                      actionButton("genResult" , "Show Stop Usage!"),
                                      status = "primary", width=4
                                  ),
                                  infoBoxOutput("boarding"),
                                  infoBoxOutput("alighting"),
                                  box(plotOutput("plot"), status="primary", width=8)
                                )
                        )
                        )
                    )
                    )
