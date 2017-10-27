library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
stops <- c("KR_terminal", "UHC", "Central_Library", "Kent_RIdge_MRT", "COM2", "Museum", "BIZ2", "Opp_HSSML", "LT13", "Ventus", "PGP_Hse_12", "Computer_Centre", "PGP_Hse_14_15", "PGP_Hse_7", "YIH", "Opp_YIH", "UHall", "Opp_UHall", "PGP", "Raffles_Hall", "EA", "PGPR", "LT29", "S17", "Opp_Kent_Ridge_MRT")
timeIntervals <- c("Monthly", "Weekly", "Daily", "Hourly")


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
                                )
                        ),
                        
                        tabItem(tabName = "bus",
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
                                       actionButton("clear", "Clear"),
                                       
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
                        
                        tabItem(tabName = "busstop",
                                box(selectInput("busStop", "Select Bus Stop", sort(stops), selected="COM2"),
                                             selectInput("timeFrame", "Select Time Frame", timeIntervals, selected="Daily"),
                                             conditionalPanel("input.timeFrame == 'Hourly'", dateInput("startDate1", "Choose a Day")),
                                             conditionalPanel("input.timeFrame != 'Hourly'", dateInput("startDate2", "Starting Date"), dateInput("endDate", "Ending Date")),
                                             actionButton("genResult" , "Show Stop Usage!")
                                ),
                                box(div(tags$label("Number of people boarding"), textOutput("boarding"),
                                              tags$label("Number of people aighting"), textOutput("alighting"), style="color:teal"), br(),
                                          div(plotOutput("plot"), 
                                              # test outputs
                                              textOutput("testTyext"), dataTableOutput("teystTable"))
                                )
                        )
                        )
                      )
                    )


# ui <- fluidPage(
#   titlePanel(div(h1("The Route Ranger"), style="color:red")),
#   sidebarLayout(
#     sidebarPanel(selectInput("busStop", "Select Bus Stop", sort(stops), selected="COM2"),
#                  selectInput("timeFrame", "Select Time Frame", timeIntervals, selected="Daily"),
#                  conditionalPanel("input.timeFrame == 'Hourly'", dateInput("startDate1", "Choose a Day")),
#                  conditionalPanel("input.timeFrame != 'Hourly'", dateInput("startDate2", "Starting Date"), dateInput("endDate", "Ending Date")),
#                  actionButton("genResult" , "Show Stop Usage!")
#                  ),
#     mainPanel(div(tags$label("Number of people boarding"), textOutput("boarding"),
#               tags$label("Number of people aighting"), textOutput("alighting"), style="color:teal"), br(),
#               div(plotOutput("plot"), 
#                   # test outputs
#                   textOutput("testTyext"), dataTableOutput("teystTable"))
#               )
#               )
#   )
