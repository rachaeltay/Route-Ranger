library(shiny)

stops <- c("KR_terminal", "UHC", "Central_Library", "Kent_RIdge_MRT", "COM2", "Museum", "BIZ2", "Opp_HSSML", "LT13", "Ventus", "PGP_Hse_12", "Computer_Centre", "PGP_Hse_14_15", "PGP_Hse_7", "YIH", "Opp_YIH", "UHall", "Opp_UHall", "PGP", "Raffles_Hall", "EA", "PGPR", "LT29", "S17", "Opp_Kent_Ridge_MRT")
timeIntervals <- c("Monthly", "Weekly", "Daily", "Hourly")

ui <- fluidPage(
  titlePanel(div(h1("The Route Ranger"), style="color:red")),
  sidebarLayout(
    sidebarPanel(selectInput("busStop", "Select Bus Stop", sort(stops), selected="COM2"),
                 selectInput("timeFrame", "Select Time Frame", timeIntervals, selected="Daily"),
                 conditionalPanel("input.timeFrame == 'Hourly'", dateInput("startDate1", "Choose a Day")),
                 conditionalPanel("input.timeFrame != 'Hourly'", dateInput("startDate2", "Starting Date"), dateInput("endDate", "Ending Date")),
                 actionButton("genResult" , "Show Stop Usage!")
                 ),
    mainPanel(div(tags$label("Number of people boarding"), textOutput("boarding"),
              tags$label("Number of people aighting"), textOutput("alighting"), style="color:teal"), br(),
              div(plotOutput("plot"), 
                  # test outputs
                  textOutput("testText"), dataTableOutput("testTable"))
              )
              )
  )